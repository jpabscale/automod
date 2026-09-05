import com.fasterxml.jackson.databind.JsonNode
import com.github.jpabscale.dnlib4j.api.IlService
import com.github.jpabscale.dnlib4j.api.OperandSpec
import com.github.jpabscale.dnlib4j.dotnet.MethodDef
import com.github.jpabscale.dnlib4j.dotnet.ModuleDefMD
import com.github.jpabscale.dnlib4j.dotnet.UTF8String
import com.github.jpabscale.dnlib4j.dotnet.emit.CilBody
import com.github.jpabscale.dnlib4j.dotnet.emit.Code
import com.github.jpabscale.dnlib4j.dotnet.emit.Instruction
import com.github.jpabscale.dnlib4j.dotnet.emit.OpCode

import os._
import scala.jdk.CollectionConverters._

/**
 * IL patch rule engine (Tiers 1-3), ported from the Kotlin automod's il.kt so the Scala
 * automod is on par. Parses an IlPatch TOML root and applies every rule via dnlib4j IlService.
 * See il.kt for the full contract (byte-level const swaps, CIL body rewrites, [[inject.*]]).
 */
object IlEngine {

  // Kotlin inline classes (`RVA`, `UInt`) mangle several IlService/CilBody/MethodDef member names at
  // the JVM boundary (e.g. `applyBody` -> `applyBody-xesCC1k`); Scala cannot call those names even with
  // backticks, so we reach them via reflection on the exact (mangled) JVM signatures.
  private object Dnlib {
    private val svc = IlService.INSTANCE
    private val applyBodyM = svc.getClass.getMethod("applyBody-xesCC1k", classOf[Array[Byte]], classOf[ModuleDefMD], java.lang.Long.TYPE, classOf[CilBody])
    private val rvaM = classOf[MethodDef].getMethod("getRVA-wDu2Vro")
    private val updOffM = classOf[CilBody].getMethod("UpdateInstructionOffsets-pVg5ArA")

    def applyBodyBytes(bytes: Array[Byte], module: ModuleDefMD, method: MethodDef, body: CilBody): Array[Byte] =
      applyBodyM.invoke(svc, bytes, module, java.lang.Long.valueOf(rvaOf(method)), body).asInstanceOf[Array[Byte]]

    def rvaOf(method: MethodDef): Long =
      rvaM.invoke(method).asInstanceOf[java.lang.Long].longValue()

    def updateOffsets(body: CilBody): Unit = { updOffM.invoke(body); () }
  }

  def applyIlPatch(root: JsonNode, input: Array[Byte]): Array[Byte] = {
    val mod = IlService.INSTANCE.loadModule(input)
    var out = input.clone().asInstanceOf[Array[Byte]]

    for (rule <- arr(root.get("float"))) out = applyConst(out, mod, rule, "float")
    for (rule <- arr(root.get("int"))) out = applyConst(out, mod, rule, "int")
    for (rule <- arr(root.get("long"))) out = applyConst(out, mod, rule, "long")

    val mod2 = IlService.INSTANCE.loadModule(out)

    val edits = collection.mutable.LinkedHashMap[MethodDef, collection.mutable.ListBuffer[CilBody => Unit]]()
    def addEdit(m: MethodDef, t: CilBody => Unit): Unit = {
      val buf = edits.getOrElseUpdate(m, collection.mutable.ListBuffer.empty)
      buf += t
    }

    for (rule <- arr(root.get("string"))) applyString(rule, mod2, addEdit)
    for (rule <- arr(root.get("il"))) handleIl(rule, mod2, addEdit)
    for (rule <- arr(root.get("method"))) handleMethod(rule, mod2, addEdit)
    val inject = root.get("inject")
    if (inject != null && !inject.isMissingNode && !inject.isNull) {
      for (rule <- arr(inject.get("call"))) handleInject(rule, mod2, InjectPoint.CALL, addEdit)
      for (rule <- arr(inject.get("field_read"))) handleInject(rule, mod2, InjectPoint.FIELD_READ, addEdit)
      for (rule <- arr(inject.get("field_write"))) handleInject(rule, mod2, InjectPoint.FIELD_WRITE, addEdit)
    }

    for ((method, transforms) <- edits) {
      val body = method.getBody
      transforms.foreach(t => t(body))
      body.UpdateMaxStack()
      out = Dnlib.applyBodyBytes(out, mod2, method, body)
    }
    out
  }

  def parseToml(path: os.Path): JsonNode =
    new com.fasterxml.jackson.dataformat.toml.TomlMapper().readTree(path.toIO)

  private def applyConst(out: Array[Byte], mod: ModuleDefMD, rule: JsonNode, kind: String): Array[Byte] = {
    val typeSel = opt(rule.get("type"))
    val methodName = opt(rule.get("method"))
    val occ = parseOccurrences(rule.get("occurrence"))
    val oldV = constOld(rule, kind)
    val newV = constNew(rule, kind)
    val sites = collection.mutable.ListBuffer[(MethodDef, Int)]()
    for (m <- IlService.INSTANCE.findMethods(mod, typeSel, methodName).asScala) {
      val body = m.getBody
      for ((ins, idx) <- body.getInstructions.asScala.zipWithIndex) {
        if (matchesConst(ins, kind, oldV)) sites += ((m, idx))
      }
    }
    val selected = if (occ == null) sites.toList else sites.zipWithIndex.filter { case (_, i) => occ.contains(i + 1) }.map(_._1)
    if (selected.isEmpty) {
      println(s"  ilpatch: $kind ${if (typeSel == null) "" else typeSel}::${if (methodName == null) "" else methodName} : no matching site for $oldV (skipped)")
      return out
    }
    val buf = out.clone().asInstanceOf[Array[Byte]]
    for ((m, idx) <- selected) {
      kind match {
        case "float" =>
          val fo = IlService.INSTANCE.operandFileOffset(buf, m, idx)
          IlService.INSTANCE.writeIntLE(buf, fo, java.lang.Float.floatToIntBits(newV.asInstanceOf[Float]))
        case "long" =>
          val fo = IlService.INSTANCE.operandFileOffset(buf, m, idx)
          IlService.INSTANCE.writeLongLE(buf, fo, newV.asInstanceOf[Long])
        case "int" => patchIntSite(buf, m, idx, newV.asInstanceOf[Int])
      }
    }
    println(s"  ilpatch: $kind ${if (typeSel == null) "" else typeSel}::${if (methodName == null) "" else methodName} : ${selected.size} site(s) $oldV -> $newV")
    buf
  }

  private def constOld(rule: JsonNode, kind: String): Any = kind match {
    case "float" => rule.get("old").floatValue()
    case "int" => rule.get("old").intValue()
    case "long" => rule.get("old").longValue()
    case _ => throw new IllegalArgumentException(s"unknown const kind $kind")
  }

  private def constNew(rule: JsonNode, kind: String): Any = kind match {
    case "float" => rule.get("new").floatValue()
    case "int" => rule.get("new").intValue()
    case "long" => rule.get("new").longValue()
    case _ => throw new IllegalArgumentException(s"unknown const kind $kind")
  }

  private def matchesConst(ins: Instruction, kind: String, oldV: Any): Boolean = kind match {
    case "float" => ins.getOpCode.getName == "ldc.r4" && ins.getOperand == oldV
    case "long" => ins.getOpCode.getName == "ldc.i8" && ins.getOperand == oldV
    case "int" => intValue(ins) == oldV
    case _ => false
  }

  private def intValue(ins: Instruction): Integer = ins.getOpCode.getCode match {
    case Code.ldc_i4_m1 => -1
    case Code.ldc_i4_0 => 0
    case Code.ldc_i4_1 => 1
    case Code.ldc_i4_2 => 2
    case Code.ldc_i4_3 => 3
    case Code.ldc_i4_4 => 4
    case Code.ldc_i4_5 => 5
    case Code.ldc_i4_6 => 6
    case Code.ldc_i4_7 => 7
    case Code.ldc_i4_8 => 8
    case Code.ldc_i4_s => ins.getOperand.asInstanceOf[Byte].toInt
    case Code.ldc_i4 => ins.getOperand.asInstanceOf[Int]
    case _ => null
  }

  private def patchIntSite(buf: Array[Byte], m: MethodDef, idx: Int, newV: Int): Unit = {
    val ins = m.getBody.getInstructions.get(idx)
    val fo = IlService.INSTANCE.opcodeFileOffset(buf, m, idx)
    ins.getOpCode.getCode match {
      case Code.ldc_i4_m1 | Code.ldc_i4_0 | Code.ldc_i4_1 | Code.ldc_i4_2 | Code.ldc_i4_3 | Code.ldc_i4_4 | Code.ldc_i4_5 | Code.ldc_i4_6 | Code.ldc_i4_7 | Code.ldc_i4_8 =>
        require(newV >= -1 && newV <= 8, s"same-size int patch for ldc.i4.0..8 requires newValue -1..8, got $newV")
        buf(fo) = smallOpcodeFor(newV).toByte
      case Code.ldc_i4_s =>
        require(newV >= -128 && newV <= 127, s"ldc.i4.s same-size patch requires -128..127, got $newV")
        require((buf(fo) & 0xFF) == 0x1F, s"opcode mismatch at $fo")
        buf(fo + 1) = newV.toByte
      case Code.ldc_i4 =>
        require((buf(fo) & 0xFF) == 0x20, s"opcode mismatch at $fo")
        IlService.INSTANCE.writeIntLE(buf, fo + 1, newV)
      case _ => throw new IllegalArgumentException(s"unsupported int site code ${ins.getOpCode.getCode}")
    }
  }

  private def smallOpcodeFor(v: Int): Int = v match {
    case -1 => 0x15
    case 0 => 0x16
    case 1 => 0x17
    case 2 => 0x18
    case 3 => 0x19
    case 4 => 0x1A
    case 5 => 0x1B
    case 6 => 0x1C
    case 7 => 0x1D
    case 8 => 0x1E
    case _ => throw new IllegalArgumentException(s"no small opcode for $v")
  }

  private def applyString(rule: JsonNode, mod: ModuleDefMD, addEdit: (MethodDef, CilBody => Unit) => Unit): Unit = {
    val typeSel = opt(rule.get("type"))
    val methodName = opt(rule.get("method"))
    val oldS = rule.get("old").asText()
    val newS = rule.get("new").asText()
    for (m <- IlService.INSTANCE.findMethods(mod, typeSel, methodName).asScala) {
      val body = m.getBody
      val idxs = body.getInstructions.asScala.zipWithIndex.filter { case (ins, _) => ins.getOpCode.getName == "ldstr" && ins.getOperand == oldS }.map(_._2)
      for (i <- idxs) addEdit(m, c => {
        val instrs = c.getInstructions
        instrs.set(i, IlService.INSTANCE.buildInstruction(IlService.INSTANCE.opcode("ldstr"), new OperandSpec.StrVal(newS), mod, m))
      })
    }
    println(s"  ilpatch: string ${if (typeSel == null) "" else typeSel}::${if (methodName == null) "" else methodName} : $oldS -> $newS")
  }

  private def handleIl(rule: JsonNode, mod: ModuleDefMD, addEdit: (MethodDef, CilBody => Unit) => Unit): Unit = {
    val findSpecs = arr(rule.get("find")).map(_.asText()).toList
    val replaceSpecs = arr(rule.get("replace")).map(_.asText()).toList
    require(findSpecs.nonEmpty, "[[il]] requires a non-empty 'find' array")
    val typeSel = opt(rule.get("type"))
    val methodName = opt(rule.get("method"))
    val occ = parseOccurrences(if (rule.has("label")) rule.get("label") else rule.get("occurrence"))
    val methods = IlService.INSTANCE.findMethods(mod, typeSel, methodName)
    for (m <- methods.asScala) {
      val body = m.getBody
      val findT = findSpecs.map(parseInstrSpec(_, mod, m))
      val replaceT = replaceSpecs.map(parseInstrSpec(_, mod, m))
      val positions = findSequence(body.getInstructions.asScala.toList, findT)
      val selected = if (occ == null) positions.take(1) else positions.zipWithIndex.filter { case (_, i) => occ.contains(i + 1) }.map(_._1)
      if (selected.isEmpty) {
        println(s"  ilpatch: [[il]] ${if (typeSel == null) "" else typeSel}::${if (methodName == null) "" else methodName} : sequence not found (skipped)")
      } else {
        for (start <- selected) {
          val end = start + findT.size
          if (IlService.INSTANCE.ehOverlaps(body, start, end))
            throw new IllegalStateException(s"[[il]] ${if (typeSel == null) "" else typeSel}::${if (methodName == null) "" else methodName} : edit window [$start, $end) overlaps an exception handler region; refusing rule to avoid corrupting EH")
          addEdit(m, c => IlService.INSTANCE.spliceBody(c, start, findT.size, replaceT.asJava))
        }
        println(s"  ilpatch: [[il]] ${if (typeSel == null) "" else typeSel}::${if (methodName == null) "" else methodName} : ${selected.size} occurrence(s) replaced")
      }
    }
  }

  private def handleMethod(rule: JsonNode, mod: ModuleDefMD, addEdit: (MethodDef, CilBody => Unit) => Unit): Unit = {
    val kind = Option(opt(rule.get("kind"))).getOrElse {
      rule.fieldNames().asScala.find(n => n != "type" && n != "method").getOrElse("body")
    }
    val replaceSpecs = arr(rule.get("replace")).map(_.asText()).toList
    val typeSel = opt(rule.get("type"))
    val methodName = opt(rule.get("method"))
    val methods = IlService.INSTANCE.findMethods(mod, typeSel, methodName)
    for (m <- methods.asScala) {
      val body = m.getBody
      val replaceT = replaceSpecs.map(parseInstrSpec(_, mod, m))
      kind match {
        case "body" =>
          if (IlService.INSTANCE.ehOverlaps(body, 0, body.getInstructions.size()))
            throw new IllegalStateException(s"[[method.body]] ${if (typeSel == null) "" else typeSel}::${if (methodName == null) "" else methodName} : method has exception handlers; refusing rule (would drop EH)")
          addEdit(m, c => {
            val instrs = c.getInstructions
            instrs.clear()
            instrs.addAll(replaceT.asJava)
            Dnlib.updateOffsets(c)
          })
        case "entry" => addEdit(m, c => {
          val instrs = c.getInstructions
          instrs.addAll(0, replaceT.asJava)
          Dnlib.updateOffsets(c)
        })
        case "exit" => addEdit(m, c => {
          val instrs = c.getInstructions
          val rets = instrs.asScala.zipWithIndex.filter(_._1.getOpCode.getName == "ret").map(_._2).toList
          var shift = 0
          for (i <- rets) {
            instrs.addAll(i + shift, replaceT.asJava)
            shift += replaceT.size
          }
          Dnlib.updateOffsets(c)
        })
        case _ => throw new IllegalArgumentException(s"unknown [[method]] kind '$kind'")
      }
      println(s"  ilpatch: [[method.$kind]] ${if (typeSel == null) "" else typeSel}::${if (methodName == null) "" else methodName}")
    }
  }

  private sealed trait InjectPoint
  private object InjectPoint { case object CALL extends InjectPoint; case object FIELD_READ extends InjectPoint; case object FIELD_WRITE extends InjectPoint }

  private def handleInject(rule: JsonNode, mod: ModuleDefMD, point: InjectPoint, addEdit: (MethodDef, CilBody => Unit) => Unit): Unit = {
    val target = rule.get("target").asText()
    val whenn = (if (rule.has("when")) rule.get("when").asText() else if (rule.has("mode")) rule.get("mode").asText() else "AFTER").toUpperCase
    val scope = opt(rule.get("scope"))
    val codeSpecs = arr(rule.get("code")).map(_.asText()).toList
    require(codeSpecs.nonEmpty, "[[inject]] requires a non-empty 'code' array")
    val (typeSel, methSel) = if (scope != null && scope.contains("::")) {
      val p = scope.split("::", 2)
      (p(0), p(1))
    } else (scope, null)
    val scanned = IlService.INSTANCE.findMethods(mod, typeSel, methSel)
    val resolved = try {
      point match {
        case InjectPoint.CALL => IlService.INSTANCE.resolveMethod(mod, target)
        case _ => IlService.INSTANCE.resolveField(mod, target)
      }
    } catch {
      case e: Exception =>
        println(s"  ilpatch: [[inject]] target '$target' not resolved (skipped): ${e.getMessage}")
        return
    }
    val relevantOpcodes = point match {
      case InjectPoint.CALL => Set("call", "callvirt")
      case InjectPoint.FIELD_READ => Set("ldfld", "ldsfld")
      case InjectPoint.FIELD_WRITE => Set("stfld", "stsfld")
    }
    var count = 0
    for (m <- scanned.asScala) {
      val body = m.getBody
      val matched = body.getInstructions.asScala.zipWithIndex.filter { case (ins, _) => relevantOpcodes.contains(ins.getOpCode.getName) && ins.getOperand == resolved }.map(_._2).toSet
      if (whenn == "REPLACE") {
        for (idx <- matched) {
          if (IlService.INSTANCE.ehOverlaps(body, idx, idx + 1))
            throw new IllegalStateException(s"[[inject.${point.toString.toLowerCase}]] $target on ${UTF8String.ToSystemString(m.getName)} : REPLACE at instruction $idx overlaps an exception handler region; refusing rule")
        }
      }
      val codeInstrs = codeSpecs.map(parseInstrSpec(_, mod, m))
      addEdit(m, c => {
        val before = c.getInstructions.size()
        injectIntoBody(c, matched, codeInstrs, whenn)
        count += c.getInstructions.size() - before
      })
    }
    println(s"  ilpatch: [[inject.${point.toString.toLowerCase}]] target=$target when=$whenn scope=$scope : ${scanned.size} method(s), $count instr(s) injected")
  }

  private def injectIntoBody(body: CilBody, matchedIndices: Set[Int], codeInstrs: List[Instruction], whenn: String): Unit = {
    val instrs = body.getInstructions
    val newList = new java.util.ArrayList[Instruction]()
    for (i <- 0 until instrs.size()) {
      val ins = instrs.get(i)
      if (matchedIndices.contains(i)) {
        whenn match {
          case "BEFORE" => newList.addAll(codeInstrs.asJava); newList.add(ins)
          case "AFTER" => newList.add(ins); newList.addAll(codeInstrs.asJava)
          case "AROUND" => newList.addAll(codeInstrs.asJava); newList.add(ins); newList.addAll(codeInstrs.asJava)
          case "REPLACE" => newList.addAll(codeInstrs.asJava)
          case _ => newList.add(ins)
        }
      } else newList.add(ins)
    }
    instrs.clear()
    instrs.addAll(newList)
    Dnlib.updateOffsets(body)
  }

  private def parseInstrSpec(spec: String, mod: ModuleDefMD, m: MethodDef): Instruction = {
    val sp = spec.trim.split("\\s+", 2)
    val op = IlService.INSTANCE.opcode(sp(0))
    if (sp.length == 1) Instruction.Companion.Create(op)
    else buildOperand(op, sp(1).trim, mod, m)
  }

  private def buildOperand(op: OpCode, operand: String, mod: ModuleDefMD, m: MethodDef): Instruction = {
    if (operand.startsWith("\""))
      IlService.INSTANCE.buildInstruction(op, new OperandSpec.StrVal(operand.stripPrefix("\"").stripSuffix("\"").replace("\\\"", "\"")), mod, m)
    else if (operand.startsWith("call ") || operand.startsWith("newobj ") || (operand.contains("::") && !operand.startsWith("ldfld") && !operand.startsWith("stfld") && !operand.startsWith("ldsfld") && !operand.startsWith("stsfld")))
      IlService.INSTANCE.buildInstruction(op, new OperandSpec.MethodRef(operand.trim), mod, m)
    else if (operand.startsWith("ldfld ") || operand.startsWith("stfld ") || operand.startsWith("ldsfld ") || operand.startsWith("stsfld "))
      IlService.INSTANCE.buildInstruction(op, new OperandSpec.FieldRef(operand.trim), mod, m)
    else if (operand.toIntOption.isDefined)
      IlService.INSTANCE.buildInstruction(op, new OperandSpec.IntVal(operand.toInt), mod, m)
    else if (operand.toFloatOption.isDefined)
      IlService.INSTANCE.buildInstruction(op, new OperandSpec.FloatVal(operand.toFloat), mod, m)
    else throw new IllegalArgumentException(s"unsupported operand '$operand' in IL spec")
  }

  private def findSequence(instrs: List[Instruction], templates: List[Instruction]): List[Int] = {
    if (templates.isEmpty || instrs.size < templates.size) return List.empty
    val res = collection.mutable.ListBuffer[Int]()
    for (i <- 0 to instrs.size - templates.size) {
      if ((0 until templates.size).forall(j => matchInstr(instrs(i + j), templates(j)))) res += i
    }
    res.toList
  }

  private def matchInstr(ins: Instruction, t: Instruction): Boolean = {
    if (ins.getOpCode.getName != t.getOpCode.getName) return false
    val to = t.getOperand
    if (to == null) return true
    ins.getOperand == to
  }

  private def parseOccurrences(node: JsonNode): Set[Int] = {
    if (node == null || node.isMissingNode || node.isNull) null
    else if (node.isArray) node.elements().asScala.map(_.asInt()).toSet
    else if (node.isTextual) node.asText().split(",").map(_.trim.toInt).toSet
    else Set(node.asInt())
  }

  private def arr(node: JsonNode): Seq[JsonNode] =
    if (node == null || node.isMissingNode || node.isNull) Seq.empty
    else node.elements().asScala.toSeq

  private def opt(node: JsonNode): String =
    if (node == null || node.isMissingNode || node.isNull) null else node.asText()
}
