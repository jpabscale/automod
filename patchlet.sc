import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.fasterxml.jackson.databind.node.{ArrayNode, BooleanNode, DoubleNode, IntNode, JsonNodeFactory, NullNode, ObjectNode, TextNode}
import com.jayway.jsonpath
import java.util.{List => JList}
import org.luaj.vm2.{LuaValue, LuaFunction}
import org.graalvm.polyglot.{Context, HostAccess, Value}
import scala.collection.immutable.TreeMap
import scala.jdk.CollectionConverters._
import scala.collection.parallel.CollectionConverters._
import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox


object Constants {
  val codePrefixScala: String = "=>"
  val codePrefixTypescript: String = "=ts>"
  val codePrefixJavascript: String = "=js>"
  val codePrefixPython: String = "=py>"
  val codePrefixLua: String = "=lua>"
  val codePrefixKotlin: String = "=kt>"
  val atPrefix = ".@:"
  val javaRegexPrefix = ".*:"
  val addValueKey = "value"
  private val tbLocal = ThreadLocal.withInitial(() => runtimeMirror(getClass.getClassLoader).mkToolBox())
  def tb = tbLocal.get // scala.tools.reflect.ToolBox is not thread-safe; give each thread its own

  val dataTableJsonPath = toJsonPath(automod.dataTablePath) // "$['Exports'][0]['Table']['Data']"
  def toJsonPath(jsonPtr: String): String = ("$" +: jsonPtr.split('/').drop(1).map(s => s.toIntOption match {
    case Some(n) => s"[$n]"
    case _ => s"['$s']"
  })).mkString

  val luaJson = {
    val parse = new LuaFunction {
      override def call(arg: LuaValue): LuaValue = uassetapi.toLuaValue(new ObjectMapper().readTree(arg.tojstring))
    }
    val stringify = new LuaFunction {
      override def call(arg: LuaValue): LuaValue = LuaValue.valueOf(uassetapi.fromLuaValue(arg).toString)
    }
    LuaValue.tableOf(Array(LuaValue.valueOf("parse"), parse, LuaValue.valueOf("stringify"), stringify))
  }
}

import Constants._

sealed trait Lang
object Lang {
  case object Scala extends Lang
  case object Typescript extends Lang
  case object Js extends Lang
  case object Python extends Lang
  case object Lua extends Lang
  case object Kotlin extends Lang
}

// Shared struct surface for patch code expressions: implemented by both
// uassetapi.Struct (UE {Name, Value} rows) and unityapi.UnityStruct (asset4j flat
// Data field trees), so evalStructProperty can operate on either engine.
trait StructLike {
  def uassetName: String
  def value: JsonNode
  def name: String
  def getJson(name: String): JsonNode
  def setJson(property: String, value: JsonNode): Option[JsonNode]
  def set(name: String, value: Boolean): Boolean
  def set(name: String, value: Int): Int
  def set(name: String, value: Double): Double
  def set(name: String, value: String): String
  def update(name: String, value: JsonNode): Option[JsonNode]
  def getBoolean(name: String): Boolean
  def getInt(name: String): Int
  def getDouble(name: String): Double
  def getString(name: String): String
  def apply[T: TypeTag](name: String): T
}

def graalContext(id: String): Context =
  Context.newBuilder(id).
    allowHostAccess(HostAccess.newBuilder(HostAccess.ALL).build).
    allowHostClassLookup(_ => true).
    build

def evalPoly(context: Context, lang: String, code: String): Value = try context.eval(lang, code) catch {
  case t: Throwable =>
    automod.exit(-1, 
      s"""Failed to evaluate $lang: ${t.getMessage}
         |$code""".stripMargin)
}
def evalScala[T](exp: String): T = tb.eval(tb.parse(exp)).asInstanceOf[T]
def evalJs(v: Context => Value, code: String): (Context, Value) = {
  val context = graalContext("js")
  context.getBindings("js").putMember("v", v(context))
  (context, evalPoly(context, "js", code))
}

def evalJsRaw(v: RawScriptContext, code: String): (Context, Value) = {
  val context = graalContext("js")
  val bindings = context.getBindings("js")
  val orig = v.getOrElse("orig", Array.emptyByteArray).asInstanceOf[Array[Byte]]
  val current = v.getOrElse("current", Array.emptyByteArray).asInstanceOf[Array[Byte]]
  bindings.putMember("__orig", context.asValue(orig))
  bindings.putMember("__current", context.asValue(current))
  val extra = new java.util.HashMap[String, Object]
  for ((k, value) <- v if k != "orig" && k != "current") extra.put(k, value.toString)
  bindings.putMember("__map", context.asValue(extra))
  // asset-JSON helpers: lazy decode of `current`, encode back to bytes, and patch-local
  // resources — file/ttmap/external plumbing lives in automod's context, not the patch.
  bindings.putMember("__toJson", context.asValue(new java.util.function.Supplier[String] {
    def get: String = currentJson(v).toString
  }))
  bindings.putMember("__encode", context.asValue(new java.util.function.Function[String, Array[Byte]] {
    def apply(s: String): Array[Byte] = jsonBytesFromString(v, s)
  }))
  bindings.putMember("__resource", context.asValue(new java.util.function.Function[String, Array[Byte]] {
    def apply(name: String): Array[Byte] = patchResourceBytes(v, name)
  }))
  context.eval("js", "var v = { orig: new Uint8Array(__orig), current: new Uint8Array(__current), map: __map, toJson: function() { return JSON.parse(__toJson()); }, fromJson: function(o) { return new Uint8Array(__encode(JSON.stringify(o))); }, resource: function(name) { var b = __resource(name); return b == null ? null : new Uint8Array(b); } }")
  (context, evalPoly(context, "js", code))
}

lazy val tsCache = new java.util.concurrent.ConcurrentHashMap[String, String]

def compileTypescript(code: String): String = {
  Option(tsCache.get(code)) match {
    case Some(c) => c
    case _ =>
      val d = os.temp.dir()
      val input = d / "f.ts"
      val output = d / "f.js"
      os.write.over(input, code)
      if (automod.osKind.isWin) os.proc("cmd", "/d", "/c", "tsc", "--outFile", output, input).call()
      else os.proc("tsc", "--outFile", output, input).call()
      val c = os.read(output)
      tsCache.put(code, c)
      c
  }
}

def evalTypescript(preamble: String)(v: Context => Value, exp: String): (Context, Value) = {
  val code = s"$preamble${util.Properties.lineSeparator}$exp"
  evalJs(v, compileTypescript(code))
}

def evalTypescriptRaw(v: RawScriptContext, exp: String): (Context, Value) = {
  val preamble = """declare var v: { orig: Uint8Array; current: Uint8Array; map: { [key: string]: string }; toJson: () => any; fromJson: (o: any) => Uint8Array; resource: (name: string) => Uint8Array | null }"""
  val code = s"$preamble${util.Properties.lineSeparator}$exp"
  evalJsRaw(v, compileTypescript(code))
}

def evalPythonRaw(v: RawScriptContext, code: String): (Context, Value) = {
  val context = graalContext("python")
  val bindings = context.getBindings("python")
  val orig = v.getOrElse("orig", Array.emptyByteArray).asInstanceOf[Array[Byte]]
  val current = v.getOrElse("current", Array.emptyByteArray).asInstanceOf[Array[Byte]]
  bindings.putMember("__orig", context.asValue(orig.map(_ & 0xFF)))
  bindings.putMember("__current", context.asValue(current.map(_ & 0xFF)))
  bindings.putMember("__ttmap", if (v.contains("ttmap")) context.asValue(v("ttmap").toString) else context.asValue(null))
  bindings.putMember("__map", context.asValue(v.filter(kv => kv._1 != "orig" && kv._1 != "current").map(kv => (kv._1, kv._2.toString)).toMap.asJava))
  bindings.putMember("__toJson", context.asValue(new java.util.function.Supplier[String] {
    def get: String = currentJson(v).toString
  }))
  bindings.putMember("__encode", context.asValue(new java.util.function.Function[String, Array[Byte]] {
    def apply(s: String): Array[Byte] = jsonBytesFromString(v, s)
  }))
  bindings.putMember("__resource", context.asValue(new java.util.function.Function[String, Array[Byte]] {
    def apply(name: String): Array[Byte] = patchResourceBytes(v, name)
  }))
  context.eval("python", "import json\nv = { 'orig': bytearray(__orig), 'current': bytearray(__current), 'ttmap': __ttmap, 'map': __map, 'toJson': lambda: json.loads(__toJson()), 'fromJson': lambda o: bytearray(__encode(json.dumps(o))), 'resource': lambda n: bytearray(__resource(n)) if __resource(n) is not None else None }")
  (context, evalPoly(context, "python", code.trim))
}

def evalPython(v: Context => Value, code: String): (Context, Value) = {
  val context = graalContext("python")
  context.getBindings("python").putMember("v", v(context))
  (context, evalPoly(context, "python", code.trim))
}

def evalLua(v: LuaValue, code: String, err: String => String): JsonNode = {
  try {
    val g = org.luaj.vm2.lib.jse.JsePlatform.standardGlobals
    org.luaj.vm2.luajc.LuaJC.install(g)
    g.set(LuaValue.valueOf("JSON"), luaJson)
    val chunk = g.load(
      s"""function __f(v)
         |  $code
         |end
         |
         |_f = __f""".stripMargin)
    chunk.call()
    assert(!chunk.isclosure)
    val f = g.get("_f").asInstanceOf[LuaValue]
    uassetapi.fromLuaValue(f.call(v))
  } catch {
    case t: Throwable => automod.exit(-1, err(t.getMessage))
  }
}

type RawScriptContext = scala.collection.immutable.Map[String, Any]

// -- Context helpers: the asset-JSON round-trip and patch-local resource reads, with the
//    game-dir/external/ttmap plumbing consumed here (automod supplies it in the context)
//    so patches never see file locations.
private val jsonMapper = new ObjectMapper()

def currentBytesOf(v: RawScriptContext): Array[Byte] =
  v.getOrElse("current", Array.emptyByteArray).asInstanceOf[Array[Byte]]

def originalPathOf(v: RawScriptContext): java.nio.file.Path =
  v.get("originalPath").map(x => java.nio.file.Path.of(x.toString)).orNull

def ttmapNameOf(v: RawScriptContext): String = {
  val s = v.get("ttmap").map(_.toString).filter(_.nonEmpty).orNull
  if (s != null && java.nio.file.Files.isRegularFile(java.nio.file.Path.of(s))) s else null
}

def currentJson(v: RawScriptContext): ObjectNode = {
  val op = originalPathOf(v)
  if (op == null) throw new RuntimeException("v.toJson() requires a Unity bundle context")
  com.github.jpabscale.asset4j.api.AssetService.toJsonNodeBytes(currentBytesOf(v), op, ttmapNameOf(v), op)
}

def jsonBytes(v: RawScriptContext, node: JsonNode): Array[Byte] =
  com.github.jpabscale.asset4j.api.AssetService.fromJsonNode(node, ttmapNameOf(v))

def jsonBytesFromString(v: RawScriptContext, s: String): Array[Byte] =
  jsonBytes(v, jsonMapper.readTree(s))

def patchResourceBytes(v: RawScriptContext, name: String): Array[Byte] = {
  val dir = v.get("patchDir").map(x => java.nio.file.Path.of(x.toString)).orNull
  if (dir == null) null
  else {
    val p = dir.resolve(name)
    if (java.nio.file.Files.isRegularFile(p)) java.nio.file.Files.readAllBytes(p) else null
  }
}

def evalRawScript(lang: Lang, patchPath: os.Path, target: String, v: RawScriptContext): Array[Byte] = {
  val code = os.read(patchPath)
  def err(t: Throwable): Nothing = automod.exit(-1, 
    s"""Evaluation failed for raw patch $patchPath ($lang) on $target: ${t.getMessage}
       |$code""".stripMargin)
  def toBytes(r: Any): Array[Byte] = r match {
    case b: Array[Byte] => b
    case s: String => s.getBytes(java.nio.charset.StandardCharsets.UTF_8)
    case _ => automod.exit(-1, 
      s"Raw script patch $patchPath ($lang) on $target must return a byte array or a string")
  }
  lang match {
    case Lang.Scala =>
      try {
        val f = evalScala[RawScriptContext => Any](
          s"""{
             |(vm: scala.collection.immutable.Map[String, Any]) => {
             |  val v = new {
             |    def orig: Array[Byte] = vm("orig").asInstanceOf[Array[Byte]]
             |    def current: Array[Byte] = vm("current").asInstanceOf[Array[Byte]]
             |    def apply(key: String): Any = vm(key)
             |    def toJson(): com.fasterxml.jackson.databind.node.ObjectNode = patchlet.currentJson(vm)
             |    def fromJson(node: com.fasterxml.jackson.databind.JsonNode): Array[Byte] = patchlet.jsonBytes(vm, node)
             |    def resource(name: String): Array[Byte] = patchlet.patchResourceBytes(vm, name)
             |  }
             |  def result(): Any = {
             |    $code
             |  }
             |  result()
             |}
             |}""".stripMargin)
        toBytes(f(v))
      } catch { case t: Throwable => err(t) }
    case Lang.Kotlin =>
      try {
        val jv = new java.util.HashMap[String, Any]()
        for ((k, value) <- v) jv.put(k, value)
        toBytes(kotlinapi.KotlinApi.evalKotlin(jv, code))
      } catch { case t: Throwable => err(t) }
    case Lang.Js | Lang.Typescript =>
      try {
        val (context, r) = lang match {
          case Lang.Typescript => evalTypescriptRaw(v, code)
          case _ => evalJsRaw(v, code)
        }
        try {
          if (r.isString) r.asString.getBytes(java.nio.charset.StandardCharsets.UTF_8)
          else if (r.hasArrayElements) {
            val out = new Array[Byte](r.getArraySize.toInt)
            for (i <- 0 until out.length) out(i) = r.getArrayElement(i).asInt.toByte
            out
          } else automod.exit(-1, 
            s"Raw script patch $patchPath ($lang) on $target must return a Uint8Array or a string")
        } finally context.close
      } catch { case t: Throwable => err(t) }
    case Lang.Python =>
      try {
        val (context, r) = evalPythonRaw(v, code)
        try {
          if (r.isString) r.asString.getBytes(java.nio.charset.StandardCharsets.UTF_8)
          else if (r.hasArrayElements) {
            val out = new Array[Byte](r.getArraySize.toInt)
            for (i <- 0 until out.length) out(i) = r.getArrayElement(i).asInt.toByte
            out
          } else automod.exit(-1, 
            s"Raw script patch $patchPath ($lang) on $target must return a bytearray or a str")
        } finally context.close
      } catch { case t: Throwable => err(t) }
    case Lang.Lua =>
      try {
        val mapTable = LuaValue.tableOf()
        for ((k, value) <- v if k != "orig" && k != "current")
          mapTable.set(LuaValue.valueOf(k), LuaValue.valueOf(value.toString))
        val vLua = LuaValue.tableOf(Array[LuaValue](
          LuaValue.valueOf("orig"), org.luaj.vm2.LuaString.valueOf(v.getOrElse("orig", Array.emptyByteArray).asInstanceOf[Array[Byte]]),
          LuaValue.valueOf("current"), org.luaj.vm2.LuaString.valueOf(v.getOrElse("current", Array.emptyByteArray).asInstanceOf[Array[Byte]]),
          LuaValue.valueOf("map"), mapTable))
        val g = org.luaj.vm2.lib.jse.JsePlatform.standardGlobals
        org.luaj.vm2.luajc.LuaJC.install(g)
        g.set(LuaValue.valueOf("JSON"), luaJson)
        val toJson = new LuaFunction {
          override def call(arg: LuaValue): LuaValue =
            luaJson.get("parse").call(LuaValue.valueOf(currentJson(v).toString))
        }
        val fromJson = new LuaFunction {
          override def call(arg: LuaValue): LuaValue =
            org.luaj.vm2.LuaString.valueOf(jsonBytesFromString(v, arg.tojstring))
        }
        val resource = new LuaFunction {
          override def call(arg: LuaValue): LuaValue = {
            val b = patchResourceBytes(v, arg.tojstring)
            if (b == null) LuaValue.NIL else org.luaj.vm2.LuaString.valueOf(b)
          }
        }
        vLua.set("toJson", toJson)
        vLua.set("fromJson", fromJson)
        vLua.set("resource", resource)
        val chunk = g.load(
          s"""function __f(v)
             |  $code
             |end
             |
             |_f = __f""".stripMargin)
        chunk.call()
        val f = g.get("_f").asInstanceOf[LuaValue]
        f.call(vLua) match {
          case s: org.luaj.vm2.LuaString =>
            java.util.Arrays.copyOfRange(s.m_bytes, s.m_offset, s.m_offset + s.m_length)
          case r => r.tojstring().getBytes(java.nio.charset.StandardCharsets.UTF_8)
        }
      } catch { case t: Throwable => err(t) }
  }
}

/** Scoped class patch (`ClassName@bundle.kt`): the script IS the transform over the matching
 *  objects. [decoded] holds each matching object's Data (pathId -> ObjectNode); the script
 *  sees them as `v.objects` (native `{id, data}` per language) plus `v.className`, edits
 *  them in place, and returns the objects. automod re-encodes the returned Data. No file
 *  locations, ttmap, or externals are visible to the script. */
def evalRawScriptScoped(
  lang: Lang,
  patchPath: os.Path,
  target: String,
  v: RawScriptContext,
  decoded: java.util.Map[java.lang.Long, ObjectNode],
): java.util.Map[java.lang.Long, ObjectNode] = {
  val code = os.read(patchPath)
  val className = v.getOrElse("className", "").toString
  def err(t: Throwable): Nothing = automod.exit(-1,
    s"""Evaluation failed for raw class patch $patchPath ($lang) on $target: ${t.getMessage}
       |$code""".stripMargin)
  val out = new java.util.HashMap[java.lang.Long, ObjectNode]()
  def putOut(id: java.lang.Long, node: ObjectNode): Unit = if (id != null && node != null) out.put(id, node)
  try lang match {
    case Lang.Scala =>
      val arr = new Array[(java.lang.Long, ObjectNode)](decoded.size)
      var i = 0
      decoded.forEach((id, node) => { arr(i) = (id, node); i += 1 })
      val vc = v + ("objects" -> arr) + ("className" -> className)
      val f = evalScala[RawScriptContext => Any](
        s"""{
           |(vm: scala.collection.immutable.Map[String, Any]) => {
           |  val v = new {
           |    def objects: Array[(java.lang.Long, com.fasterxml.jackson.databind.node.ObjectNode)] =
           |      vm("objects").asInstanceOf[Array[(java.lang.Long, com.fasterxml.jackson.databind.node.ObjectNode)]]
           |    def className: String = vm("className").toString
           |  }
           |  def result(): Any = {
           |    $code
           |  }
           |  result()
           |}
           |}""".stripMargin)
      f(vc).asInstanceOf[Array[(java.lang.Long, ObjectNode)]].foreach { case (id, node) => putOut(id, node) }
    case Lang.Kotlin =>
      val lst = new java.util.ArrayList[java.util.HashMap[String, Any]]()
      decoded.forEach((id, node) => {
        val m = new java.util.HashMap[String, Any]()
        m.put("id", id); m.put("data", node)
        lst.add(m)
      })
      val vc = new java.util.HashMap[String, Any]()
      for ((k, value) <- v) vc.put(k, value)
      vc.put("objects", lst); vc.put("className", className)
      val res = kotlinapi.KotlinApi.evalKotlin(vc, code)
      res.asInstanceOf[java.util.List[java.util.HashMap[String, Any]]].forEach { m =>
        putOut(m.get("id").asInstanceOf[java.lang.Long], m.get("data").asInstanceOf[ObjectNode])
      }
    case Lang.Js | Lang.Typescript =>
      val context = graalContext("js")
      try {
        val bindings = context.getBindings("js")
        val objectsJson = new java.util.ArrayList[java.util.HashMap[String, Object]]()
        decoded.forEach((id, node) => {
          val m = new java.util.HashMap[String, Object]()
          m.put("id", java.lang.Long.valueOf(id)); m.put("dataJson", node.toString)
          objectsJson.add(m)
        })
        bindings.putMember("__objects", context.asValue(objectsJson))
        bindings.putMember("__className", context.asValue(className))
        val (_, r) = {
          val c = evalPoly(context, "js",
            s"""var __objs = __objects.map(function(o){ return { id: o.id, data: JSON.parse(o.dataJson) }; });
               |var v = { objects: __objs, className: __className };
               |$code""".stripMargin)
          (context, c)
        }
        val stringify = context.eval("js", "JSON.stringify")
        val n = r.getArraySize.toInt
        for (i <- 0 until n) {
          val e = r.getArrayElement(i)
          val id = e.getMember("id").asLong
          val data = e.getMember("data")
          val jsonStr = stringify.execute(data).asString
          putOut(id, jsonMapper.readTree(jsonStr).asInstanceOf[ObjectNode])
        }
      } finally context.close
    case Lang.Python =>
      val context = graalContext("python")
      try {
        val bindings = context.getBindings("python")
        val objectsJson = new java.util.ArrayList[java.util.HashMap[String, Object]]()
        decoded.forEach((id, node) => {
          val m = new java.util.HashMap[String, Object]()
          m.put("id", java.lang.Long.valueOf(id)); m.put("dataJson", node.toString)
          objectsJson.add(m)
        })
        bindings.putMember("__objects", context.asValue(objectsJson))
        bindings.putMember("__className", context.asValue(className))
        val r = evalPoly(context, "python",
          s"""import json
             |v = { 'objects': [ {'id': o['id'], 'data': json.loads(o['dataJson'])} for o in __objects ], 'className': __className }
             |$code""".stripMargin)
        val dumps = context.eval("python", "json.dumps")
        val n = r.getArraySize.toInt
        for (i <- 0 until n) {
          val e = r.getArrayElement(i)
          val id = e.getMember("id").asLong
          val data = e.getMember("data")
          val jsonStr = dumps.execute(data).asString
          putOut(id, jsonMapper.readTree(jsonStr).asInstanceOf[ObjectNode])
        }
      } finally context.close
    case Lang.Lua =>
      val g = org.luaj.vm2.lib.jse.JsePlatform.standardGlobals
      org.luaj.vm2.luajc.LuaJC.install(g)
      g.set(LuaValue.valueOf("JSON"), luaJson)
      val objectsTbl = LuaValue.tableOf()
      var idx = 1
      decoded.forEach((id, node) => {
        val item = LuaValue.tableOf(Array[LuaValue](
          LuaValue.valueOf("id"), LuaValue.valueOf(id.longValue.toDouble),
          LuaValue.valueOf("data"), uassetapi.toLuaValue(node)))
        objectsTbl.set(idx, item); idx += 1
      })
      val vLua = LuaValue.tableOf(Array[LuaValue](
        LuaValue.valueOf("objects"), objectsTbl,
        LuaValue.valueOf("className"), LuaValue.valueOf(className)))
      val chunk = g.load(
        s"""function __f(v)
           |  $code
           |end
           |
           |_f = __f""".stripMargin)
      chunk.call()
      val f = g.get("_f").asInstanceOf[LuaValue]
      val r = f.call(vLua)
      var k = 1
      val n = r.get("n").optint(r.length)
      while (k <= n) {
        val item = r.get(k)
        val id = item.get("id").tolong
        val data = uassetapi.fromLuaValue(item.get("data"))
        putOut(id, data.asInstanceOf[ObjectNode])
        k += 1
      }
  } catch { case t: Throwable => err(t) }
  out
}

def evalObjectNamePredicate(lang: Lang, code: String): String => Boolean = {
  def graal(f: (Context => Value, String) => (Context, Value)): String => Boolean = { (v: String) => 
    val (context, r) = f((c: Context) => uassetapi.toPolyValue(c, TextNode.valueOf(v)), code)
    try uassetapi.toValue[Boolean](uassetapi.fromPolyValue(r)).get finally context.close
  }
  lang match {
    case Lang.Scala => evalScala[String => Boolean](s"{ (v: String) => def predicate(): Boolean = { $code }; predicate() }")
    case Lang.Kotlin => kotlinapi.KotlinApi.evalKotlinPredicate(code)
    case Lang.Typescript => graal(evalTypescript("declare var v: string"))
    case Lang.Js => graal(evalJs)
    case Lang.Python => graal(evalPython)
    case Lang.Lua => (s: String) => evalLua(LuaValue.valueOf(s), code, 
        msg => s"""Failed to evaluate Lua code with v as $s: $msg
                  |$code""".stripMargin) match {
      case node: BooleanNode => node.booleanValue
      case _ => 
        automod.exit(-1, 
          s"""The Lua code does not return a boolean value: $code""")
    }
  }
}

def checkPatches(uassetName: String, map: automod.UAssetPropertyChanges): automod.UAssetPropertyChanges = {
  for ((objName, properties) <- map) {
    def checkAddProperties: Boolean = {
      def checkObject(o: JsonNode): Boolean = if (o.isObject) {
        o.get(uassetapi.Constants.typeKey) match {
          case _: TextNode => true
          case _ => false
        }
      } else !o.isArray
      if (properties.size != 1) return false
      properties.get(addValueKey).flatMap(_.newValueOpt) match {
        case Some(node: ObjectNode) => checkObject(node)
        case Some(node: ArrayNode) =>
          for (i <- 0 until node.size) {
            if (!checkObject(node.get(i))) return false
          }
          true
        case _ => false
      }
    }
    isKeyPrefix(uassetName, objName)
    for ((property, value) <- properties) {
      value.newValueOpt match {
        case Some(node: TextNode) => getKeyPrefix(node.textValue) match {
          case Some(`codePrefixScala`) =>
          case Some(`codePrefixTypescript`) =>
          case Some(`codePrefixJavascript`) =>
          case Some(`codePrefixPython`) =>
          case Some(`codePrefixLua`) =>
          case Some(`codePrefixKotlin`) =>
          case Some(prefix) => automod.exit(-1, s"Unrecognized value prefix for $uassetName/$objName/$property: $prefix")
          case _ =>
        }
        case _ =>
      }
    }
  }
  map
}

def getKeyPrefix(key: String): Option[String] = {
  if (key.isEmpty) return None
  val i = key.indexOf(':')
  if (key.head == '.' && i > 0) return Some(key.substring(0, i + 1))
  if (key.startsWith(codePrefixScala)) return Some(key.substring(0, codePrefixScala.length)) 
  if (key.startsWith(codePrefixTypescript)) return Some(key.substring(0, codePrefixTypescript.length)) 
  if (key.startsWith(codePrefixJavascript)) return Some(key.substring(0, codePrefixJavascript.length)) 
  if (key.startsWith(codePrefixPython)) return Some(key.substring(0, codePrefixPython.length)) 
  if (key.startsWith(codePrefixLua)) return Some(key.substring(0, codePrefixLua.length)) 
  if (key.startsWith(codePrefixKotlin)) return Some(key.substring(0, codePrefixKotlin.length)) 
  return None
}

def isKeyPrefix(title: String, key: String): Boolean = getKeyPrefix(key) match {
  case Some(prefix) => prefix match {
    case `codePrefixScala` => true
    case `codePrefixTypescript` => true
    case `codePrefixJavascript` => true
    case `codePrefixPython` => true
    case `codePrefixLua` => true
    case `codePrefixKotlin` => true
    case `javaRegexPrefix` => true
    case `atPrefix` => true
    case _ => automod.exit(-1, s"Unrecognized prefix for $title: '$prefix'")
  }
  case _ => false
}

type CodeContext = {
  def objName: String
  def orig[T]: T
  def currentOpt[T]: Option[T]
  def valueOf[T](objName: String, property: String): Option[T]
  def ast: JsonNode
  def origAst: JsonNode
}

class PolyCodeContext(context: Context,
                      uassetName: String,
                      addToFilePatches: Boolean,
                      dataMap: collection.Map[String, ObjectNode],
                      @HostAccess.Export val objName: Value,
                      @HostAccess.Export val orig: Value,
                      @HostAccess.Export val current: Value,
                      @HostAccess.Export val ast: Value,
                      @HostAccess.Export val origAst: Value) {
  def valueOf(objName: Value, property: Value): Value = {
    dataMap.get(uassetapi.fromPolyValue(objName).asInstanceOf[String]) match {
      case Some(node) =>
        val obj = uassetapi.Struct(uassetName, node, addToFilePatches)
        uassetapi.toPolyValue(context, obj.getJson(uassetapi.fromPolyValue(property).asInstanceOf[String]))
      case _ => uassetapi.toPolyValue(context, NullNode.instance)
    }
  }
}

def evalStructProperty(lang: Lang, uassetName: String, addToFilePatches: Boolean, dataMap: collection.Map[String, ObjectNode], 
                       code: String, obj: StructLike, property: String, orig: JsonNode, _ast: automod.JsonAst, 
                       _origAst: automod.JsonAst): JsonNode = {
  val currentValue = obj.getJson(property)
  evalProperty(lang, uassetName, addToFilePatches, dataMap, code, obj.name, currentValue, 
    uassetapi.Struct(uassetName, orig, addToFilePatches = false).getJson(property), 
    property, _ast, _origAst)
}

def evalProperty(lang: Lang, uassetName: String, addToFilePatches: Boolean, dataMap: collection.Map[String, ObjectNode], 
                 code: String, name: String, currentValue: JsonNode, origValue: => JsonNode, property: String, _ast: automod.JsonAst, 
                 _origAst: automod.JsonAst): JsonNode = {
  lang match {
    case Lang.Scala =>
      try {
        val nil = """"null""""
        val propertyF = evalScala[CodeContext => Any](
        s"""{
            |import com.fasterxml.jackson.databind.JsonNode
            |import com.fasterxml.jackson.databind.node.{JsonNodeFactory, ArrayNode, DoubleNode, IntNode, NullNode, ObjectNode, TextNode}
            |
            |lazy val mapper = new com.fasterxml.jackson.databind.ObjectMapper
            |def toJsonNode(content: String): JsonNode = if (content == null) null else mapper.readTree(content)
            |def toJsonNodeT[T <: JsonNode](content: String): T = (if (content == null) null else mapper.readTree(content)).asInstanceOf[T]
            |def fromJsonNode(node: JsonNode): String = Option(node).map(_.toString).getOrElse($nil)
            |
            |(v: {
            |    def objName: String
            |    def orig[T]: T
            |    def currentOpt[T]: Option[T]
            |    def valueOf[T](objName: String, property: String): Option[T]
            |    def ast: JsonNode
            |    def origAst: JsonNode
            |  }) => 
            |  def calc(): Any = {
            |    $code
            |  }
            |  calc() 
            |}""".stripMargin)              
        val ctx = (new {
          def objName: String = name
          def orig[T]: T = uassetapi.toValue[T](origValue).get
          def currentOpt[T]: Option[T] = uassetapi.toValue[T](currentValue)
          def valueOf[T](objName: String, property: String): Option[T] = {
            dataMap.get(objName) match {
              case Some(node) =>
                val obj = uassetapi.Struct(uassetName, node, addToFilePatches)
                uassetapi.toValue[T](obj.getJson(property))
              case _ => None
            }
          }
          def ast: JsonNode = _ast.json[JsonNode]
          def origAst: JsonNode = _origAst.json[JsonNode]
        }: CodeContext)
        return uassetapi.fromValue(propertyF(ctx))
      } catch {
        case t: Throwable =>
          automod.exit(-1, 
            s"""Evaluation failed for $name/$property with the game original value of ${origValue} and 
               |the current value ${currentValue}: ${t.getMessage}
               |$code""".stripMargin)
      }
    case Lang.Kotlin =>
      try {
        def toJava(v: Any): Any = v match {
          case m: scala.collection.Map[_, _] =>
            val r = new java.util.LinkedHashMap[String, Any]()
            m.foreach { case (k, v2) => r.put(k.toString, toJava(v2)) }
            r
          case s: scala.collection.Iterable[_] =>
            val r = new java.util.ArrayList[Any]()
            s.foreach(x => r.add(toJava(x)))
            r
          case other => other
        }
        val jv = new java.util.HashMap[String, Any]()
        jv.put("objName", name)
        jv.put("orig", toJava(uassetapi.toValue[Any](origValue).getOrElse(null)))
        jv.put("current", toJava(uassetapi.toValue[Any](currentValue).getOrElse(null)))
        jv.put("ast", _ast.json[JsonNode])
        jv.put("origAst", _origAst.json[JsonNode])
        jv.put("valueOf", kotlinapi.KotlinApi.valueOfFn(dataMap, uassetName, addToFilePatches))
        return uassetapi.fromValue(kotlinapi.KotlinApi.evalKotlinValue(jv, code))
      } catch {
        case t: Throwable =>
          automod.exit(-1, 
            s"""Evaluation failed for $name/$property with the game original value of ${origValue} and 
               |the current value ${currentValue}: ${t.getMessage}
               |$code""".stripMargin)
      }
    case Lang.Lua =>
      val v = LuaValue.tableOf(Array[LuaValue](
        LuaValue.valueOf("objName"), LuaValue.valueOf(name),
        LuaValue.valueOf("orig"), uassetapi.toLuaValue(origValue),
        LuaValue.valueOf("current"), uassetapi.toLuaValue(currentValue),
        LuaValue.valueOf("ast"), uassetapi.toLuaValue(_ast.json[JsonNode]),
        LuaValue.valueOf("origAst"), uassetapi.toLuaValue(_origAst.json[JsonNode]),
        LuaValue.valueOf("valueOf"), new LuaFunction {
          override def call(arg1: LuaValue, arg2: LuaValue): LuaValue = {
            val objName = arg1.tojstring
            val property = arg2.tojstring
            dataMap.get(objName) match {
              case Some(node) =>
                val obj = uassetapi.Struct(uassetName, node, addToFilePatches)
                uassetapi.toLuaValue(obj.getJson(property))
              case _ => LuaValue.NIL
            }
          }
        }))
      evalLua(v, code, msg => 
            s"""Evaluation failed for $name/$property with the game original value of ${origValue} and 
               |the current value ${currentValue}: $msg
               |$code""".stripMargin)
    case _ =>
      val v = (c: Context) => c.asValue(
          new PolyCodeContext(c, uassetName, addToFilePatches, dataMap,
                              uassetapi.toPolyValue(c, TextNode.valueOf(name)), 
                              uassetapi.toPolyValue(c, origValue),
                              uassetapi.toPolyValue(c, currentValue),
                              uassetapi.toPolyValue(c, _ast.json[JsonNode]),
                              uassetapi.toPolyValue(c, _origAst.json[JsonNode])))     
      val (context, r) = (lang: @unchecked) match {
        case Lang.Typescript => evalTypescript(
          s"""type JsonNode = { [key: string]: any }
             |
             |interface PolyCodeContext {
             |  objName(): string
             |  orig(): any
             |  current(): any | undefined
             |  ast(): JsonNode
             |  origAst(): JsonNode
             |  valueOf(objName: string, property: string): any | undefined
             |}
             |
             |declare var v: PolyCodeContext""".stripMargin
        )(v, code)
        case Lang.Js => evalJs(v, code)
        case Lang.Python => evalPython(v, code)
      }                        
      return try uassetapi.fromPolyValue(r) finally context.close
  }
}

sealed trait FilteredChanges {
  def addToFilePatches: Boolean
  def uassetName: String
  def ast: automod.JsonAst
  def orig: automod.JsonAst
  def dataMap: collection.Map[String, ObjectNode]
  def changes: automod.PropertyChanges
  def engineUnityMode: Boolean = false
  def applyAtChange(name: String, o: ObjectNode, property: String, v: JsonNode, vOrig: JsonNode): Unit = {
    var value = v
    value match {
      case v: TextNode => 
        def code(codePrefix: String, lang: Lang): Unit =
          value = evalProperty(lang, uassetName, addToFilePatches, dataMap, v.textValue.substring(codePrefix.length), 
                               name, o.get(property), vOrig, property, this.ast, this.orig)
        getKeyPrefix(v.textValue) match {
          case Some(`codePrefixScala`) => code(codePrefixScala, Lang.Scala)
          case Some(`codePrefixTypescript`) => code(codePrefixTypescript, Lang.Typescript)
          case Some(`codePrefixJavascript`) => code(codePrefixJavascript, Lang.Js)
          case Some(`codePrefixPython`) => code(codePrefixPython, Lang.Python)
          case Some(`codePrefixLua`) => code(codePrefixLua, Lang.Lua)
          case Some(`codePrefixKotlin`) => code(codePrefixKotlin, Lang.Kotlin)
          case _ =>
        }
      case _ =>
    }
    if (engineUnityMode) {
      if (o.get(property) == null) automod.exit(-1, s"Cannot replace a non-existing property: $name/$property")
      val oldValueOpt = Option(o.replace(property, value))
      automod.logPatch(uassetName, s"* $name/$property: ${automod.toJsonPrettyString(oldValueOpt)} => ${automod.toJsonPrettyString(Some(value))}", console = false)
      if (addToFilePatches) automod.updatePatch(uassetName, name, property, automod.ValuePair(Some(value), oldValueOpt))
    } else {
      uassetapi.objSetJson(isAt = true, addToFilePatches, uassetName, name, o, property, value)
    }
  }
  def applyStructChanges(path: String, node: JsonNode, orig: JsonNode): Unit = {
    if (engineUnityMode) {
      val obj = unityapi.UnityStruct(uassetName, node, addToFilePatches)
      for ((property, valueOldValuePair) <- changes) {
        var value = valueOldValuePair.newValueOpt.get
        value match {
          case v: TextNode => 
            def code(codePrefix: String, lang: Lang): Unit =
              value = evalStructProperty(lang, uassetName, addToFilePatches, dataMap, v.textValue.substring(codePrefix.length), 
                                         obj, property, orig, this.ast, this.orig)
            getKeyPrefix(v.textValue) match {
              case Some(`codePrefixScala`) => code(codePrefixScala, Lang.Scala)
              case Some(`codePrefixTypescript`) => code(codePrefixTypescript, Lang.Typescript)
              case Some(`codePrefixJavascript`) => code(codePrefixJavascript, Lang.Js)
              case Some(`codePrefixPython`) => code(codePrefixPython, Lang.Python)
              case Some(`codePrefixLua`) => code(codePrefixLua, Lang.Lua)
              case Some(`codePrefixKotlin`) => code(codePrefixKotlin, Lang.Kotlin)
              case _ =>
            }
          case _ =>
        }
        obj.setJson(property, value)
      }
    } else {
      val obj = uassetapi.Struct(uassetName, node, addToFilePatches)
      for ((property, valueOldValuePair) <- changes) {
        var value = valueOldValuePair.newValueOpt.get
        value match {
          case v: TextNode => 
            def code(codePrefix: String, lang: Lang): Unit =
              value = evalStructProperty(lang, uassetName, addToFilePatches, dataMap, v.textValue.substring(codePrefix.length), 
                                         obj, property, orig, this.ast, this.orig)
            getKeyPrefix(v.textValue) match {
              case Some(`codePrefixScala`) => code(codePrefixScala, Lang.Scala)
              case Some(`codePrefixTypescript`) => code(codePrefixTypescript, Lang.Typescript)
              case Some(`codePrefixJavascript`) => code(codePrefixJavascript, Lang.Js)
              case Some(`codePrefixPython`) => code(codePrefixPython, Lang.Python)
              case Some(`codePrefixLua`) => code(codePrefixLua, Lang.Lua)
              case Some(`codePrefixKotlin`) => code(codePrefixKotlin, Lang.Kotlin)
              case _ =>
            }
          case _ =>
        }
        obj.setJson(property, value)
      }
    }
  }
}

case class AtFilteredChanges(addToFilePatches: Boolean,
                             uassetName: String,
                             ast: automod.JsonAst,
                             orig: automod.JsonAst,
                             dataMap: collection.Map[String, ObjectNode],
                             path: String, 
                             changes: automod.PropertyChanges,
                             unityMode: Boolean = false) extends FilteredChanges {
  override def engineUnityMode: Boolean = unityMode
  def applyChanges(ast: automod.JsonAst): Unit = {
    val nodes: Seq[(JsonNode, JsonNode)] = if (path.isEmpty) {
      automod.exit(-1, s"The path for $uassetName name cannot be empty")
    } else if (path.head == '/') {
      val node = ast.json[JsonNode].at(path)
      if (node.isMissingNode) automod.exit(-1, s"Could not find $uassetName's path: $path")
      Seq((node, orig.json[JsonNode].at(path)))
    } else if (path.head == '$') {
      try {
        val array = ast.read[ArrayNode](path)
        val origArray = orig.read[ArrayNode](path)
        for (i <- 0 until array.size if array.get(i).isInstanceOf[ObjectNode]) yield (array.get(i), origArray.get(i))
      } catch {
        case t: Throwable => automod.exit(-1, 
          s"""Failed to search $uassetName path: $path
             |  reason: ${t.getMessage}""".stripMargin)
      }
    } else {
      automod.exit(-1, s"Unrecognized path for $uassetName: $path")
    }
    if (nodes.isEmpty) automod.exit(-1, s"Could not find objects for $uassetName: $path")
    for ((node, orig) <- nodes.par) {
      if (uassetapi.isStruct(node)) applyStructChanges(path, node, orig)
      else node match {
        case node: ObjectNode =>
          for ((property, valuePair) <- changes) Option(node.get(property)) match {
            case Some(old) => 
              applyAtChange(path, node, property, valuePair.newValueOpt.get, if (orig == null) null else orig.get(property))
            case _ => automod.exit(-1, s"$uassetName @$path does not have the property: $property")
          }
        case _ => automod.exit(-1, s"$uassetName @$path is neither a UAssetAPI's struct nor a JSON object node")
      }
    }
  }
}

case class KeyFilteredChanges(addToFilePatches: Boolean,
                              uassetName: String,
                              ast: automod.JsonAst,
                              orig: automod.JsonAst,
                              dataMap: collection.Map[String, ObjectNode],
                              f: String => Boolean, 
                              changes: automod.PropertyChanges,
                              unityMode: Boolean = false) extends FilteredChanges {
  override def engineUnityMode: Boolean = unityMode
  def apply(key: String): Boolean = f(key)
}

def kfcMap(maxOrder: Int, order: Int, addToFilePatches: Boolean, uassetName: String, ast: automod.JsonAst, origAst: automod.JsonAst,
           origAstPath: automod.JsonAst, t: automod.UAssetPropertyChanges, unityMode: Boolean = false): (collection.mutable.TreeMap[String, KeyFilteredChanges], collection.mutable.TreeMap[String, AtFilteredChanges], automod.UAssetPropertyChanges) = {
  var r1 = collection.mutable.TreeMap.empty[String, KeyFilteredChanges]
  var r2 = collection.mutable.TreeMap.empty[String, AtFilteredChanges]
  var rt = automod.emptyUAssetPropertyChanges 
  val dataMap: collection.Map[String, ObjectNode] = ast.json[JsonNode].at(automod.dataTablePath) match {
    case array: ArrayNode => automod.toDataMap(array)
    case _ => Map.empty
  } 
  for ((key, properties) <- t) {
    r1.get(key) match {
      case Some(_) => automod.exit(-1, s"Redefined key for $uassetName: $key")
      case _ =>
    }
    def code(codePrefix: String, lang: Lang, key: String): Unit = {
      val (code, props) = properties.get(codePrefix) match {
        case Some(text) => 
          text.newValueOpt match {
            case Some(codeText: TextNode) => (codeText.asText, properties.removed(codePrefix))
            case _ => automod.exit(-1, s"Invalid code for $uassetName: $text")
          }
        case _ => (key.substring(codePrefix.length), properties)
      }
      try {
        val fun = evalObjectNamePredicate(lang, code)  
        r1.put(key, KeyFilteredChanges(addToFilePatches, uassetName, ast, origAst, dataMap, fun, props, unityMode))
      } catch {
        case _: Throwable => automod.exit(-1, s"Invalid code for $uassetName: $code")
      }
    }
    patchlet.getKeyPrefix(key) match {
      case Some(prefix) => prefix match {
        case `codePrefixScala` => code(codePrefixScala, Lang.Scala, key)
        case `codePrefixTypescript` => code(codePrefixTypescript, Lang.Typescript, key)
        case `codePrefixJavascript` => code(codePrefixJavascript, Lang.Js, key)
        case `codePrefixPython` => code(codePrefixPython, Lang.Python, key)
        case `codePrefixLua` => code(codePrefixLua, Lang.Lua, key)
        case `codePrefixKotlin` => code(codePrefixKotlin, Lang.Kotlin, key)
        case `atPrefix` =>
          var path = key.substring(atPrefix.length).trim
          var i = 0
          while (i < path.length && path(i) != '/' && path(i) != '$') i += 1
          if (i >= path.length) automod.exit(-1, s"Invalid $atPrefix path: $path")
          path = path.substring(i)
          val isDataTable = if (path.startsWith(automod.dataTablePath + "/")) true else {
            val r = (if (path.head == '/') {
              val array = JsonNodeFactory.instance.arrayNode
              Option(origAstPath.json[JsonNode].at(path)).foreach(_ => array.add(toJsonPath(path)))
              array
            } else util.Try(origAstPath.read(path)).getOrElse(JsonNodeFactory.instance.arrayNode)).asInstanceOf[ArrayNode]
            val prefix = dataTableJsonPath + "["
            def allWithPrefix: Boolean = {
              if (r.isEmpty) return false
              for (i <- 0 until r.size if !r.get(i).textValue.startsWith(prefix)) return false
              true
            }
            allWithPrefix
          }
          r2.put(key, AtFilteredChanges(addToFilePatches = isDataTable, uassetName, ast, origAst, dataMap, path, properties, unityMode))
          if (addToFilePatches && !isDataTable) {
            val digits = (maxOrder + 1).toString.length
            val name = s"$atPrefix #${(for (i <- 0 until digits - order.toString.length) yield "0").mkString}$order $path"
            automod.updatePatches(uassetName, name, properties)
          }
        case `javaRegexPrefix` =>
          val regexText = key.substring(javaRegexPrefix.length).trim
          try {
            val regex = regexText.r
            val fun = (s: String) => regex.matches(s)
            r1.put(key, KeyFilteredChanges(addToFilePatches, uassetName, ast, origAst, dataMap, fun, properties, unityMode))
          } catch {
            case _: Throwable => automod.exit(-1, s"Invalid Java regex for $uassetName: $regexText")
          }
        case _ => automod.exit(-1)
      }
      case _ => rt = rt + (key -> properties)
    }
  }
  (r1, r2, rt) 
}

def applyRawJsonPatches(uassetName: String, ast: automod.JsonAst, origAst: automod.JsonAst, origAstPath: automod.JsonAst, tree: automod.UAssetPropertyChanges, unityMode: Boolean = false): Unit = {
  for ((key, _) <- tree) {
    if (!key.startsWith(atPrefix)) automod.exit(-1, 
      s"Raw JSON patch for $uassetName only supports '$atPrefix' sections, not '$key'")
  }
  val (keyFiltered, atFiltered, plain) = kfcMap(0, 0, addToFilePatches = false, uassetName, ast, origAst, origAstPath, tree, unityMode)
  if (keyFiltered.nonEmpty) automod.exit(-1, s"Raw JSON patch for $uassetName only supports '$atPrefix' sections, not key-filtered ones: ${keyFiltered.keys.mkString(", ")}")
  if (plain.nonEmpty) automod.exit(-1, s"Raw JSON patch for $uassetName only supports '$atPrefix' sections, not property sections: ${plain.keys.mkString(", ")}")
  for (kfc <- atFiltered.values) kfc.applyChanges(ast)
}