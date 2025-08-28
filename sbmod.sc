//> using scala 2.13.16
//> using dep com.fasterxml.jackson.core:jackson-databind:2.19.1
//> using dep com.fasterxml.jackson.dataformat:jackson-dataformat-toml:2.19.1
//> using dep com.lihaoyi::os-lib:0.11.4

import com.fasterxml.jackson.core.util.{DefaultIndenter, DefaultPrettyPrinter}
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.fasterxml.jackson.databind.node.{ArrayNode, DoubleNode, IntNode, ObjectNode, TextNode}
import com.fasterxml.jackson.dataformat.toml.TomlMapper
import com.fasterxml.jackson.core.`type`.TypeReference
import java.util.{Map => JMap}
import scala.collection.immutable.{TreeMap, TreeSet}

def exit(code: Int, msg: String = null): Nothing = {
  Option(msg).foreach(println(_))
  System.exit(code)
  throw new RuntimeException
}

if (!scala.util.Properties.isWin) exit(-1, "This script can only be used in Windows")

val header = s"Stellar Blade Auto Modding Script v1.7"

val argName = args.head

def printUsage(): Unit = {
  exit(0,
    s"""$header
       |
       |Usage: scala-cli sbmod.sc -- [ <mod-name> <path-to-StellarBlade>
       |                             | .code <path-to-jd-patch-file>
       |                             | .diff <from-path> <to-path> <out-path>
       |                             | .setup
       |                             | .toml <out-path>
       |                             ]
       |
       |  .code    Print Auto Modding Script patching code from a jd patch file
       |  .diff    Recursively diff JSON files and write jd and TOML patch files
       |  .setup   Only set up modding tools
       |  .toml    Merge existing patch files in patches as TOML patch files""".stripMargin)
}

argName match {
  case ".setup" => if (args.length != 1) printUsage()
  case ".diff" => if (args.length != 4) printUsage()
  case _ => if (args.length != 2) printUsage()
}

def absPath(p: String): os.Path = os.Path(new java.io.File(p).getAbsolutePath)

lazy val argPath = absPath(args(1))
lazy val sbPakDir = argPath / "SB" / "Content" / "Paks"

val retocVersion = "0.1.2"
val uassetGuiVersion = "1.0.3"
val fmodelSha = "03a4f79c3aab3516005de92786183451e81601f5"
val fmodelShortSha = fmodelSha.substring(0, 7)
val jdVersion = "2.2.3"
val sbMapVersion = "1.1.0"
val ueVersion = "4.26"
val ueVersionCode = s"UE${ueVersion.replace('.', '_')}"

val retocZip = "retoc-x86_64-pc-windows-msvc.zip"
val sbMap = s"StellarBlade_$sbMapVersion"
val sbMapFilename = s"$sbMap.usmap"

val retocUrl = s"https://github.com/trumank/retoc/releases/download/v$retocVersion/$retocZip"
val uassetGuiUrl = s"https://github.com/atenfyr/UAssetGUI/releases/download/v$uassetGuiVersion/UAssetGUI.exe"
val sbMapUrl = s"https://github.com/Stellar-Blade-Modding-Team/Stellar-Blade-Modding-Guide/raw/0eab1b4d7c1b88dea72298f60f6bb871682d3d1f/$sbMapFilename"
val fmodelUrl = s"https://github.com/4sval/FModel/releases/download/qa/$fmodelSha.zip"
val jdUrl = s"https://github.com/josephburnett/jd/releases/download/v$jdVersion/jd-amd64-windows.exe"

val workingDir = os.pwd
val retocExe = workingDir / "retoc.exe"
val uassetGuiExe = workingDir / "UAssetGUI.exe"
val fmodelExe = workingDir / "FModel.exe"
val jdExe = workingDir / "jd.exe"
val uassetGuiSettingsDir = os.Path(System.getenv("LOCALAPPDATA")) / "UAssetGUI"
val uassetGuiConfig = uassetGuiSettingsDir / "config.json"
val uassetGuiMappingsDir = uassetGuiSettingsDir / "Mappings"

def setupModTools(): Boolean = {
  var setup = true
  if (!os.exists(retocExe)) {
    setup = false
    println(s"Please wait while setting up retoc v$retocVersion in $workingDir ...")
    os.proc("curl", "-JLO", retocUrl).call(cwd = workingDir)
    os.proc("tar", "-xf", retocZip).call(cwd = workingDir)
    os.remove.all(workingDir / retocZip)
    os.move(workingDir / "LICENSE", workingDir / "retoc-LICENSE")
    os.move(workingDir / "README.md", workingDir / "retoc-README.md")
    println()
  }

  if (!os.exists(uassetGuiExe)) {
    setup = false
    println(s"Please wait while setting up UAssetGUI v$uassetGuiVersion in $workingDir ...")
    os.proc("curl", "-JLO", uassetGuiUrl).call(cwd = workingDir)
    println()
  }

  if (!os.exists(workingDir / sbMapFilename)) {
    setup = false
    println(s"Please wait while setting up $sbMapFilename in $workingDir ...")
    os.proc("curl", "-JLO", sbMapUrl).call(cwd = workingDir)
    println()

    val src = workingDir / sbMapFilename
    val dest = uassetGuiMappingsDir / sbMapFilename
    if (!os.exists(dest)) {
      os.makeDir.all(uassetGuiMappingsDir)
      os.copy.over(src, dest)
      println(s"Copied map file to $dest")
      println()
    }
  }

  if (!os.exists(fmodelExe)) {
    setup = false
    val fmodelZip = s"${fmodelExe.last}.zip"
    println(s"Please wait while setting up FModel @$fmodelShortSha in $workingDir ...")
    os.proc("curl", "-JLo", fmodelZip, fmodelUrl).call(cwd = workingDir)
    os.proc("tar", "-xf", fmodelZip).call(cwd = workingDir)
    os.remove.all(workingDir / fmodelZip)
    println()
  }

  if (!os.exists(jdExe)) {
    setup = false
    println(s"Please wait while setting up jd v$jdVersion in $workingDir ...")
    os.proc("curl", "-JLo", jdExe.last, jdUrl).call(cwd = workingDir)
    println()
  }

  setup
}

def jdPatchTree(path: os.Path): TreeMap[String, TreeMap[String, String]] = {
  val entryPathPrefix = """@ [0,"Rows","""
  var map = TreeMap.empty[String, TreeMap[String, String]]
  for (Array(entryPath, _, valueText) <- os.read(path).trim.replace("\r", "").split('\n').map(_.trim).grouped(3)) {
    var ok = false
    if (entryPath.startsWith(entryPathPrefix)) {
      entryPath.substring(entryPathPrefix.length, entryPath.length - 1).split(',').map(_.trim) match {
        case Array(n, p) =>
          ok = true
          var name = n
          var property = p
          name = name.substring(1, name.length - 1)
          property = property.substring(1, property.length - 1)
          var value = valueText.substring(2).trim
          if (value.contains("::")) value = "\"" + value.substring(value.lastIndexOf("::") + 2)
          if (value(0) != '"' && (value.contains('.') && value.toIntOption.isEmpty)) value = value + "d"
          map = map + (name -> (map.getOrElse(name, TreeMap.empty[String, String]) + (property -> value)))
        case _ =>
      }
    }
    if (!ok) println(s"The script currently does not handle the patch entry (skipped): $entryPath")
  }
  map
}

lazy val patches: TreeMap[String, TreeMap[String, TreeMap[String, String]]] = {
  def tomlPatchTree(path: os.Path): TreeMap[String, TreeMap[String, String]] = {
    val toml: JMap[String, JMap[String, String]] =
      new TomlMapper().readValue(path.toIO, new TypeReference[JMap[String, JMap[String, String]]] {})
    import scala.jdk.CollectionConverters._
    var map = TreeMap.empty[String, TreeMap[String, String]]
    for (name <- toml.keySet.asScala) {
      var m = TreeMap.empty[String, String]
      val properties = toml.get(name)
      for (property <- properties.keySet.asScala) {
        val value = properties.get(property)
        m = m + (property -> value)
      }
      map = map + (name -> m)
    }
    map
  }

  var map = TreeMap.empty[String, TreeMap[String, TreeMap[String, String]]]

  def add(uassetName: String, data: TreeMap[String, TreeMap[String, String]]): Unit = {
    var m = map.getOrElse(uassetName, TreeMap.empty[String, TreeMap[String, String]])
    for ((name, properties) <- data) {
      var m2 = m.getOrElse(name, TreeMap.empty[String, String])
      for ((property, value) <- properties) {
        val valueString = toJsonNode(value).toPrettyString
        m2.get(property) match {
          case Some(v) => println(s"* $name/$property: $v => $valueString")
          case _ => println(s"* $name/$property: $valueString")
        }
        m2 = m2 + (property -> value)
      }
      m = m + (name -> m2)
    }
    map = map + (uassetName -> m)
  }
  def rec(path: os.Path): Unit =   {
    for (p <- os.list(path).sortWith((p1, p2) =>
      if (os.isDir(p1) && os.isDir(p2)) p1.last <= p2.last
      else if (os.isDir(p1)) false
      else if (os.isDir(p2)) true
      else p1.last <= p2.last
    )) {
      if (os.isDir(p)) {
        rec(p)
      } else if (os.isFile(p)) {
        p.ext match {
          case "patch" =>
            println(s"Loading $p ...")
            val uassetName = p.baseName
            add(uassetName, jdPatchTree(p))
            println()
          case "toml" =>
            println(s"Loading $p ...")
            val uassetName = p.baseName
            add(uassetName, tomlPatchTree(p))
            println()
          case _ =>
        }
      }
    }
  }

  val patchesDir = workingDir / "patches"
  if (os.exists(patchesDir)) rec(patchesDir)
  map
}

def readJson(path: os.Path): JsonNode = new ObjectMapper().readTree(path.toIO)

def writeJson(path: os.Path, node: JsonNode): Unit = {
  val indenter = new DefaultIndenter("  ", DefaultIndenter.SYS_LF)
  val printer = new DefaultPrettyPrinter
  printer.indentObjectsWith(indenter)
  printer.indentArraysWith(indenter)
  os.move(path, path / os.up / path.last.replace(".json", ".orig.json"))
  new ObjectMapper().writer(printer).writeValue(path.toIO, node)
}

case class UAssetObject(value: JsonNode) {
  def obj(name: String): ObjectNode = {
    val values = value.get("Value").asInstanceOf[ArrayNode]
    for (j <- 0 until values.size) {
      val element = values.get(j).asInstanceOf[ObjectNode]
      if (element.get("Name").asText == name) {
        return element
      }
    }
    exit(-1, s"Could not find '$name' in $getName")
  }
  def setJson(name: String, value: JsonNode): JsonNode = {
    val r = obj(name).replace("Value", value)
    println(s"* $getName/$name: ${r.toPrettyString} => ${value.toPrettyString}")
    r
  }
  def set(name: String, value: Int): Int = setJson(name, IntNode.valueOf(value)).asInt
  def set(name: String, value: Double): Double = setJson(name, DoubleNode.valueOf(value)).asDouble
  def set(name: String, value: String): String = setJson(name, TextNode.valueOf(value)).asText
  def getName: String = value.get("Name").asText
  def getJson(name: String): JsonNode = obj(name).get("Value")
  def getInt(name: String): Int = getJson(name).asInt
  def getDouble(name: String): Double = getJson(name).asDouble
  def getString(name: String): String = getJson(name).asText
}

def patchEffect(obj: UAssetObject): Unit = {
  val name = obj.getName
  name match {

    // based on https://www.nexusmods.com/stellarblade/mods/802
    case "N_Drone_Scan" => obj.set("LifeTime", 20d)

    // based on https://www.nexusmods.com/stellarblade/mods/897
    case _ if name.startsWith("P_Eve_SkillTree_Just") && (name.contains("BetaGauge") || name.contains("BurstGauge")) =>
      val cmv = name match {
        case "P_Eve_SkillTree_JustParry_BetaGauge1" => 8d
        case "P_Eve_SkillTree_JustParry_BetaGauge2" => 6d
        case "P_Eve_SkillTree_JustEvade_BurstGauge1" => 4d
        case "P_Eve_SkillTree_JustEvade_BurstGauge2" => 3d
      }
      obj.set("CalculationMultipleValue", cmv)
      obj.set("OverlapCount", 1)
      obj.set("LifeType", "EffectLifeType_IndependentTime")
      obj.set("LifeTime", 11d)
      obj.set("StartDelayTime", 1d)
      obj.set("LoopIntervalTime", 1d)
      obj.set("ActiveTargetFilterAlias", "Self")
      obj.set("LoopTargetFilterAlias", "Self")

    // ... add more cases for other EffectTable properties of interest here, e.g.,
    //case _ if name.startsWith("Gear_") && name.contains("_3_MK2") && !name.endsWith("_HitDmgUp_Melee") =>
    //  obj.set("CalculationValue", obj.getDouble("CalculationValue") * 2)

    case _ =>
  }
}

def patchTargetFilter(obj: UAssetObject): Unit = {
  val name = obj.getName
  name match {

    // based on https://www.nexusmods.com/stellarblade/mods/802
    case _ if name.startsWith("N_Drone_Normal_Scan1_1_Target_") =>
      obj.set("FarDistance", 30000d)
      obj.set("TargetCheckValue1", 3000d)

    case _ =>
  }
}

def toJsonNode(value: String): JsonNode = {
  if (value(0) == '"') return TextNode.valueOf(value.substring(1, value.length - 1))
  value.toIntOption match {
    case Some(v) => return IntNode.valueOf(v)
    case _ =>
  }
  value.toDoubleOption match {
    case Some(v) => return DoubleNode.valueOf(v)
    case _ =>
  }
  TextNode.valueOf(value)
}

def patchFromTree(data: ArrayNode)(tree: TreeMap[String, TreeMap[String, String]]): Unit = {
  for (i <- 0 until data.size) {
    val obj = UAssetObject(data.get(i))
    tree.get(obj.getName) match {
      case Some(properties) =>
        for ((property, value) <- properties) {
          obj.setJson(property, toJsonNode(value))
        }
      case _ =>
    }
  }
}

def patchUasset(name: String, file: os.Path, fOpt: Option[UAssetObject => Unit]): Unit = {
  println(s"Patching $file ...")
  val ast = readJson(file)
  val data = ast.at("/Exports/0/Table/Data").asInstanceOf[ArrayNode]
  for (i <- 0 until data.size) {
    fOpt.foreach(_(UAssetObject(data.get(i))))
  }
  patches.get(name).foreach(patchFromTree(data))
  writeJson(file, ast)
  println()
}

def generateMod(): Unit = {
  val modDir = workingDir / argName
  val output = workingDir / "out"

  def recreateDir(dir: os.Path): Unit = {
    os.remove.all(dir)
    os.makeDir.all(dir)
  }

  def unpackJson(name: String): os.Path = {
    val uasset = output / "SB" / "Content" / "Local" / "Data" / s"$name.uasset"
    val uexp = s"$name.uexp"
    val json = s"$name.json"
    val r = workingDir / json

    recreateDir(output)

    println(s"Extracting $uasset ...")
    os.proc(retocExe, "to-legacy", "--no-parallel", "--version", ueVersionCode, "--filter", uasset.last, sbPakDir, output).call(cwd = workingDir)
    for (p <- os.walk(output) if os.isFile(p) && p.last != uasset.last && p.last != uexp) os.remove(p)
    println()

    println(s"Converting to $r ...")
    os.proc(uassetGuiExe, "tojson", uasset, json, s"VER_$ueVersionCode", sbMap).call(cwd = workingDir)
    os.remove.all(output)
    println()

    r
  }

  def packJson(name: String, path: os.Path): Unit = {
    val dataDir = output / "SB" / "Content" / "Local" / "Data"
    val uasset = dataDir / s"$name.uasset"
    os.makeDir.all(dataDir)

    println(s"Regenerating $uasset ...")
    os.proc(uassetGuiExe, "fromjson", path, uasset, sbMap).call(cwd = workingDir)
    println()
  }

  def packMod(): os.Path = {
    val zip = workingDir / s"$argName.zip"
    os.remove.all(zip)

    val utoc = modDir / s"${argName}_P.utoc"
    println(s"Converting to $utoc ...")
    os.proc(retocExe, "to-zen", "--no-parallel", "--version", ueVersionCode, output, utoc).call(cwd = workingDir)
    println()

    println(s"Archiving $zip ...")
    os.proc("tar", "-acf", zip.last, argName).call(cwd = workingDir)
    println()

    zip
  }

  recreateDir(modDir)

  val uassetCodeMap = TreeMap(
    // add more/change to uasset patching of interest here, e.g.,
    //"TargetFilterTable" -> patchTargetFilter _,
    "EffectTable" -> patchEffect _ // comment in this line to disable modding via code
  )

  val uassetNames = TreeSet.empty[String] ++ uassetCodeMap.keys ++ patches.keys
  val jsonMap = Map.empty[String, os.Path] ++ (for (uassetName <- uassetNames) yield (uassetName, unpackJson(uassetName)))

  for (uassetName <- uassetNames) patchUasset(uassetName, jsonMap(uassetName), uassetCodeMap.get(uassetName))

  for ((uassetName, path) <- jsonMap) packJson(uassetName, path)

  packMod()


  // comment out the following six lines to keep intermediate JSON, .uasset, .uexp, .utoc, .ucas, and .pak files
  os.remove.all(output)
  os.remove.all(modDir)
  for (path <- jsonMap.values) {
    os.remove.all(path)
    os.remove.all(path / os.up / s"${path.baseName}.orig.json")
  }
}

def setUAssetGUIConfigAndRun(f: () => Unit): Unit = {
  val oldConfigOpt = if (os.exists(uassetGuiConfig)) Some(os.read(uassetGuiConfig)) else None
  try {
    os.write.over(uassetGuiConfig,
      s"""{
         |  "PreferredVersion": $ueVersion,
         |  "PreferredMappings": "$sbMap"
         |}""".stripMargin)

    f()
  } finally {
    oldConfigOpt match {
      case Some(oldConfig) => os.write.over(uassetGuiConfig, oldConfig)
      case _ => os.remove(uassetGuiConfig)
    }
  }
}

def code(path: os.Path): Unit = {
  val map = jdPatchTree(path)

  val name = {
    val i = path.last.indexOf('.')
    if (i >= 0) path.last.substring(0, i) else path.last
  }
  var lines = Vector(
    s"def patch$name(obj: UAssetObject): Unit = {",
    "  val name = obj.getName",
    "  name match {"
  )
  for ((name, properties) <- map) {
    if (properties.size == 1) {
      for ((property, value) <- properties) lines = lines :+ s"    case \"$name\" => obj.set($property, $value)"
    } else {
      lines = lines :+ s"    case \"$name\" =>"
      for ((property, value) <- properties) lines = lines :+ s"      obj.set($property, $value)"
    }
    lines = lines :+ ""
  }
  lines = lines :+ "    case _ =>"
  lines = lines :+ "  }"
  lines = lines :+ "}"
  println(lines.mkString(util.Properties.lineSeparator))
}

def writeToml(path: os.Path, data: TreeMap[String, TreeMap[String, String]]): Unit = {
  os.remove.all(path)
  val sep = util.Properties.lineSeparator
  for ((name, properties) <- data) {
    os.write.append(path, s"[$name]$sep")
    for ((property, value) <- properties) {
      val v = toJsonNode(value).toPrettyString
      os.write.append(path, s"$property = $v$sep")
    }
    os.write.append(path, sep)
  }
  println(s"Wrote $path")
}

def toml(path: os.Path): Unit = {
  if (os.exists(path) && !os.isDir(path)) {
    exit(-1, s"$path is not a directory")
  }

  os.makeDir.all(path)
  for ((uassetName, data) <- patches) {
    val p = path / s"$uassetName.toml"
    writeToml(p, data)
  }
  if (patches.isEmpty) println("No patches to write")
  else println()
}

def diff(from: os.Path, to: os.Path, out: os.Path): Unit = {
  def rec(f: os.Path, t: os.Path): Unit = {
    if (os.isFile(f) && os.isFile(t) && f.ext.toLowerCase == "json" && t.ext.toLowerCase == "json") {
      val patch = out / s"${f.baseName}.patch"
      println(s"Diffing $f => $t ...")
      os.proc(jdExe, "-o", patch, f, t).call(cwd = workingDir, check = false).exitCode match {
        case 0 =>
          println("No changes found")
        case 1 =>
          println(s"Wrote $patch")
          writeToml(out / s"${f.baseName}.toml", jdPatchTree(patch))
        case code => exit(code, s"Error occurred when running jd")
      }
      println()
    } else if (os.isDir(f) && os.isDir(t)) {
      for (p <- os.list(f)) {
        rec(f / p.last, t / p.last)
      }
    }
  }
  if (os.exists(out) && !os.isDir(out)) {
    exit(-1, s"$out is not a directory")
  }
  os.makeDir.all(out)
  rec(from, to)
  println()
}

val setup = setupModTools()

argName match {
  case ".code" => code(argPath)
  case ".diff" => diff(argPath, absPath(args(2)), absPath(args(3)))
  case ".setup" => if (setup) println("All modding tools have been set up")
  case ".toml" => toml(argPath)
  case _ =>
    if (!os.isDir(sbPakDir)) exit(-1, s"$sbPakDir directory does not exist")
    println(
      s"""$header
         |* Game directory: $argPath
         |* Mod name to generate: $argName
         |* Working directory: $workingDir
         |* Using: retoc v$retocVersion, UAssetGUI v$uassetGuiVersion, $sbMapFilename
         |* Extra: FModel @$fmodelShortSha, jd v$jdVersion
         |""".stripMargin)
    setUAssetGUIConfigAndRun(generateMod _)
}
