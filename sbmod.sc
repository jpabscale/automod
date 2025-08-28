//> using scala 2.13.16
//> using dep com.fasterxml.jackson.core:jackson-databind:2.19.1
//> using dep com.lihaoyi::os-lib:0.11.4

import com.fasterxml.jackson.core.util.{DefaultIndenter, DefaultPrettyPrinter}
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.fasterxml.jackson.databind.node.{ArrayNode, DoubleNode, IntNode, ObjectNode, TextNode}

import scala.collection.immutable.TreeMap

def exit(code: Int, msg: String = null): Nothing = {
  Option(msg).foreach(println(_))
  System.exit(code)
  throw new RuntimeException
}

if (!scala.util.Properties.isWin) exit(-1, "This script can only be used in Windows")

val header = s"Stellar Blade Auto Modding Script v1.4"

if (args.length != 2) exit(0,
  s"""$header
     |
     |Usage: scala-cli sbmod.sc -- ( <mod-name> <path-to-StellarBlade>
     |                             | .code <path-to-jd-patch-file>
     |                             | .setup
     |                             )
     |
     |  .code    Print Auto Modding Script patching code from a jd patch file
     |  .setup   Only set up modding tools""".stripMargin)

val argName = args.head
val argPath = os.Path(new java.io.File(args.last).getAbsolutePath)
val sbPakDir = argPath / "SB" / "Content" / "Paks"

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

def setupModTools(): Unit = {
  if (!os.exists(retocExe)) {
    println(s"Please wait while setting up retoc v$retocVersion in $workingDir ...")
    os.proc("curl", "-JLO", retocUrl).call(cwd = workingDir)
    os.proc("tar", "-xf", retocZip).call(cwd = workingDir)
    os.remove.all(workingDir / retocZip)
    os.move(workingDir / "LICENSE", workingDir / "retoc-LICENSE")
    os.move(workingDir / "README.md", workingDir / "retoc-README.md")
    println()
  }

  if (!os.exists(uassetGuiExe)) {
    println(s"Please wait while setting up UAssetGUI v$uassetGuiVersion in $workingDir ...")
    os.proc("curl", "-JLO", uassetGuiUrl).call(cwd = workingDir)
    println()
  }

  if (!os.exists(workingDir / sbMapFilename)) {
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
    val fmodelZip = s"${fmodelExe.last}.zip"
    println(s"Please wait while setting up FModel @$fmodelShortSha in $workingDir ...")
    os.proc("curl", "-JLo", fmodelZip, fmodelUrl).call(cwd = workingDir)
    os.proc("tar", "-xf", fmodelZip).call(cwd = workingDir)
    os.remove.all(workingDir / fmodelZip)
    println()
  }

  if (!os.exists(jdExe)) {
    println(s"Please wait while setting up jd v$jdVersion in $workingDir ...")
    os.proc("curl", "-JLo", jdExe.last, jdUrl).call(cwd = workingDir)
    println()
  }
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
    case "N_Drone_Scan" => obj.set("LifeTime", 30d)

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

def patchTable(file: os.Path, f: UAssetObject => Unit): Unit = {
  println(s"Patching $file ...")
  val ast = readJson(file)
  val data = ast.at("/Exports/0/Table/Data").asInstanceOf[ArrayNode]
  for (i <- 0 until data.size) {
    f(UAssetObject(data.get(i)))
  }
  writeJson(file, ast)
  println()
}

def generateMod(): Unit = {
  val output = workingDir / "out"
  val modDir = output / argName

  def unpackJson(name: String): os.Path = {
    val sbDir = output / "SB"
    val sbOrigDir = output / "SB.orig"
    val uasset = sbDir / "Content" / "Local" / "Data" / s"$name.uasset"
    val uexp = s"$name.uexp"
    val json = s"$name.json"
    val r = output / "JSON" / json
    os.makeDir.all(r / os.up)

    println(s"Extracting $uasset ...")
    os.proc(retocExe, "to-legacy", "--no-parallel", "--version", ueVersionCode, "--filter", uasset.last, sbPakDir, output).call(cwd = workingDir)
    for (p <- os.walk(sbDir) if os.isFile(p) && p.last != uasset.last && p.last != uexp) os.remove(p)
    println()

    println(s"Converting to $r ...")
    os.proc(uassetGuiExe, "tojson", uasset, r, s"VER_$ueVersionCode", sbMap).call(cwd = workingDir)
    os.copy(sbDir, sbOrigDir, mergeFolders = true)
    os.remove.all(sbDir)
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
    os.proc("tar", "-acf", s"..\\${zip.last}", argName).call(cwd = output)
    println()

    zip
  }

  os.remove.all(output)
  os.makeDir.all(modDir)

  val uassetPatches = TreeMap(
    // add more/change to uasset patching of interest here, e.g.,
    //"TargetFilterTable" -> patchTargetFilter _,
    "EffectTable" -> patchEffect _
  )

  val jsonMap = Map.empty[String, os.Path] ++ (for (uassetName <- uassetPatches.keys) yield (uassetName, unpackJson(uassetName)))

  for ((table, f) <- uassetPatches) patchTable(jsonMap(table), f)

  for ((name, path) <- jsonMap) packJson(name, path)

  packMod()

  // comment out the following line to keep intermediate JSON, .uasset, .uexp, .utoc, .ucas, and .pak files
  os.remove.all(output)
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
  val entryPathPrefix = """@ [0,"Rows","""
  var map = TreeMap.empty[String, TreeMap[String, String]]
  for (Array(entryPath, _, valueText) <- os.read(path).trim.split(Array('\r', '\n')).map(_.trim).grouped(3)) {
    val Array(name, property) = entryPath.substring(entryPathPrefix.length, entryPath.length - 1).split(',').map(_.trim)
    var value = valueText.substring(2).trim
    if (value.contains("::")) value = "\"" + value.substring(value.lastIndexOf("::") + 2)
    if (value(0) != '"' && (value.contains('.') && value.toIntOption.isEmpty)) value = value + "d"
    map = map + (name -> (map.getOrElse(name, TreeMap.empty[String, String]) + (property -> value)))
  }

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
      for ((property, value) <- properties) lines = lines :+ s"    case $name => obj.set($property, $value)"
    } else {
      lines = lines :+ s"    case $name =>"
      for ((property, value) <- properties) lines = lines :+ s"      obj.set($property, $value)"
    }
    lines = lines :+ ""
  }
  lines = lines :+ "    case _ =>"
  lines = lines :+ "  }"
  lines = lines :+ "}"
  println(lines.mkString(util.Properties.lineSeparator))
}

setupModTools()

argName match {
  case ".code" => code(argPath)

  case ".setup" => // skip

  case _ =>

    if (!os.isDir(sbPakDir)) exit(-1, s"$sbPakDir directory does not exist")

    println(header)
    println(s"* Game directory: $argPath")
    println(s"* Mod name to generate: $argName")
    println(s"* Working directory: $workingDir")
    println(s"* Using: retoc v$retocVersion, UAssetGUI v$uassetGuiVersion, $sbMapFilename")
    println(s"* Extra: FModel @$fmodelShortSha, jd v$jdVersion")
    println()

    setUAssetGUIConfigAndRun(generateMod _)
}
