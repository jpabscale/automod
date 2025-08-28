//> using scala 2.13.16
//> using dep com.fasterxml.jackson.core:jackson-databind:2.19.1
//> using dep com.lihaoyi::os-lib:0.11.4

import com.fasterxml.jackson.core.util.{DefaultIndenter, DefaultPrettyPrinter}
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.fasterxml.jackson.databind.node.{ArrayNode, DoubleNode, IntNode, ObjectNode, TextNode}

def exit(code: Int, msg: String = null): Nothing = {
  Option(msg).foreach(println(_))
  System.exit(code)
  throw new RuntimeException
}

if (!scala.util.Properties.isWin) exit(-1, "This script can only be used in Windows")

if (args.length != 2) exit(0, "Usage: scala-cli sbmod.sc -- <mod-name> <absolute-path-to-StellarBlade>")

val modName = args.head
val sbDir = os.Path(args.last)
val sbPakDir = sbDir / "SB" / "Content" / "Paks"

val retocVersion = "0.1.2"
val uassetGuiVersion = "1.0.3"
val sbMapVersion = "1.1.0"
val ueVersion = "4.26"
val ueVersionCode = s"UE${ueVersion.replace('.', '_')}"

val retocZip = "retoc-x86_64-pc-windows-msvc.zip"
val sbMap = s"StellarBlade_$sbMapVersion"
val sbMapFilename = s"$sbMap.usmap"

val retocUrl = s"https://github.com/trumank/retoc/releases/download/v$retocVersion/$retocZip"
val uassetGuiUrl = s"https://github.com/atenfyr/UAssetGUI/releases/download/v$uassetGuiVersion/UAssetGUI.exe"
val sbMapUrl = s"https://github.com/Stellar-Blade-Modding-Team/Stellar-Blade-Modding-Guide/raw/0eab1b4d7c1b88dea72298f60f6bb871682d3d1f/$sbMapFilename"

val workingDir = os.pwd
val retocExe = workingDir / "retoc.exe"
val uassetGuiExe = workingDir / "UAssetGUI.exe"
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

  if (!os.exists(uassetGuiMappingsDir / sbMapFilename)) {
    println(s"Please wait while setting up $sbMapFilename in $uassetGuiMappingsDir ...")
    os.makeDir.all(uassetGuiMappingsDir)
    os.proc("curl", "-JLO", sbMapUrl).call(cwd = uassetGuiMappingsDir)
    println()
  }
}

def recreateDir(dir: os.Path): Unit = {
  os.remove.all(dir)
  os.makeDir.all(dir)
}

def readJson(path: os.Path): JsonNode = new ObjectMapper().readTree(path.toIO)

def writeJson(path: os.Path, node: JsonNode): Unit = {
  val indenter = new DefaultIndenter("  ", DefaultIndenter.SYS_LF)
  val printer = new DefaultPrettyPrinter
  printer.indentObjectsWith(indenter)
  printer.indentArraysWith(indenter)
  os.remove.all(path)
  new ObjectMapper().writer(printer).writeValue(path.toIO, node)
}

case class RichObjectNode(value: JsonNode) {
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
  def setJson(name: String, value: JsonNode): JsonNode = obj(name).replace("Value", value)
  def set(name: String, value: Int): Int = setJson(name, IntNode.valueOf(value)).asInt
  def set(name: String, value: Double): Double = setJson(name, DoubleNode.valueOf(value)).asDouble
  def set(name: String, value: String): String = setJson(name, TextNode.valueOf(value)).asText
  def getName: String = value.get("Name").asText
  def getJson(name: String): JsonNode = obj(name).get("Value")
  def getInt(name: String): Int = getJson(name).asInt
  def getDouble(name: String): Double = getJson(name).asDouble
  def getString(name: String): String = getJson(name).asText
}

def patchEffectTable(file: os.Path): Unit = {
  println(s"Patching $file ...")
  val ast = readJson(file)
  val data = ast.at("/Exports/0/Table/Data").asInstanceOf[ArrayNode]
  for (i <- 0 until data.size) {
    val obj = RichObjectNode(data.get(i))
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
  writeJson(file, ast)
  println()
}

def generateMod(): Unit = {
  val modDir = workingDir / modName
  val output = workingDir / "output"

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
    val zip = workingDir / s"$modName.zip"
    os.remove.all(zip)

    val utoc = modDir / s"${modName}_P.utoc"
    println(s"Converting to $utoc ...")
    os.proc(retocExe, "to-zen", "--no-parallel", "--version", ueVersionCode, output, utoc).call(cwd = workingDir)
    println()

    println(s"Archiving $zip ...")
    os.proc("tar", "-acf", zip.last, modName).call(cwd = workingDir)
    println()

    zip
  }

  recreateDir(modDir)

  val effectTable = "EffectTable"
  val uassetNames = Vector(effectTable) // add more/change to uassets of interest here

  val jsonMap = Map.empty[String, os.Path] ++ (for (uassetName <- uassetNames) yield (uassetName, unpackJson(uassetName)))

  patchEffectTable(jsonMap(effectTable))
  // ... add more patching code for a different uasset (as JSON) file here

  recreateDir(output)
  for ((name, path) <- jsonMap) packJson(name, path)

  packMod()

  // comment out the following three lines to keep intermediate JSON, .uasset, .uexp, .utoc, .ucas, and .pak files
  os.remove.all(output)
  os.remove.all(modDir)
  for (path <- jsonMap.values) os.remove.all(path)
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

if (!os.isDir(sbPakDir)) exit(-1, s"$sbPakDir directory does not exist")

println(s"Stellar Blade game directory: $sbDir")
println(s"Mod name to generate: $modName")
println(s"Working directory: $workingDir")
println(s"Using: retoc v$retocVersion, UAssetGUI v$uassetGuiVersion, $sbMapFilename")
println()

setupModTools()
setUAssetGUIConfigAndRun(generateMod _)