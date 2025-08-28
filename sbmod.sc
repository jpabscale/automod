//> using scala 2.13.16
//> using dep com.fasterxml.jackson.core:jackson-databind:2.19.1
//> using dep com.lihaoyi::os-lib:0.11.4

import com.fasterxml.jackson.core.util.{DefaultIndenter, DefaultPrettyPrinter}
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.fasterxml.jackson.databind.node.{ArrayNode, ObjectNode}

def exit(code: Int): Nothing = {
  System.exit(code)
  throw new RuntimeException
}

if (!scala.util.Properties.isWin) {
  println("This script can only be used in Windows")
  exit(-1)
}

if (args.length != 2) {
  println("Usage: scala-cli sbmod.sc -- <mod-name> <absolute>\\<path>\\<of>\\<stellar-blade-game-dir>")
  exit(0)
}

val modName = args.head
val sbDir = os.Path(args.last)
val sbPakDir = sbDir / "SB" / "Content" / "Paks"

val retocVersion = "0.1.2"
val uassetGuiVersion = "1.0.3"
val sbMapVersion = "1.1.0"
val ueVersion = "UE4_26"

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
    println(s"Please wait while setting up retoc $retocVersion in $workingDir ...")
    os.proc("curl", "-JLO", retocUrl).call(cwd = workingDir)
    os.proc("tar", "-xf", retocZip).call(cwd = workingDir)
    os.remove.all(workingDir / retocZip)
    os.move(workingDir / "LICENSE", workingDir / "retoc-LICENSE")
    os.move(workingDir / "README.md", workingDir / "retoc-README.md")
    println()
  }

  if (!os.exists(uassetGuiExe)) {
    println(s"Please wait while setting up UAssetGUI $uassetGuiVersion in $workingDir ...")
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

case class RichObjectNode(value: JsonNode) {
  def obj(name: String): ObjectNode = {
    val values = value.get("Value").asInstanceOf[ArrayNode]
    for (j <- 0 until values.size) {
      val element = values.get(j).asInstanceOf[ObjectNode]
      if (element.get("Name").asText == name) {
        return element
      }
    }
    println(s"Could not find `$name` in ${value.get("Name")}")
    exit(-1)
    return null
  }
  def setValue(name: String, value: Int): Int = {
    val sub = obj(name)
    val r = sub.get("Value").asInt
    sub.put("Value", value)
    r
  }
  def setValue(name: String, value: Double): Double = {
    val sub = obj(name)
    val r = sub.get("Value").asDouble
    sub.put("Value", value)
    r
  }
  def setValue(name: String, value: String): String = {
    val sub = obj(name)
    val r = sub.get("Value").asText
    sub.put("Value", value)
    r
  }
  def getName: String = value.get("Name").asText
}

def writeJson(path: os.Path, node: JsonNode): Unit = {
  val indenter = new DefaultIndenter("  ", DefaultIndenter.SYS_LF)
  val printer = new DefaultPrettyPrinter
  printer.indentObjectsWith(indenter)
  printer.indentArraysWith(indenter)
  os.remove.all(path)
  new ObjectMapper().writer(printer).writeValue(path.toIO, node)
}

def patchEffectTableJson(file: os.Path): Unit = {
  println(s"Patching $file ...")
  val ast = new ObjectMapper().readTree(file.toIO)
  val data = ast.at("/Exports/0/Table/Data").asInstanceOf[ArrayNode]
  for (i <- 0 until data.size) {
    val obj = RichObjectNode(data.get(i))
    val name = obj.getName
    name match {

      // based on https://www.nexusmods.com/stellarblade/mods/802
      case "N_Drone_Scan" =>
        obj.setValue("LifeTime", 30d)

      // based on https://www.nexusmods.com/stellarblade/mods/897
      case _ if name.startsWith("P_Eve_SkillTree_Just") && (name.contains("BetaGauge") || name.contains("BurstGauge")) =>
        name match {
          case "P_Eve_SkillTree_JustParry_BetaGauge1" => obj.setValue("CalculationMultipleValue", 8d)
          case "P_Eve_SkillTree_JustParry_BetaGauge2" => obj.setValue("CalculationMultipleValue", 6d)
          case "P_Eve_SkillTree_JustEvade_BurstGauge1" => obj.setValue("CalculationMultipleValue", 4d)
          case "P_Eve_SkillTree_JustEvade_BurstGauge2" => obj.setValue("CalculationMultipleValue", 3d)
          case _ => exit(-1)
        }
        obj.setValue("OverlapCount", 1)
        obj.setValue("LifeType", "EffectLifeType_IndependentTime")
        obj.setValue("LifeTime", 11d)
        obj.setValue("StartDelayTime", 1d)
        obj.setValue("LoopIntervalTime", 1d)
        obj.setValue("ActiveTargetFilterAlias", "Self")
        obj.setValue("LoopTargetFilterAlias", "Self")
        
      case _ =>
    }
  }
  writeJson(file, ast)
  println()
}


def generateMod(): Unit = {
  val output = workingDir / "output"
  def recreateOutput(): Unit = {
    if (os.exists(output)) {
      os.remove.all(output)
    }
    os.makeDir.all(output)
  }

  def unpackJson(name: String): os.Path = {
    val uasset = output / "SB" / "Content" / "Local" / "Data" / s"$name.uasset"
    val uexp = s"$name.uexp"
    val json = s"$name.json"
    val r = workingDir / json

    recreateOutput()

    println(s"Extracting $uasset ...")
    os.proc(retocExe, "to-legacy", "--no-parallel", "--version", ueVersion, "--filter", uasset.last, sbPakDir, output).call(cwd = workingDir)
    for (p <- os.walk(output) if os.isFile(p) && p.last != uasset.last && p.last != uexp) {
      os.remove(p)
    }
    println()

    println(s"Converting to $r ...")
    os.proc(uassetGuiExe, "tojson", uasset, json, s"VER_$ueVersion", sbMap).call(cwd = workingDir)
    os.remove.all(output)
    println()

    r
  }

  def packJson(name: String, path: os.Path): Unit = {
    val dataDir = output / "SB" / "Content" / "Local" / "Data"
    os.makeDir.all(dataDir)
    val uasset = dataDir / s"$name.uasset"
    println(s"Regenerating $uasset ...")
    os.proc(uassetGuiExe, "fromjson", path, dataDir / s"$name.uasset", sbMap).call(cwd = workingDir)
    println()
  }

  def packMod(dir: os.Path): os.Path = {
    val zip = workingDir / s"$modName.zip"
    os.remove.all(zip)

    val utoc = dir / s"${modName}_P.utoc"
    println(s"Converting to $utoc ...")
    os.proc(retocExe, "to-zen", "--no-parallel", "--version", ueVersion, output, utoc).call(cwd = workingDir)
    println()

    println(s"Archiving $zip ...")
    os.proc("tar", "-acf", zip.last, modName).call(cwd = workingDir)
    println()

    zip
  }

  val effectTable = "EffectTable"
  val uassetNames = Vector(effectTable)

  val jsonMap = Map.empty[String, os.Path] ++ (for (uassetName <- uassetNames) yield (uassetName, unpackJson(uassetName)))

  patchEffectTableJson(jsonMap(effectTable))

  recreateOutput()

  for ((name, path) <- jsonMap) packJson(name, path)

  val modDir = workingDir / modName
  if (os.exists(modDir)) {
    os.remove.all(modDir)
  }
  os.makeDir(modDir)

  packMod(modDir)

  os.remove.all(output)
  os.remove.all(modDir)
  for (path <- jsonMap.values) os.remove(path)
}

def setUAssetGUIConfigAndRun(f: () => Unit): Unit = {
  val oldConfigOpt = if (os.exists(uassetGuiConfig)) Some(os.read(uassetGuiConfig)) else None
  try {
    os.write.over(uassetGuiConfig,
      s"""{
         |  "PreferredVersion": "4.26",
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

if (!os.isDir(sbPakDir)) {
  println(s"$sbPakDir directory does not exist")
  exit(-1)
}

println(s"Stellar Blade game directory: $sbDir")
println(s"Mod name to generate: $modName")
println(s"Working directory: $workingDir")
println(s"Using: retoc v$retocVersion, UAssetGUI v$uassetGuiVersion, $sbMapFilename")
println()

setupModTools()
setUAssetGUIConfigAndRun(generateMod _)