import com.fasterxml.jackson.core.util.{DefaultIndenter, DefaultPrettyPrinter}
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper, ObjectWriter}
import com.fasterxml.jackson.databind.node.{JsonNodeFactory, ArrayNode, BooleanNode, DoubleNode, IntNode, NullNode, ObjectNode, TextNode}
import com.fasterxml.jackson.dataformat.toml.TomlMapper
import com.fasterxml.jackson.core.`type`.TypeReference
import com.jayway.jsonpath
import java.util.{Map => JMap}
import scala.beans.BeanProperty
import scala.collection.immutable.{TreeMap, TreeSet}
import scala.collection.mutable.HashMap
import scala.jdk.CollectionConverters._
import scala.util.Properties

val header = s"Auto Modding Script v2.4.5"

val dataTablePath = "/Exports/0/Table/Data"
val noCodePatching = "--no-code-patching"
val dryRun = "--dry-run"

var cliArgs = args

def exit(code: Int, msg: String = null): Nothing = {
  Option(msg).foreach((if (code == 0) Console.out else Console.err).println(_))
  System.exit(code)
  throw new RuntimeException
}

if (!Properties.isWin) exit(-1, "This script can only be used in Windows")

val zipToolVersion = "25.01"
val modExt = "zip"

class Game {
  @BeanProperty var contentPaks: String = "SB/Content/Paks"
  @BeanProperty var unrealEngine: String = "4.26"
  @BeanProperty var mapUrl: String = "https://github.com/Stellar-Blade-Modding-Team/Stellar-Blade-Modding-Guide/raw/0eab1b4d7c1b88dea72298f60f6bb871682d3d1f/StellarBlade_1.1.0.usmap"
  @BeanProperty var aesKey: String = ""
}

class Tools {
  @BeanProperty var retoc: String = "0.1.2"
  @BeanProperty var uassetGui: String = "1.0.3"
  @BeanProperty var fmodel: String = "0760e1105806670163796ccb41110740e98089d5"
  @BeanProperty var jd: String = "2.2.3"
}

class Config {
  @BeanProperty var game: Game = new Game
  @BeanProperty var tools: Tools = new Tools
}

final case class ValuePair(newValueOpt: Option[JsonNode], oldValueOpt: Option[JsonNode])
final case class OrderedString(value: String, path: String, order: Int = OrderedString.claimOrder()) extends Comparable[OrderedString] {
  override def compareTo(o: OrderedString): Int = {
    val r = order.compareTo(o.order)
    if (r == 0) value.compareTo(o.value) else r
  }
  override def hashCode: Int = value.hashCode
  override def equals(other: Any): Boolean = other match {
    case other: OrderedString => compareTo(other) == 0
    case _ => false
  }
  override def toString: String = if (order == 0) value else s"$value ($order)"
}
object OrderedString {
  private var currentOrder: Int = 1
  def claimOrder(): Int = {
    val r = currentOrder
    currentOrder += 1
    r
  }
}
type PropertyChanges = TreeMap[String, ValuePair]
type UAssetPropertyChanges = TreeMap[String, PropertyChanges]
type DataTableFilePatches = TreeMap[OrderedString, UAssetPropertyChanges]
type DataTableCodePatches = TreeMap[String, uassetapi.Struct => Unit]
type JsonAst = com.jayway.jsonpath.DocumentContext

val jp: jsonpath.ParseContext = {
  jsonpath.Configuration.setDefaults(new jsonpath.Configuration.Defaults {
    override val options = java.util.EnumSet.of(jsonpath.Option.ALWAYS_RETURN_LIST) 
    override val jsonProvider = new jsonpath.spi.json.JacksonJsonNodeJsonProvider
    override val mappingProvider = new jsonpath.spi.mapper.JacksonMappingProvider
  })
  jsonpath.JsonPath.using(jsonpath.Configuration.defaultConfiguration)
}

object builder {
  
  type PropertyUpdates = TreeMap[String, JsonNode]
  type UAssetUpdates = TreeMap[String, PropertyUpdates]
  type DataTableUpdates = TreeMap[String, UAssetUpdates]

  type JsonsOrJsonEntries = Either[Iterable[JsonNode], Iterable[(String, JsonNode)]]
  implicit def jsons(o: Iterable[JsonNode]): JsonsOrJsonEntries = Left(o)
  implicit def jsonEntries(o: Iterable[(String, JsonNode)]): JsonsOrJsonEntries = Right(o)

  implicit class StringImplicit(val key: String) extends AnyVal {
    def ~>(objChanges: UAssetUpdates): DataTableUpdates = {
      var r: DataTableUpdates = TreeMap.empty
      r = r + (key -> (r.getOrElse(key, TreeMap.empty: UAssetUpdates) ++ objChanges))
      r
    }

    def +>(entry: Iterable[(String, JsonNode)]): UAssetUpdates = {
      var r: UAssetUpdates = TreeMap.empty
      r = r + (key -> (r.getOrElse(key, TreeMap.empty: PropertyUpdates) ++ entry))
      r
    }
  }

  implicit class UAssetUpdatesImplicit(val o: UAssetUpdates) extends AnyVal {
    def +(other: UAssetUpdates): UAssetUpdates = o ++ other
  }

  implicit class DataTableUpdatesImplicit(val o: DataTableUpdates) extends AnyVal {
    def +(other: DataTableUpdates): DataTableUpdates = o ++ other
  }

  implicit def boolean2json(v: Boolean): JsonNode = BooleanNode.valueOf(v)
  implicit def int2json(v: Int): JsonNode = DoubleNode.valueOf(v)
  implicit def float2json(v: Float): JsonNode = DoubleNode.valueOf(v)
  implicit def double2json(v: Double): JsonNode = DoubleNode.valueOf(v)
  implicit def string2json(v: String): JsonNode = TextNode.valueOf(v)
  implicit def iterable2json(o: JsonsOrJsonEntries): JsonNode = o match {
    case Left(v) => JsonNodeFactory.instance.arrayNode.addAll(v.asInstanceOf[Iterable[JsonNode]].asJavaCollection)
    case Right(v) => JsonNodeFactory.instance.objectNode.setAll((TreeMap.empty[String, JsonNode] ++ v).asJava)
  }
}

def objectWriter: ObjectWriter = {
  val indenter = new DefaultIndenter("  ", DefaultIndenter.SYS_LF)
  val printer = new DefaultPrettyPrinter
  printer.indentObjectsWith(indenter)
  printer.indentArraysWith(indenter)
  new ObjectMapper().writer(printer)
}

def writeConfig(config: Config): Unit = {
  objectWriter.writeValue(configPath.toIO, config)
  println(s"Wrote $configPath")
  println()
}

def getTimestamp(): String = {
  import java.time.{ZonedDateTime, ZoneOffset}
  import java.time.format.DateTimeFormatter
  DateTimeFormatter.ISO_INSTANT.format(ZonedDateTime.now(ZoneOffset.UTC)).replace(":", "-").replace(".", "-")
}

def absPath(p: os.Path): String = p.toIO.getCanonicalFile.getAbsolutePath
def absPath(p: String): os.Path = os.Path(new java.io.File(p).getCanonicalFile.getAbsolutePath)

val workingDir = os.pwd
val configPath = workingDir / ".config.json"
val cacheDir = workingDir / ".cache"

val config = {
  var r = new Config
  var ok = true
  if (os.exists(configPath)) try {
    r = new ObjectMapper().readValue(configPath.toIO, classOf[Config])
  } catch {
    case _: Throwable => 
      ok = false
  } finally {
    if (!ok && os.exists(configPath)) {
      val backup = configPath / os.up / s".config-${getTimestamp()}.json"
      os.move.over(configPath, backup)
      println(s"Could not load $configPath; backed up to ${absPath(backup)}")
    }
  }
  if (!os.exists(configPath)) writeConfig(r)
  r
}

val retocVersion = config.tools.retoc
val uassetGuiVersion = config.tools.uassetGui
val fmodelSha = config.tools.fmodel
val fmodelShortSha = fmodelSha.substring(0, 7)
val jdVersion = config.tools.jd
val ueVersion = config.game.unrealEngine
val ueVersionCode = s"UE${ueVersion.replace('.', '_')}"

val retocZip = "retoc-x86_64-pc-windows-msvc.zip"

val retocUrl = s"https://github.com/trumank/retoc/releases/download/v$retocVersion/$retocZip"
val uassetGuiUrl = s"https://github.com/atenfyr/UAssetGUI/releases/download/v$uassetGuiVersion/UAssetGUI.exe"
val usmapUrl = config.game.mapUrl
val fmodelUrl = s"https://github.com/4sval/FModel/releases/download/qa/$fmodelSha.zip"
val jdUrl = s"https://github.com/josephburnett/jd/releases/download/v$jdVersion/jd-amd64-windows.exe"
val usmapFilename = usmapUrl.substring(usmapUrl.lastIndexOf('/') + 1)
val usmapPath = workingDir / usmapFilename
val usmap = usmapPath.baseName
val z7rUrl = s"https://github.com/ip7z/7zip/releases/download/$zipToolVersion/7zr.exe"
val z7Url = s"https://github.com/ip7z/7zip/releases/download/$zipToolVersion/7z${zipToolVersion.replace(".", "")}.exe"

val toolsDir = workingDir / "tools"

val retocExe = toolsDir / "retoc.exe"
val uassetGuiExe = toolsDir / "UAssetGUI.exe"
val fmodelExe = toolsDir / "FModel.exe"
val jdExe = toolsDir / "jd.exe"
val uassetGuiSettingsDir = os.Path(System.getenv("LOCALAPPDATA")) / "UAssetGUI"
val uassetGuiConfig = uassetGuiSettingsDir / "config.json"
val uassetGuiMappingsDir = uassetGuiSettingsDir / "Mappings"
val zipExe = toolsDir / "7z" / "7z.exe"

val automod = workingDir / "automod.bat"

def setupModTools(): Boolean = {
  var setup = true
  
  os.makeDir.all(toolsDir)

  def download(url: String, renameOpt: Option[String] = None): Unit = {
    val cacheName = java.util.Base64.getEncoder().encodeToString(url.getBytes(java.nio.charset.StandardCharsets.UTF_8))
    val cachePath = os.Path(System.getenv("LOCALAPPDATA")) / "Temp" / "automod" / cacheName
    val dest = renameOpt match {
      case Some(name) => toolsDir / name
      case _ => toolsDir / url.substring(url.lastIndexOf('/') + 1)
    }
    if (!os.exists(cachePath)) {
      os.makeDir.all(cachePath / os.up)
      os.proc("curl", "-JLo", cachePath, url).call(cwd = toolsDir, stdout = os.Inherit, stderr = os.Inherit)
    }
    os.copy.over(cachePath, dest)
  }

  if (!os.exists(zipExe)) {
    setup = false
    println(s"Setting up 7z v$zipToolVersion in $toolsDir ...")
    val z7 = s"7z$zipToolVersion.exe"
    val z7r = "7zr.exe"
    download(z7rUrl, Some(z7r))
    download(z7Url, Some(z7))
    os.makeDir.all(toolsDir / "7z")
    os.proc(toolsDir / "7zr.exe", "x", toolsDir / z7).call(cwd = toolsDir / "7z")
    os.remove.all(toolsDir / z7)
    os.remove.all(toolsDir / z7r)
    println()
  }

  if (!os.exists(retocExe)) {
    setup = false
    println(s"Setting up retoc v$retocVersion in $toolsDir ...")
    download(retocUrl)
    os.proc(zipExe, "x", retocZip).call(cwd = toolsDir)
    os.remove.all(toolsDir / retocZip)
    os.move(toolsDir / "LICENSE", toolsDir / "retoc-LICENSE")
    os.move(toolsDir / "README.md", toolsDir / "retoc-README.md")
    println()
  }

  if (!os.exists(uassetGuiExe)) {
    setup = false
    println(s"Setting up UAssetGUI v$uassetGuiVersion in $toolsDir ...")
    download(uassetGuiUrl)
    println()
  }

  if (!os.exists(toolsDir / usmapFilename)) {
    setup = false
    println(s"Setting up $usmapFilename in $toolsDir ...")
    download(usmapUrl)
    println()

    val src = toolsDir / usmapFilename
    val dest = uassetGuiMappingsDir / usmapFilename
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
    println(s"Setting up FModel @$fmodelShortSha in $toolsDir ...")
    download(fmodelUrl, Some(fmodelZip))
    os.proc(zipExe, "x", fmodelZip).call(cwd = toolsDir)
    os.remove.all(toolsDir / fmodelZip)
    println()
  }

  if (!os.exists(jdExe)) {
    setup = false
    println(s"Setting up jd v$jdVersion in $toolsDir ...")
    download(jdUrl, Some(jdExe.last))
    println()
  }

  if (!os.exists(automod)) {
    setup = false
    os.write(automod, s"@scala-cli --suppress-outdated-dependency-warning --server=false project.scala -- %*${Properties.lineSeparator}")
    println(s"Wrote $automod")
    println()
  }

  setup
}

def toJsonNode(content: String): JsonNode = new ObjectMapper().readTree(content)

def jdDataTableFilePatches(path: os.Path)(err: Vector[String] => Unit = 
  msgs => if (msgs.nonEmpty) exit(-1, 
    s"""Error when loading $path:
       |${msgs.mkString(Properties.lineSeparator)}""".stripMargin)): UAssetPropertyChanges = {
  val entryPathPrefix = """@ [0,"Rows","""
  var map: UAssetPropertyChanges = TreeMap.empty
  val lines = os.read(path).trim.replace("\r", "").split('\n').map(_.trim)
  var errors = Vector[String]()
  val grouped = {
    var r = Vector.empty[Vector[String]]
    var i = 0
    def unrecognized(): Unit = {
      errors = errors :+ s"* Unsupported patch form at line $i: ${lines(i)}"
      while (i < lines.length && lines(i).head != '@') i += 1
    }
    while (i < lines.length) {
      if (lines(i).startsWith(entryPathPrefix) && i + 1 < lines.length) {
        if (lines(i + 1).startsWith("- ")) {
          if (i + 2 < lines.length) {
            if (lines(i + 2).startsWith("+ ")) {
              r = r :+ Vector("-+", lines(i), lines(i + 1), lines(i + 2))
              i += 3
            } else {
              r = r :+ Vector("-", lines(i), lines(i + 1))
              i += 2
            }
          } else {
            r = r :+ Vector("-", lines(i), lines(i + 1))
            i += 2
          }
        } else if (lines(i + 1).startsWith("+ ")) {
          r = r :+ Vector("+", lines(i), lines(i + 1))
          i += 2
        } else {
          unrecognized()
          i += 2
        }
      } else {
        unrecognized()
      }
    }
    r
  }

  def diff(mode: String, entryPath: String, oldValueText: String, newValueTextOpt: Option[String]): Unit = {
    var ok = false
    if (entryPath.startsWith(entryPathPrefix)) {
      entryPath.substring(entryPathPrefix.length, entryPath.length - 1).split(',').map(_.trim) match {
        case Array(n, p) =>
          ok = true
          var name = n
          var property = p
          name = name.substring(1, name.length - 1)
          property = property.substring(1, property.length - 1)
          val oldValue = toJsonNode(oldValueText)
          newValueTextOpt match {
            case Some(newValueText) =>
              val value = newValueText
              val json = toJsonNode(value)
              if (json.isObject || json.isArray) {
                ok = false
              } else {
                map = map + (name -> (map.getOrElse(name, TreeMap.empty: PropertyChanges) +
                  (property -> ValuePair(Some(json), Some(oldValue)))))
              }
            case _ =>
              map = map + (name -> (map.getOrElse(name, TreeMap.empty: PropertyChanges) +
                (property -> ValuePair(None, Some(oldValue)))))
          }
        case Array(n) if mode == "-" =>
          ok = true
          var name = n
          name = name.substring(1, name.length - 1)
          val oldObj = toJsonNode(oldValueText).asInstanceOf[ObjectNode]
          var m: PropertyChanges = TreeMap.empty
          for (fieldName <- oldObj.fieldNames.asScala) {
            val oldValue = oldObj.get(fieldName)
            m = m + (fieldName -> ValuePair(None, Some(oldValue)))
          }
          map = map + (name -> m)
        case Array(n) if mode == "+" =>
          ok = true
          var name = n
          name = name.substring(1, name.length - 1)
          val newObjObj = toJsonNode(oldValueText).asInstanceOf[ObjectNode]
          var m: PropertyChanges = TreeMap.empty
          for (fieldName <- newObjObj.fieldNames.asScala) {
            val newValue = newObjObj.get(fieldName)
            m = m + (fieldName -> ValuePair(Some(newValue), None))
          }
          map = map + (name -> m)
        case _ =>
      }
    }
    if (!ok) println(s"The script currently does not handle the patch entry (skipped): $entryPath")
  }

  for (d <- grouped) {
    d match {
      case Vector(mode, entryPath, oldValueText) => diff(mode, entryPath, oldValueText.substring(2).trim, None)
      case Vector(mode, entryPath, oldValueText, newValueText) => diff(mode, entryPath, oldValueText.substring(2).trim,
        Some(newValueText.substring(2).trim))
      case _ =>
    }
  }

  if (errors.nonEmpty) err(errors)
  map
}

def tomlDataTableFilePatches(path: os.Path): UAssetPropertyChanges = {
  val toml: JMap[String, JMap[String, Object]] =
    new TomlMapper().readValue(path.toIO, new TypeReference[JMap[String, JMap[String, Object]]] {})
  var map: UAssetPropertyChanges = TreeMap.empty
  for (name <- toml.keySet.asScala) {
    var m: PropertyChanges = TreeMap.empty
    val properties = toml.get(name)
    for (property <- properties.keySet.asScala) {
      def add(value: JsonNode): Unit = m = m + (property -> ValuePair(Option(value), None))
      def valueOf(value: Object): JsonNode = value match {
        case value: java.lang.Boolean => BooleanNode.valueOf(value.booleanValue)
        case value: java.lang.Integer => IntNode.valueOf(value.intValue)
        case value: java.math.BigDecimal => DoubleNode.valueOf(value.doubleValue)
        case value: String => if (value == "null") NullNode.instance else TextNode.valueOf(value)
        case null => null
      }
      def rec(value: Object): JsonNode = {
        value match {
          case _: java.lang.Boolean | _: java.lang.Integer | _: java.math.BigDecimal | _: String => valueOf(value)
          case value: java.util.Map[_, _] =>
            val r = JsonNodeFactory.instance.objectNode
            for ((k, v) <- value.asScala) {
              r.set[JsonNode](k.toString, rec(v.asInstanceOf[Object]))
            }
            r
          case value: java.util.ArrayList[_] => 
            val r = JsonNodeFactory.instance.arrayNode
            for (e <- value.asScala) {
              r.add(rec(e.asInstanceOf[Object]))
            }
            r
          case _ => exit(-1, s"Unsupported property value form for $name/$property (${value.getClass}): $value")
        }
      }
      add(rec(properties.get(property)))
    }
    map = map + (name -> m)
  }
  patchlet.checkPatches(path.baseName, map)
}

var patchesInitialized = false
var _patches: DataTableFilePatches = TreeMap.empty

def applyChanges(path: String, map: DataTableFilePatches, uassetName: String, data: UAssetPropertyChanges): DataTableFilePatches = {
  val key = OrderedString(uassetName, path)
  var m = map.getOrElse(key, TreeMap.empty: UAssetPropertyChanges)
  for ((name, properties) <- data) {
    var m2 = m.getOrElse(name, TreeMap.empty: PropertyChanges)
    for ((property, valuePair) <- properties) {
      val valueString = toJsonPrettyString(valuePair.newValueOpt)
      val oldValueOpt = m2.get(property) match {
        case Some(v) =>
          println(s"* $name/$property: ${toJsonPrettyString(v.newValueOpt)} => $valueString")
          v.newValueOpt
        case _ =>
          println(s"* $name/$property: $valueString")
          None
      }
      m2 = m2 + (property -> ValuePair(valuePair.newValueOpt, oldValueOpt))
    }
    m = m + (name -> m2)
  }
  map + (key -> m)
}

def updatePatches(): Unit = {
  val patchesDir = workingDir / "patches"
  var map: DataTableFilePatches = if (_patches == null) TreeMap.empty else _patches

  def rec(path: os.Path): Unit =   {
    if (path.last.headOption == Some('.')) {
      println(s"Ignoring $path ...")
      println()
      return
    }
    for (p <- os.list(path).sortWith((p1, p2) =>
      if (os.isDir(p1) && os.isDir(p2)) p1.last <= p2.last
      else if (os.isDir(p1)) false
      else if (os.isDir(p2)) true
      else p1.last <= p2.last
    )) {
      if (os.isDir(p)) {
        rec(p)
      } else if (os.isFile(p)) {
        if (p.last.startsWith(".")) {
          println(s"Ignoring $path ...")
          println()
        } else {
          p.ext.toLowerCase match {
            case "patch" =>
              println(s"Loading $p ...")
              val uassetName = p.baseName
              map = applyChanges(p.relativeTo(patchesDir).toString, map, uassetName, jdDataTableFilePatches(p)())
              println()
            case "toml" =>
              println(s"Loading $p ...")
              val uassetName = p.baseName
              map = applyChanges(p.relativeTo(patchesDir).toString, map, uassetName, tomlDataTableFilePatches(p))
              println()
            case _ =>
          }
        }
      }
    }
  }

  if (os.exists(patchesDir)) rec(patchesDir)
  _patches = map
}

def patches: DataTableFilePatches = {
  if (patchesInitialized) return _patches
  patchesInitialized = true
  updatePatches()
  _patches
}

def writeJson(path: os.Path, node: JsonNode): Unit = objectWriter.writeValue(path.toIO, node)

def toJsonPrettyString(valueOpt: Option[JsonNode], default: String = ""): String =
  valueOpt.map(_.toString).getOrElse(default)

def toDataMap(data: ArrayNode): collection.Map[String, ObjectNode] = {
  val r = collection.mutable.HashMap.empty[String, ObjectNode]
  for (i <- 0 until data.size) {
    val o = data.get(i).asInstanceOf[ObjectNode]
    r.put(o.get("Name").asText, o)
  }
  r
}

def patchFromTree(addToDataTableFilePatches: Boolean, uassetName: String, ast: JsonAst, origAst: JsonAst, data: ArrayNode, origData: ArrayNode)(tree: UAssetPropertyChanges): Unit = {
  val (kfcMap, atKfcMap, t) = patchlet.kfcMap(addToDataTableFilePatches, uassetName, ast, origAst, data, tree)
  for (kfc <- atKfcMap.values) {
    kfc.applyChanges(ast)
  }
  val seenObjectNames = scala.collection.mutable.HashSet.empty[String]
  val dataMap = toDataMap(data)
  for (i <- 0 until data.size) {
    val orig = origData.get(i)
    val obj = uassetapi.Struct(uassetName, data.get(i), addToDataTableFilePatches = addToDataTableFilePatches)
    val origObj = uassetapi.Struct(uassetName, orig, addToDataTableFilePatches = false)
    seenObjectNames.add(obj.name)
    for (kfc <- kfcMap.values if kfc(obj.name)) {
      kfc.applyPathChanges(s"$dataTablePath/$i", obj.value, origObj.value)
    }
    t.get(obj.name) match {
      case Some(properties) =>
        for ((property, valueOldValuePair) <- properties) {
          var value = valueOldValuePair.newValueOpt.orNull
          value match {
            case v: TextNode if patchlet.getKeyPrefix(v.textValue) == Some(patchlet.Constants.codePrefix) =>
              val code = v.textValue.substring(patchlet.Constants.codePrefix.length)
              value = patchlet.evalProperty(uassetName, addToDataTableFilePatches, dataMap, code, obj, property, orig)
            case _ =>
          }
          obj.setJson(property, value)
        }
      case _ =>
    }
  }
}

def patchDataTable(addToDataTableFilePatches: Boolean, name: String, file: os.Path, ast: JsonAst, 
                   origAst: JsonAst, fOpt: Option[uassetapi.Struct => Unit], disableCodePatching: Boolean): Unit = {
  val json = ast.json[JsonNode]
  if (disableCodePatching) {
    for ((uassetName, tree) <- patches if uassetName.order != 0 && uassetName.value == name) {
      println(s"Patching $file from ${uassetName.path} ...")
      val data = json.at(dataTablePath).asInstanceOf[ArrayNode]
      val origData = origAst.json[JsonNode].at(dataTablePath).asInstanceOf[ArrayNode]
      patchFromTree(addToDataTableFilePatches, name, ast, origAst, data, origData)(tree)
      writeJson(file, json)
      println()
    }
  } else {
    val data = json.at(dataTablePath).asInstanceOf[ArrayNode]
    val origData = origAst.json[JsonNode].at(dataTablePath).asInstanceOf[ArrayNode]
    for (i <- 0 until data.size) {
      fOpt.foreach(_(uassetapi.Struct(name, data.get(i), addToDataTableFilePatches)))
    }
    for ((uassetName, tree) <- patches if uassetName.order != 0 && uassetName.value == name) {
      println(s"Patching $file from ${uassetName.path} ...")
      patchFromTree(addToDataTableFilePatches, name, ast, origAst, data, origData)(tree)
    }
    writeJson(file, json)
    println()
  }
}

def generateMod(addToDataTableFilePatches: Boolean,
                modNameOpt: Option[String], 
                sbPakDir: os.Path, 
                disableFilePatching: Boolean, 
                disableCodePatching: Boolean, 
                dryRun: Boolean,
                currentAstMap: collection.mutable.HashMap[String, JsonAst] = collection.mutable.HashMap.empty,
                origAstMap: collection.mutable.HashMap[String, JsonNode] = null,
                uassetNameRequests: Vector[String] = Vector())(): Unit = {
  val cacheKey = cacheDir / "key.properties"
  val output = workingDir / "out"
  os.remove.all(output)

  def computeCacheKey(): String = {
    if (!os.exists(sbPakDir)) return ""
    var r = Vector.empty[String]
    r = r :+ s"retoc=$retocVersion"
    r = r :+ s"UAssetGUI=$uassetGuiVersion"
    for (p <- os.list(sbPakDir).sortWith((p1, p2) => p1.last <= p2.last) if os.isFile(p)) {
      r = r :+ s"${p.last}=${p.toIO.lastModified}"
    }
    r.mkString(Properties.lineSeparator)
  }

  def recreateDir(dir: os.Path): Unit = {
    os.remove.all(dir)
    os.makeDir.all(dir)
  }

  val cwd = workingDir / ".temp"
  recreateDir(cwd)
  val modDirOpt = modNameOpt.map(cwd / _)

  val cacheHit = {
    val key = computeCacheKey()
    if (os.exists(cacheKey) && os.read(cacheKey) == key) true else {
      recreateDir(cacheDir)
      if (key.nonEmpty) os.write(cacheKey, key)
      false
    }
  }

  def retoc(args: os.Shellable*): Vector[os.Shellable] = {
    var r = Vector[os.Shellable](retocExe)
    if (config.game.aesKey.nonEmpty) {
      r = r :+ "--aes-key"
      r = r :+ s"0x${config.game.aesKey}"
    }
    r = r ++ args
    r
  }

  def retocFailed(title: String, pRetoc: os.proc, at: os.Path): Nothing = exit(-1, 
    s"""Failed to use retoc to $title with the following command in $at:
       |
       |${pRetoc.commandChunks.mkString(" ")}
       |
       |Try to see if this is a known issue (or filing a new one) at:
       |https://github.com/trumank/retoc/issues""".stripMargin)

  def uassetGuiFailed(title: String, pUassetGui: os.proc, at: os.Path, repack: Boolean): Nothing = {
    val moreInfo = if (!repack) "T" else 
      s"""First, check that the patched JSON file has been changed as intended with correct values. 
         |If everyhing looks proper, t""".stripMargin
    exit(-1, 
      s"""Failed to use UAssetGUI to $title with the following command in $at:
         |
         |${pUassetGui.commandChunks.mkString(" ")}
         |
         |${moreInfo}ry to see if this is a known UAssetGUI issue (or filing a new one) at:
         |https://github.com/atenfyr/UAssetAPI/issues, or
         |https://github.com/atenfyr/UAssetGUI/issues""".stripMargin)
  }

  def unpackDataTableJson(name: String): os.Path = {
    val json = s"$name.json"
    val r = cwd / json
    val jsonCache = cacheDir / json

    if (cacheHit && os.exists(jsonCache)) {
      os.copy.over(jsonCache, r)
      println(s"Using cached $jsonCache")
      println()
      return r
    }

    val uasset = output / os.RelPath(config.game.contentPaks).segments.head / "Content" / "Local" / "Data" / s"$name.uasset"
    val uexp = s"$name.uexp"

    recreateDir(output)

    println(s"Extracting $uasset ...")
    val pRetoc = os.proc(retoc("to-legacy", "--no-parallel", "--version", ueVersionCode, "--filter", uasset.last, sbPakDir, output))
    if (pRetoc.call(check = false, cwd = cwd, stdout = os.Inherit, stderr = os.Inherit).exitCode != 0 || os.walk(output).isEmpty)
      retocFailed(s"extract ${uasset.last} (double check the .uasset name)", pRetoc, cwd)
    
    for (p <- os.walk(output) if os.isFile(p) && p.last != uasset.last && p.last != uexp) os.remove(p)
    println()

    println(s"Converting to $r ...")
    val pUassetGui = os.proc(uassetGuiExe, "tojson", uasset, json, s"VER_$ueVersionCode", usmap)
    if (pUassetGui.call(check = false, cwd = cwd, stdout = os.Inherit, stderr = os.Inherit).exitCode != 0) 
      uassetGuiFailed(s"convert $uasset to JSON", pUassetGui, cwd, repack = false)
    os.remove.all(output)
    println()

    os.copy.over(r, jsonCache)
    r
  }

  def packDataTableJson(name: String, path: os.Path): Unit = {
    val dataDir = output / "SB" / "Content" / "Local" / "Data"
    val uasset = dataDir / s"$name.uasset"
    os.makeDir.all(dataDir)

    println(s"Regenerating $uasset ...")
    val pUassetGui = os.proc(uassetGuiExe, "fromjson", path, uasset, usmap)
    if (pUassetGui.call(check = false, cwd = cwd, stdout = os.Inherit, stderr = os.Inherit).exitCode != 0)
      uassetGuiFailed(s"convert $uasset from JSON", pUassetGui, cwd, repack = true)
    println()
  }

  def packMod(modName: String): os.Path = {
    val modDir = modDirOpt.get  
    if (os.exists(modDir)) {
      exit(-1, s"$modDir already exists")
    }

    val zip = workingDir / s"$modName.$modExt"
    os.remove.all(zip)

    os.makeDir.all(modDir)
    val utoc = modDir / s"${modName}_P.utoc"
    println(s"Converting to $utoc ...")
    val pRetoc = os.proc(retoc("to-zen", "--no-parallel", "--version", ueVersionCode, output, utoc))
    if (pRetoc.call(check = false, cwd = cwd, stdout = os.Inherit, stderr = os.Inherit).exitCode != 0)
      retocFailed(s"pack $modName", pRetoc, cwd)
    println()

    println(s"Archiving $zip ...")
    os.proc(zipExe, "a", s"-t$modExt", "-mtm-", zip, modName).call(cwd = cwd)
    println()

    zip
  }


  def isStellarBlade(game: Game): Boolean = {
    val default = new Game
    default.contentPaks == game.contentPaks && default.unrealEngine == game.unrealEngine && game.aesKey.isEmpty
  }

  var dataTableCodePatches: DataTableCodePatches = TreeMap.empty
  if (!disableCodePatching) {
    if (isStellarBlade(config.game)) dataTableCodePatches = dataTableCodePatches ++ patchSB.patches
  }

  val shouldPack = modNameOpt.nonEmpty
  var uassetNames = dataTableCodePatches.keySet ++ uassetNameRequests
  if (!disableFilePatching) {
    uassetNames = uassetNames ++ (for (key <- patches.keys) yield key.value)
  }
  if (!disableCodePatching) {
    uassetNames = uassetNames ++ patchCustom.uassetNames
  }
  val dataTableJsonMap = Map.empty[String, os.Path] ++ (for (uassetName <- uassetNames) yield (uassetName, unpackDataTableJson(uassetName)))

  if (disableCodePatching & disableFilePatching) return

  var messageOpt: Option[String] = None 
  var skippedUassets = TreeSet.empty[String]

  def skipUasset(uassetName: String): Unit = {
    skippedUassets = skippedUassets + uassetName
    if (messageOpt.isEmpty) {
      messageOpt = Some(
        s"""
           |This script requires additional advanced patching code to handle the skipped files.
           |You can insert your own patching code in patchCustom.sc.
           |""".stripMargin)
    }
  }

  for (uassetName <- uassetNames) {
    val file = dataTableJsonMap(uassetName)
    val ast = currentAstMap.get(uassetName) match {
      case Some(o) => 
        o
      case _ => 
        val o = jp.parse(file.toIO)
        currentAstMap.put(uassetName, o)
        o
    }
    val json = ast.json[JsonNode]
    lazy val origAst = {
      val jsonContext = classOf[jsonpath.internal.JsonContext].getDeclaredConstructors()(0)
      jsonContext.setAccessible(true)
      val r = jsonContext.newInstance(ast.json[JsonNode].deepCopy[JsonNode], 
         jsonpath.Configuration.defaultConfiguration).asInstanceOf[sbmod.JsonAst]
      jsonContext.setAccessible(false)
      r
    }
    if (origAstMap != null && !origAstMap.contains(uassetName)) origAstMap.put(uassetName, origAst.json[JsonNode])
    var custom = true
    if (!patchCustom.uassetNames.contains(uassetName)) {
      json.at(dataTablePath) match {
        case _: ArrayNode =>
          custom = false 
          if (!disableFilePatching) patchDataTable(addToDataTableFilePatches, uassetName, file, ast, origAst, dataTableCodePatches.get(uassetName), disableCodePatching)
        case _ =>
      }
    }
    def getAllPatches(uassetName: String): Seq[UAssetPropertyChanges] = {
      var r = Seq.empty[UAssetPropertyChanges]
      for ((name, changes) <- patches if name.value == uassetName) {
        r :+= changes
      } 
      r
    }
    if (custom && !patchCustom.patch(uassetName, json, getAllPatches(uassetName))) skipUasset(uassetName)
  }

  modNameOpt match {
    case Some(modName) if !dryRun =>
      for ((uassetName, path) <- dataTableJsonMap if !skippedUassets.contains(uassetName)) packDataTableJson(uassetName, path)
      if ((dataTableJsonMap.keySet -- skippedUassets).nonEmpty) packMod(modName)
    case _ =>
  }

  modDirOpt.foreach(os.remove.all)

  if (skippedUassets.nonEmpty) println(s"The following .uassets were skipped: ${skippedUassets.mkString(", ")}")
  messageOpt match {
    case Some(msg) => exit(-1, msg)
    case _ =>
  }
}

def setUAssetGUIConfigAndRun(f: () => Unit): Unit = {
  val oldConfigOpt = if (os.exists(uassetGuiConfig)) Some(os.read(uassetGuiConfig)) else None
  try {
    os.write.over(uassetGuiConfig,
      s"""{
         |  "PreferredVersion": $ueVersion,
         |  "PreferredMappings": "$usmap"
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
  val map = path.ext match {
    case "patch" => jdDataTableFilePatches(path)()
    case "toml" => tomlDataTableFilePatches(path)
  } 

  val name = {
    val i = path.last.indexOf('.')
    if (i >= 0) path.last.substring(0, i) else path.last
  }
  var lines = Vector(
    s"def patch$name(obj: uassetapi.Struct): Unit = {",
    "  val name = obj.name",
    "  name match {"
  )
  for ((name, properties) <- map) {
    if (properties.size == 1) {
      for ((property, value) <- properties) lines = lines :+ s"    case \"$name\" => obj(\"$property\") = ${toJsonPrettyString(value.newValueOpt)}"
    } else {
      lines = lines :+ s"    case \"$name\" =>"
      for ((property, value) <- properties) lines = lines :+ s"      obj(\"$property\") = ${toJsonPrettyString(value.newValueOpt)}"
    }
    lines = lines :+ ""
  }
  lines = lines :+ "    case _ =>"
  lines = lines :+ "  }"
  lines = lines :+ "}"
  println(lines.mkString(Properties.lineSeparator))
}

def tomlString(valueOpt: Option[JsonNode], default: String): String = valueOpt match {
  case Some(value: ObjectNode) =>
    var elements = Vector[String]()
    for (fieldName <- value.fieldNames.asScala) elements :+= s"\"$fieldName\" = ${tomlString(Option(value.get(fieldName)), default)}"
    s"{ ${elements.mkString(", ")} }"
  case Some(value: ArrayNode) =>
    var elements = Vector[String]()
    for (i <- 0 until value.size) elements :+= tomlString(Option(value.get(i)), default)
    s"[ ${elements.mkString(", ")} ]"
  case Some(value: TextNode) if value.textValue.contains('\n') => s"'''${value.textValue}'''"
  case Some(_: NullNode) => "'null'"
  case Some(_) => toJsonPrettyString(valueOpt, default)
  case None => default
}

def writeToml(path: os.Path, data: UAssetPropertyChanges, origAstOpt: Option[JsonNode]): Unit = {
  val oldValueColumn = 61
  os.remove.all(path)
  val uassetName = path.baseName
  val sep = Properties.lineSeparator
  val objectMap = origAstOpt match {
    case Some(origAst) =>
      val map = collection.mutable.HashMap.empty[String, uassetapi.Struct]
      val array = origAst.at(dataTablePath).asInstanceOf[ArrayNode]
      for (i <- 0 until array.size) {
        val o = uassetapi.Struct(uassetName, array.get(i), addToDataTableFilePatches = true) 
        map.put(o.name, o)
      }
      map
    case _ => null
  }
  os.write.append(path, s"# ... ${(for (_ <- 0 until oldValueColumn - 7) yield ' ').mkString} # Game Original Value$sep")
  for ((name, properties) <- data if patchlet.getKeyPrefix(name).isEmpty) {
    var n = name
    if (!n.forall(c => c.isLetterOrDigit || c == '_')) n = s"'$n'"
    os.write.append(path, s"[$n]$sep")
    val obj = if (objectMap == null) null else objectMap.get(name).get
    for ((property, valuePair) <- properties) {
      val v = tomlString(valuePair.newValueOpt, default = "\"null\"")
      val old = if (obj == null) valuePair.oldValueOpt else Option(obj.getJson(property))
      val comment = tomlString(old, default = "N/A")
      var line = s"$property = $v"
      if (line.length < oldValueColumn - 2) line = s"$line${(for (_ <- 0 until oldValueColumn - line.length - 1) yield ' ').mkString} # $comment$sep"
      else line = s"$line    # $comment$sep"
      os.write.append(path, line)
    }
    os.write.append(path, sep)
  }
  println(s"Wrote $path")
}

def toml(sbPakDir: os.Path, path: os.Path, disableCodePatching: Boolean)(): Unit = {
  if (os.exists(path) && !os.isDir(path)) {
    exit(-1, s"$path is not a directory")
  }

  val currMap = collection.mutable.HashMap.empty[String, JsonAst]
  val origMap = collection.mutable.HashMap.empty[String, JsonNode]
  if (!disableCodePatching)
    generateMod(addToDataTableFilePatches = true, None, sbPakDir, disableFilePatching = true, disableCodePatching, dryRun = true, currMap, origMap)()
  generateMod(addToDataTableFilePatches = true, None, sbPakDir, disableFilePatching = false, disableCodePatching, dryRun = true, currMap, origMap)()

  os.makeDir.all(path)
  for ((uassetName, data) <- patches if uassetName.order == 0) {
    val p = path / s"$uassetName.toml"
    writeToml(p, data, Some(origMap.get(uassetName.value).get))
  }
  if (patches.isEmpty) println("No patches to write")
  else println()
}

def diff(from: os.Path, to: os.Path, out: os.Path): Unit = {
  var errors = Vector[String]()

  def rec(f: os.Path, t: os.Path): Unit = {
    if (os.isFile(f) && os.isFile(t) && f.ext.toLowerCase == "json" && t.ext.toLowerCase == "json") {
      val patch = out / s"${f.baseName}.patch"
      println(s"Diffing $f => $t ...")
      os.proc(jdExe, "-o", patch, f, t).call(cwd = workingDir, check = false).exitCode match {
        case 0 =>
          println("No changes found")
        case 1 =>
          println(s"Wrote $patch")
          writeToml(out / s"${f.baseName}.toml", jdDataTableFilePatches(patch)(msgs => 
            errors :+= s"""* $patch
                          |${msgs.map("  " + _).mkString(Properties.lineSeparator)}""".stripMargin), None)
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

  if (errors.nonEmpty) exit(-1, 
    s"""|The following patches were skipped:
        |${errors.mkString(Properties.lineSeparator)}""".stripMargin)
}

def vscode(vscOpt: Option[os.Path]): Unit = {
  def setup(cmd: os.Path): Unit = {
    val name = if (cmd.last == "code.cmd") "VSCode"  else "VSCodium"
    println(s"Setting up ${absPath(cmd / os.up /  os.up)} ...")
    println()
    val extensions = Vector(
      "scalameta.metals", 
      "tamasfe.even-better-toml",
      absPath(workingDir / "vscode" / "sbmod-vscode.vsix")
    )
    for (extension <- extensions) {
      println(s"Installing $extension ...")
      os.proc("cmd.exe", "/C", cmd, "--force", "--install-extension", extension).call(cwd = workingDir, check = false)
      println()
    }
    println(s"To use, please open the $workingDir directory in $name")
    println()
  }
  var cmds = Vector(
    os.Path(s"${System.getenv("LOCALAPPDATA")}\\Programs\\Microsoft VS Code\\bin\\code.cmd"),
    os.Path("C:\\Program Files\\Microsoft VS Code\\bin\\code.cmd"),
    os.Path("C:\\Program Files (x86)\\Microsoft VS Code\\bin\\code.cmd"),
    os.Path(s"${System.getenv("LOCALAPPDATA")}\\Programs\\VSCodium\\bin\\codium.cmd"),
    os.Path("C:\\Program Files\\VSCodium\\bin\\codium.cmd")
  )
  for (vsc <- vscOpt) cmds = Vector(vsc / "bin" / "code.cmd", vsc / "bin" / "codium.cmd") ++ cmds
  for (cmd <- cmds if os.isFile(cmd)) {
    setup(cmd)
    return
  }
  exit(-1, "Could not find a suitable VSCode/VSCodium to install into")
}

def demo(isAIO: Boolean, isHard: Boolean, isEffect: Boolean, gameDir: os.Path): Unit = {
  def execute(p: os.proc): Unit = {
    println(s"Executing: ${p.commandChunks.mkString(" ")} ...")
    println()
    if (p.call(cwd = workingDir, check = false, stdout = os.Inherit, stderr = os.Inherit).exitCode != 0) exit(-1)
  }

  var modName = if (isAIO) "all-in-one" 
                else if (isEffect) "effect-table" 
                     else "beta-burst-recovery-scan"
  if (isHard) modName = s"$modName-hard"

  val modPatches = workingDir / "patches" / modName
  if (isAIO) {
    val dotAIO = workingDir / "patches" / ".all-in-one-code-patches-unified"
    if (!os.exists(dotAIO)) exit(-1, s"$dotAIO does not exist")
    os.remove.all(modPatches)
    println()
    execute(os.proc("xcopy", "/e", s"$dotAIO\\", s"$modPatches\\"))
    if (isHard) {
      def hard: os.Path = {
        for (p <- os.list(workingDir / "patches" / ".all-in-one-patches") if p.last.contains("987") && os.exists(p / ".hard")) 
          return p / ".hard"
        exit(-1, s"Could not find the .hard patch")
      }
      println()
      execute(os.proc("xcopy", "/e", s"$hard\\", s"$modPatches\\hard\\"))
    }
  } else if (isEffect) {
    var found = false
    for (p <- os.list(workingDir / "patches" / ".all-in-one-patches") if p.last.contains("987") if !found) {
      found = true
      os.remove.all(modPatches)
      println()
      execute(os.proc("xcopy", "/e", s"$p\\", s"$modPatches\\"))
    }
  }

  try {
    println()
    execute(os.proc("cmd", "/D", "/C", "automod", modName, gameDir))
  } catch {
    case _: Throwable => exit(-1)
  } finally {
    if (isAIO | isEffect) execute(os.proc("cmd", "/D", "/C", "rmdir", "/s", "/q", modPatches))
  }
}

case class SearchPath(labelOpt: Option[String], path: String)
case class UAssetSearch(uassetName: String, searchPaths: Vector[SearchPath])

def search(sbPakDir: os.Path, pathsInput: os.Path, outDir: os.Path): Unit = {
  def objectNameOpt(array: ArrayNode): Option[String] = {
    if (array.size != 1) return None
    array.get(0) match {
      case o: ObjectNode => o.get("Name") match {
        case name: TextNode => return Some(name.textValue)
        case _ =>
      }
      case _ =>
    }
    None
  }
  def isLabelChar(c: Char): Boolean = c == '_' || c == '-' || c.isLetterOrDigit
  def getLabelLineOpt(line: String): Option[(String, String)] = {
    var i = 0
    while (i < line.length && isLabelChar(line(i))) {
      i += 1
    }
    if (0 < i && i < line.length && line(i) == ':') Some((line.substring(0, i), line.substring(i + 1).trim))
    else None
  }
  

  val prefix = "#"
  
  val lines = for (line <- os.read(pathsInput).split('\n').map(_.trim) if line.nonEmpty) yield line 
  var uassetPaths = Vector[UAssetSearch]()
  var i = 0
  
  var uassetNames = Vector[String]()
  while (i < lines.length && !lines(i).startsWith(prefix)) i += 1
  while (i < lines.length) {
    val uassetName = lines(i).substring(prefix.length).trim
    uassetNames = uassetNames :+ uassetName
    var paths = Vector[SearchPath]()
    i += 1
    while (i < lines.length && !lines(i).startsWith(prefix)) {
      var (labelOpt, line) = getLabelLineOpt(lines(i)) match {
        case Some((label, l)) => (Some(label), l)
        case l => (None, lines(i))
      }
      if (line.head == '$') paths = paths :+ SearchPath(labelOpt, line)
      i += 1
    }
    if (paths.nonEmpty) uassetPaths = uassetPaths :+ UAssetSearch(uassetName, paths)
  }
  
  if (uassetPaths.isEmpty) exit(-1, s"Could not find any JSON path in $pathsInput")

  generateMod(addToDataTableFilePatches = false, None, sbPakDir, disableFilePatching = true, disableCodePatching = true, 
              dryRun = true, uassetNameRequests = uassetNames)()

  os.makeDir.all(outDir)

  for (UAssetSearch(uassetName, paths) <- uassetPaths) {
    val f = cacheDir / s"$uassetName.json"
    val dc = jp.parse(f.toIO)

    for (j <- 1 to paths.length) {
      val SearchPath(labelOpt, path) = paths(j - 1)
      try {
        val o = JsonNodeFactory.instance.objectNode
        val r = dc.read[ArrayNode](path)
        o.set("path", TextNode.valueOf(path))
        o.set("result", r)
        val out = labelOpt match {
          case Some(label) => outDir / s"$uassetName-$j-$label.json"
          case _ => objectNameOpt(r) match {
            case Some(name) => outDir / s"$uassetName-$j-$name.json"
            case _ => outDir / s"$uassetName-$j.json"
          }
        } 
        os.remove.all(out)
        writeJson(out, o)
        println(s"Wrote $out") 
      }catch {
        case t: Throwable => 
          println(s"""Could not search $uassetName using path (skipped): $path
                     |  reason: ${t.getMessage}""".stripMargin)
      }
    }
  }
}

def printUsage(): Unit = {
  exit(0,
    s"""$header
       |
       |Usage: automod [ <mod-name> <path-to-game> option*
       |               | .code <path-to-jd-patch-file>
       |               | .demo[.aio|.all|.effect] <path-to-StellarBlade>
       |               | .diff[.into] <from-path> <to-path> <out-path>
       |               | .search <path-to-game> <paths-input>.sam <out-path>
       |               | .setup[.vscode [ <path-to-vscode> ]]
       |               | .toml[.all] <path-to-game> <out-path>
       |               ]
       |
       |option:
       | --no-code-patching   Disable code patching
       | --dry-run            Disable actual mod generation and just test patches
       |
       |.code                 Print Auto Modding Script patching code from a jd/TOML patch file
       |.demo                 Generate the beta-burst-recovery-scan demo mod
       |.demo.aio             Generate the all-in-one demo mod
       |.demo.aio.hard        Generate the all-in-one demo mod (harder mode)
       |.demo.all             Generate all demo mods
       |.demo.effect          Generate the effect-table demo mod
       |.diff                 Recursively diff JSON files and write jd and TOML patch files
       |.diff.into            Use .diff between <from-path> with each sub-folder of <to-path>
       |.search               Query UAssetAPI JSON files using the JSON paths in <paths-input>
       |.setup                Only set up modding tools
       |.setup.vscode         Set up modding tools and VSCode extensions
       |.toml                 Merge existing patch files in patches as TOML patch files
       |.toml.all             Merge script code patches with patch files in patches as TOML""".stripMargin)
}

def checkDir(p: os.Path): os.Path = if (os.isDir(p)) p else exit(-1, s"$p is not a directory")
def checkFileExt(p: os.Path, ext: String): os.Path = if (os.isFile(p) && p.ext == ext) p else exit(-1, s"$p is not a file with .$ext extension")
def checkDirAvailable(p: os.Path): os.Path = if (os.isFile(p)) exit(-1, s"$p is a file") else p

def run(): Unit = {
  if (cliArgs.length == 0) printUsage()
  val argName = cliArgs.head
  argName match {
    case ".setup" => if (cliArgs.length != 1) printUsage()
    case ".diff" | ".diff.into" | ".search" => if (cliArgs.length != 4) printUsage()
    case ".toml" | ".toml.all" => if (cliArgs.length != 3) printUsage()
    case ".setup.vscode" => if (cliArgs.length != 1 && cliArgs.length != 2) printUsage()
    case _ if argName.head != '.' => if (cliArgs.length < 2) printUsage()
    case _ => if (cliArgs.length != 2) printUsage()
  }
  
  val setup = setupModTools()

  def demoFirst(): Unit = demo(isAIO = false, isHard = false, isEffect = false, checkDir(absPath(cliArgs(1))))
  def demoAio(): Unit = demo(isAIO = true, isHard = false, isEffect = false, checkDir(absPath(cliArgs(1))))
  def demoAioHard(): Unit = demo(isAIO = true, isHard = true, isEffect = false, checkDir(absPath(cliArgs(1))))
  def demoEffect(): Unit = demo(isAIO = false, isHard = false, isEffect = true, checkDir(absPath(cliArgs(1))))
  def demoAll(): Unit = { demoFirst(); demoAio(); demoAioHard(); demoEffect() }

  argName match {
    case ".code" => code(absPath(cliArgs(1)))
    case ".demo" => demoFirst()
    case ".demo.aio" => demoAio()
    case ".demo.aio.hard" => demoAioHard()
    case ".demo.all" => demoAll()
    case ".demo.effect" => demoEffect()
    case ".diff" => diff(checkDir(absPath(cliArgs(1))), checkDir(absPath(cliArgs(2))), checkDirAvailable(absPath(cliArgs(3))))
    case ".diff.into" =>
      val out = checkDirAvailable(absPath(cliArgs(3)))
      val from = checkDir(absPath(cliArgs(1)))
      for (d <- os.list(checkDir(absPath(cliArgs(2)))) if os.isDir(d)) diff(from, d, out / d.last)
    case ".search" =>
      val sbPakDir = checkDir(absPath(cliArgs(1)) / os.RelPath(config.game.contentPaks))
      val input = checkFileExt(absPath(cliArgs(2)), "sam")
      val outDir = checkDirAvailable(absPath(cliArgs(3)))
      search(sbPakDir, input, outDir)
    case ".setup" => if (setup) println("All modding tools have been set up")
    case ".setup.vscode" => vscode(if (cliArgs.length == 2) Some(absPath(cliArgs(1))) else None)
    case ".toml" | ".toml.all" => 
      val sbPakDir = checkDir(absPath(cliArgs(1)) / os.RelPath(config.game.contentPaks))
      val outDir = checkDirAvailable(absPath(cliArgs(2)))
      setUAssetGUIConfigAndRun(toml(sbPakDir, outDir, argName == ".toml"))
    case _ =>
      if (argName.startsWith(".")) exit(-1, s"Unrecognized command $argName")
      val modName = argName
      val gameDir = absPath(cliArgs(1))
      val sbPakDir = checkDir(gameDir / os.RelPath(config.game.contentPaks))
      println(
        s"""$header
           |* Game directory: $gameDir
           |* Mod name to generate: $argName
           |* Working directory: $workingDir
           |* Using: retoc v$retocVersion, UAssetGUI v$uassetGuiVersion, jd v$jdVersion, $usmapFilename
           |* Extra: FModel @$fmodelShortSha
           |""".stripMargin)
      var _noCodePatching = false
      var _dryRun = false
      for (i <- 2 until cliArgs.length) {
        cliArgs(i) match {
          case `noCodePatching` =>
            if (_noCodePatching) exit(-1, s"Redundant option $noCodePatching")
            _noCodePatching = true
          case `dryRun` =>
            if (_dryRun) exit(-1, s"Redundant option $dryRun")
            _dryRun = true
        }
      }
      setUAssetGUIConfigAndRun(generateMod(addToDataTableFilePatches = false, Some(modName), sbPakDir, disableFilePatching = false, _noCodePatching, _dryRun))
  }
  println("... done!")
}

run()