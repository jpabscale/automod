import com.fasterxml.jackson.core.util.{DefaultIndenter, DefaultPrettyPrinter}
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper, ObjectWriter}
import com.fasterxml.jackson.databind.node.{JsonNodeFactory, ArrayNode, BooleanNode, DoubleNode, IntNode, NullNode, ObjectNode, TextNode}
import com.fasterxml.jackson.dataformat.toml.TomlMapper
import com.fasterxml.jackson.core.`type`.TypeReference
import com.jayway.jsonpath
import java.util.{EnumSet, Map => JMap}
import java.util.concurrent.ConcurrentHashMap
import scala.beans.BeanProperty
import scala.collection.immutable.{ListMap, TreeMap, TreeSet}
import scala.collection.mutable.HashMap
import scala.collection.parallel.CollectionConverters._
import scala.jdk.CollectionConverters._
import scala.util.Properties

val header = s"Auto Modding Script v2.8.1"

val isArm = System.getProperty("os.arch") == "arm64" || System.getProperty("os.arch") == "aarch64"

sealed trait OsKind {
  def isWin: Boolean
  def isLinux: Boolean
  def isMac: Boolean
  def isArm: Boolean
}

object OsKind {
  case object WinAmd64 extends OsKind {
    def isWin: Boolean = true
    def isLinux: Boolean = false
    def isMac: Boolean = false
    def isArm: Boolean = false
    override def toString = "Windows/amd64"
  }
  case object LinuxAmd64 extends OsKind {
    def isWin: Boolean = false
    def isLinux: Boolean = true
    def isMac: Boolean = false
    def isArm: Boolean = false
    override def toString = "Linux/amd64"
  }
  case object MacAmd64 extends OsKind {
    def isWin: Boolean = false
    def isLinux: Boolean = false
    def isMac: Boolean = true
    def isArm: Boolean = false
    override def toString = "macOS/amd64"
  }
  case object WinArm64 extends OsKind {
    def isWin: Boolean = true
    def isLinux: Boolean = false
    def isMac: Boolean = false
    def isArm: Boolean = true
    override def toString = "Windows/arm64"
  }
  case object LinuxArm64 extends OsKind {
    def isWin: Boolean = false
    def isLinux: Boolean = true
    def isMac: Boolean = false
    def isArm: Boolean = true
    override def toString = "Linux/arm64"
  }
  case object MacArm64 extends OsKind {
    def isWin: Boolean = false
    def isLinux: Boolean = false
    def isMac: Boolean = true
    def isArm: Boolean = true
    override def toString = "macOS/arm64"
  }
}

val osKind = if (util.Properties.isWin) if (isArm) OsKind.WinArm64 else OsKind.WinAmd64 
             else if (util.Properties.isLinux) if (isArm) OsKind.LinuxArm64 else OsKind.LinuxAmd64
             else if (util.Properties.isMac) if (isArm) OsKind.MacArm64 else OsKind.MacAmd64
             else exit(-1, s"Unsupported platform")

if (osKind.isMac) exit(-1, s"Unsupported platform: .NET 8 for macOS does not currently work well enough for UAssetCLI")

val automodDir = {
  var file = new java.io.File(sourcecode.File())
  while (!new java.io.File(file, "automod.sc").exists) file = file.getParentFile
  os.Path(file.getCanonicalFile.getAbsolutePath)
}

val maxLogs = Option(System.getenv("AUTOMOD_MAX_LOGS")).flatMap(_.toDoubleOption.map(Math.ceil(_).toInt)).getOrElse(30)

val noPar = "true" == System.getenv("AUTOMOD_NO_PAR") || osKind.isLinux || osKind.isMac || osKind.isArm

val dataTablePath = "/Exports/0/Table/Data"
val noCodePatching = "--no-code-patching"
val dryRun = "--dry-run"
val includePatches = "--include-patches"
val ultraCompression = "--ultra-compression"

var cliArgs = args match {
  case Array("-s", _*) => args.drop(1)
  case _ => args
}

def exit(code: Int, msg: String = null): Nothing = {
  Option(msg).foreach((if (code == 0) Console.out else Console.err).println(_))
  System.exit(code)
  throw new RuntimeException
}

val zipToolVersion = "25.01"
var modExt = "zip"
val usmapUrlPrefix = "https://github.com/jpabscale/UAssetCLI/releases/download/usmap/"

class Game {
  @BeanProperty var contentPaks: String = "SB/Content/Paks"
  @BeanProperty var unrealEngine: String = "4.26"
  @BeanProperty var mapUri: String = s"${usmapUrlPrefix}StellarBlade_1.3.1.usmap"
  @BeanProperty var aesKey: String = ""
}

class Tools {
  @BeanProperty var retoc: String = "0.1.2"
  @BeanProperty var uassetCli: String = "1.0.0"
  @BeanProperty var fmodel: String = "0cc8da95e1cacd90662afe3124f88c6527079ea7"
  @BeanProperty var jd: String = "2.3.0"
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
type UAssetPropertyChanges = Map[String, PropertyChanges]
type FilePatches = TreeMap[OrderedString, UAssetPropertyChanges]
type CodePatches = TreeMap[String, uassetapi.Struct => Unit]
type JsonAst = com.jayway.jsonpath.DocumentContext

class ILinkedHashMap[K, +V](value: java.util.LinkedHashMap[K, V] = new java.util.LinkedHashMap[K, V]) extends collection.immutable.Map[K, V] {

  override def iterator: Iterator[(K, V)] = new Iterator[(K, V)] {
    val it = value.entrySet.iterator
    override def hasNext: Boolean = it.hasNext
    override def next(): (K, V) = {
      val next = it.next
      (next.getKey, next.getValue)
    }
  }

  override def get(key: K): Option[V] = Option(value.get(key))

  override def removed(key: K): Map[K,V] = {
    val newValue = cloneValue
    newValue.remove(key)
    new ILinkedHashMap(newValue)
  }

  override def updated[V1 >: V](key: K, value: V1): Map[K, V1] = {
    val newValue = cloneValue[V1]
    newValue.put(key, value)
    new ILinkedHashMap(newValue)
  }

  def cloneValue[V1 >: V]: java.util.LinkedHashMap[K, V1] = value.clone().asInstanceOf[java.util.LinkedHashMap[K, V1]]
}

object ILinkedHashMap {
  def empty[K, V]: ILinkedHashMap[K, V] = new ILinkedHashMap[K, V]
}

val emptyPropertyChanges: PropertyChanges = TreeMap.empty
val emptyUAssetPropertyChanges: UAssetPropertyChanges = ILinkedHashMap.empty
val emptyFilePatches: FilePatches = TreeMap.empty
val emptyCodePatches: CodePatches = TreeMap.empty

val jp: jsonpath.ParseContext = {
  jsonpath.Configuration.setDefaults(new jsonpath.Configuration.Defaults {
    override val options = EnumSet.of(jsonpath.Option.ALWAYS_RETURN_LIST) 
    override val jsonProvider = new jsonpath.spi.json.JacksonJsonNodeJsonProvider
    override val mappingProvider = new jsonpath.spi.mapper.JacksonMappingProvider
  })
  jsonpath.JsonPath.using(jsonpath.Configuration.defaultConfiguration)
}

val jpPathList: jsonpath.ParseContext = {
  jsonpath.Configuration.setDefaults(new jsonpath.Configuration.Defaults {
    override val options = EnumSet.of(jsonpath.Option.ALWAYS_RETURN_LIST, jsonpath.Option.AS_PATH_LIST) 
    override val jsonProvider = new jsonpath.spi.json.JacksonJsonNodeJsonProvider
    override val mappingProvider = new jsonpath.spi.mapper.JacksonMappingProvider
  })
  jsonpath.JsonPath.using(jsonpath.Configuration.defaultConfiguration)
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

def absPath(p: os.Path): String = p.toString
def absPath(p: String): os.Path = os.Path(new java.io.File(p).getCanonicalFile.getAbsolutePath)

val workingDir = os.pwd
val patchesDir = workingDir / "patches"
val configPath = workingDir / ".config.json"
lazy val logDir = {
  val d = workingDir / ".log"
  os.makeDir.all(d)
  for (p <- os.list(d).filter(os.isDir).sortWith((p1, p2) => p1.toIO.lastModified >= p2.toIO.lastModified).drop(maxLogs)) 
    os.remove.all(p)
  d / s"${cliArgs.head}-${getTimestamp()}"
}
val localAppData = if (osKind.isWin) os.Path(System.getenv("LOCALAPPDATA")) else os.home / ".local" / "share"
val userName = if (osKind.isWin) System.getenv("USERNAME") else os.proc("whoami").call().out.toString.trim
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
  r
}

val retocVersion = config.tools.retoc
val uassetCliVersion = config.tools.uassetCli
val fmodelSha = config.tools.fmodel
val fmodelShortSha = fmodelSha.substring(0, 7)
val jdVersion = config.tools.jd
val ueVersion = config.game.unrealEngine
val ueVersionCode = s"UE${ueVersion.replace('.', '_')}"

val usmapUri = config.game.mapUri
val usmapFilename = usmapUri.substring(usmapUri.lastIndexOf('/') + 1)
val usmapPath = automodDir / usmapFilename
val usmap = usmapPath.baseName
val toolsDir = automodDir / "tools"
val usmapDir = toolsDir / "usmap"

val retocUrlPrefix = s"https://github.com/trumank/retoc/releases/download/v$retocVersion"
val uassetCliUrl = s"https://github.com/jpabscale/UAssetCLI/releases/download/v$uassetCliVersion/UAssetCLI.zip"
val fmodelUrl = s"https://github.com/4sval/FModel/releases/download/qa/$fmodelSha.zip"
val jdUrlPrefix = s"https://github.com/josephburnett/jd/releases/download/v$jdVersion"
val z7rUrl = s"https://github.com/ip7z/7zip/releases/download/$zipToolVersion/7zr.exe"
val z7UrlPrefix = s"https://github.com/ip7z/7zip/releases/download/$zipToolVersion"
val retocExe = toolsDir / "retoc" / (if (osKind.isWin) "retoc.exe" else "retoc")
val fmodelExe = toolsDir / "FModel.exe"
val jdExe = toolsDir / (if (osKind.isWin) "jd.exe" else "jd")
val zipExe = toolsDir / "7z" / (if (osKind.isWin) "7z.exe" else "7zz")

val uassetCliDir = toolsDir / "UAssetCLI"
val uassetGuiSettingsDir = localAppData / "UAssetGUI"
val uassetGuiConfig = uassetGuiSettingsDir / "config.json"
val uassetGuiMappingsDir = uassetGuiSettingsDir / "Mappings"

val gameId = config.game.contentPaks.split('/').head
val automodGameCacheDir = automodDir / ".cache" / usmapFilename.replace(".usmap", "") / gameId
val cacheDir = workingDir / ".cache"
lazy val dotnet = if (os.exists(os.home / ".dotnet" / "dotnet")) absPath(os.home / ".dotnet" / "dotnet") else "dotnet"

val discardProcessOutput = new os.ProcessOutput {
  def redirectTo: ProcessBuilder.Redirect = ProcessBuilder.Redirect.DISCARD
  def processOutput(out: => os.SubProcess.OutputStream): Option[Runnable] = None
}

def init(): Boolean = {
  var setup = true

  val tempDir = localAppData / "Temp" / "automod"
  os.makeDir.all(tempDir)
  os.write.over(tempDir / ".automod.dir", automodDir.toString)
  
  os.makeDir.all(usmapDir)

  def download(uri: String, noCache: Boolean = false): Option[os.Path] = {
    val cacheName = java.util.Base64.getEncoder().encodeToString(uri.getBytes(java.nio.charset.StandardCharsets.UTF_8))
    val cachePath = tempDir / cacheName
    val dest = toolsDir / uri.substring(uri.lastIndexOf('/') + 1)
    if (noCache || !os.exists(cachePath)) {
      os.remove.all(cachePath)
      if (uri.startsWith("https://")) os.proc("curl", "-JLo", cachePath, uri).call(cwd = toolsDir, stdout = os.Inherit, stderr = os.Inherit)
      else if (uri.startsWith("file://")) os.copy.over(os.Path(new java.io.File(new java.net.URI(uri)).getCanonicalFile.getAbsolutePath), cachePath)
      else if (os.exists(automodDir / os.RelPath(uri))) os.copy.over(automodDir / os.RelPath(uri), cachePath)
    }
    if (os.exists(cachePath)) os.copy.over(cachePath, dest)
    if (os.exists(dest)) Some(dest) else None
  }

  def downloadCheck(uri: String): os.Path = {
    download(uri) match {
      case Some(p) => p
      case _ => exit(-1, s"Could not set up from $uri")
    }
  }

  if (!os.exists(zipExe)) {
    setup = false
    println(s"Setting up 7z v$zipToolVersion in $toolsDir ...")
    osKind match {
      case OsKind.WinAmd64 | OsKind.WinArm64 =>
        val z7r = downloadCheck(z7rUrl)
        val z7 = downloadCheck(s"$z7UrlPrefix/7z${zipToolVersion.replace(".", "")}-${if (isArm) "arm64" else "x64"}.exe")
        os.makeDir.all(toolsDir / "7z")
        os.proc(toolsDir / "7zr.exe", "x", z7).call(cwd = toolsDir / "7z")
        os.remove.all(z7)
        z7r.toIO.deleteOnExit
      case OsKind.LinuxAmd64 | OsKind.LinuxArm64 =>
        val z7 = downloadCheck(s"$z7UrlPrefix/7z${zipToolVersion.replace(".", "")}-linux-${if (isArm) "arm64" else "x64"}.tar.xz")
        os.makeDir.all(toolsDir / "7z")
        os.proc("tar", "xf", z7).call(cwd = toolsDir / "7z")
        os.remove.all(z7)
      case OsKind.MacAmd64 | OsKind.MacArm64 =>
        val z7 = downloadCheck(s"$z7UrlPrefix/7z${zipToolVersion.replace(".", "")}-mac.tar.xz")
        os.makeDir.all(toolsDir / "7z")
        os.proc("tar", "xf", z7).call(cwd = toolsDir / "7z")
        os.remove.all(z7)
    }
    println()
  }

  if (!os.exists(retocExe)) {
    setup = false
    println(s"Setting up retoc v$retocVersion in $toolsDir ...")
    val retocBundleName = osKind match {
      case OsKind.WinAmd64 | OsKind.WinArm64 => "retoc-x86_64-pc-windows-msvc.zip"
      case OsKind.MacAmd64 => "retoc-x86_64-apple-darwin.tar.xz"
      case OsKind.LinuxAmd64 => "retoc-x86_64-unknown-linux-gnu.tar.xz"
      case OsKind.LinuxArm64 => "retoc-aarch64-unknown-linux-gnu.tar.xz"
      case OsKind.MacArm64 => "retoc-aarch64-apple-darwin.tar.xz"
    }
    val retocBundle = downloadCheck(s"$retocUrlPrefix/$retocBundleName")
    os.remove.all(retocExe / os.up)
    if (osKind.isWin) {
      os.makeDir.all(retocExe / os.up)
      os.proc(zipExe, "x", retocBundle).call(cwd = retocExe / os.up)
    } else {
      os.proc("tar", "xf", retocBundle).call(cwd = toolsDir)
      for (p <- os.list(toolsDir) if os.isDir(p) && p.last.startsWith("retoc-")) {
        os.move.over(p, toolsDir / "retoc")
      }
    }
    os.remove.all(retocBundle)
    println()
  }

  if (!os.exists(uassetCliDir)) {
    setup = false
    println(s"Setting up UAssetCLI v$uassetCliVersion in $uassetCliDir ...")
    val uassetCliZip = downloadCheck(uassetCliUrl)
    os.proc(zipExe, "x", uassetCliZip).call(cwd = toolsDir)
    os.remove.all(uassetCliZip)
    println()
  }

  val usmap = usmapDir / usmapFilename

  if (!os.exists(usmap)) {
    setup = false
    println(s"Setting up $usmapFilename in $usmapDir ...")
    os.move.over(downloadCheck(usmapUri), usmap)
    println()
  }

  {
    val dest = uassetGuiMappingsDir / usmapFilename
    if (!os.exists(dest)) {
      setup = false
      os.makeDir.all(uassetGuiMappingsDir)
      os.copy.over(usmap, dest)
      println(s"Copied map file to $dest")
      println()
    }
  }

  if (osKind.isWin && !os.exists(fmodelExe)) {
    setup = false
    println(s"Setting up FModel @$fmodelShortSha in $toolsDir ...")
    val fmodelZip = downloadCheck(fmodelUrl)
    os.proc(zipExe, "x", fmodelZip).call(cwd = toolsDir)
    os.remove.all(fmodelZip)
    println()
  }

  if (!os.exists(jdExe)) {
    setup = false
    println(s"Setting up jd v$jdVersion in $toolsDir ...")
    val jdBundleName = osKind match {
      case OsKind.WinAmd64 => "jd-amd64-windows.exe"
      case OsKind.WinArm64 => "jd-arm64-windows.exe"
      case OsKind.LinuxAmd64 => "jd-amd64-linux"
      case OsKind.MacAmd64 => "jd-amd64-darwin"
      case OsKind.LinuxArm64 => "jd-arm64-linux"
      case OsKind.MacArm64 => "jd-arm64-darwin"
    }
    val jdBundle = downloadCheck(s"$jdUrlPrefix/$jdBundleName")
    os.move.over(jdBundle, jdExe)
    if (!osKind.isWin) jdExe.toIO.setExecutable(true)
    println()
  }

  if (!os.exists(automodGameCacheDir) && usmapUri.startsWith(usmapUrlPrefix)) {
    println(s"Setting up $automodGameCacheDir ...")
    download(usmapUri.replace("/usmap/", "/cache/").replace(".usmap", ".7z")) match {
      case Some(p) => 
        os.proc(zipExe, "x", p).call(cwd = automodDir, stdout = os.Inherit, stderr = os.Inherit)
        os.remove.all(p)
      case _ =>
    }
    println()
  }

  if (!os.exists(configPath) && automodDir.toString == workingDir.toString) writeConfig(config)

  setup
}

def toJsonNode(content: String): JsonNode = new ObjectMapper().readTree(content)

def jdFilePatches(path: os.Path)(err: Vector[String] => Unit = 
  msgs => if (msgs.nonEmpty) exit(-1, 
    s"""Error when loading $path:
       |${msgs.mkString(Properties.lineSeparator)}""".stripMargin)): UAssetPropertyChanges = {
  val entryPathPrefix = """@ [0,"Rows","""
  var map = emptyUAssetPropertyChanges
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
                map = map + (name -> (map.getOrElse(name, emptyPropertyChanges) +
                  (property -> ValuePair(Some(json), Some(oldValue)))))
              }
            case _ =>
              map = map + (name -> (map.getOrElse(name, emptyPropertyChanges) +
                (property -> ValuePair(None, Some(oldValue)))))
          }
        case Array(n) if mode == "-" =>
          ok = true
          var name = n
          name = name.substring(1, name.length - 1)
          val oldObj = toJsonNode(oldValueText).asInstanceOf[ObjectNode]
          var m = emptyPropertyChanges
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
          var m = emptyPropertyChanges
          for (fieldName <- newObjObj.fieldNames.asScala) {
            val newValue = newObjObj.get(fieldName)
            m = m + (fieldName -> ValuePair(Some(newValue), None))
          }
          map = map + (name -> m)
        case _ =>
      }
    }
    if (!ok) logPatch(path.baseName, s"The script currently does not handle the patch entry (skipped): $entryPath", console = true)
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

def tomlFilePatches(path: os.Path): UAssetPropertyChanges = {
  val toml: JMap[String, JMap[String, Object]] =
    new TomlMapper().readValue(path.toIO, new TypeReference[JMap[String, JMap[String, Object]]] {})
  var map = emptyUAssetPropertyChanges
  for (name <- toml.keySet.asScala) {
    var m = emptyPropertyChanges
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
private var _patches = emptyFilePatches

val updatedPatches = new ConcurrentHashMap[String, UAssetPropertyChanges]

def updatePatch(uassetName: String, objName: String, property: String, valuePair: ValuePair): Unit = {
  var map = Option(updatedPatches.get(uassetName)).getOrElse(emptyUAssetPropertyChanges)
  var m = map.getOrElse(objName, emptyPropertyChanges)
  m = m + (property -> valuePair)
  map = map + (objName -> m)
  updatedPatches.put(uassetName, map)
}

def updatePatches(uassetName: String, objName: String, properties: Iterable[(String, ValuePair)]): Unit = {
  var map = Option(updatedPatches.get(uassetName)).getOrElse(emptyUAssetPropertyChanges)
  var m = map.getOrElse(objName, emptyPropertyChanges)
  m = m ++ properties
  map = map + (objName -> m)
  updatedPatches.put(uassetName, map)
}

def applyChanges(path: String, map: FilePatches, uassetName: String, data: UAssetPropertyChanges): FilePatches = {
  val key = OrderedString(uassetName, path)
  var m = map.getOrElse(key, emptyUAssetPropertyChanges)
  for ((name, properties) <- data) {
    var m2 = m.getOrElse(name, emptyPropertyChanges)
    for ((property, valuePair) <- properties) {
      val valueString = toJsonPrettyString(valuePair.newValueOpt)
      val oldValueOpt = m2.get(property) match {
        case Some(v) =>
          logPatch(uassetName, s"* $name/$property: ${toJsonPrettyString(v.newValueOpt)} => $valueString", console = false)
          v.newValueOpt
        case _ =>
          logPatch(uassetName, s"* $name/$property: $valueString", console = false)
          None
      }
      m2 = m2 + (property -> ValuePair(valuePair.newValueOpt, oldValueOpt))
    }
    m = m + (name -> m2)
  }
  map + (key -> m)
}

def updatePatches(): Unit = {
  var map = if (_patches == null) emptyFilePatches else _patches

  def rec(path: os.Path): Unit =   {
    if (path.last.headOption == Some('.')) {
      println(s"Ignoring $path ...")
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
        } else {
          p.ext.toLowerCase match {
            case "patch" =>
              val uassetName = p.baseName
              logPatch(uassetName, s"Loading $p ...", console = true)
              var relPath = p.relativeTo(patchesDir).toString
              if (osKind.isWin) relPath = relPath.replace('/' , '\\')
              map = applyChanges(relPath, map, uassetName, jdFilePatches(p)())
            case "toml" =>
              val uassetName = p.baseName
              logPatch(uassetName, s"Loading $p ...", console = true)
              var relPath = p.relativeTo(patchesDir).toString
              if (osKind.isWin) relPath = relPath.replace('/' , '\\')
              map = applyChanges(relPath, map, uassetName, tomlFilePatches(p))
            case _ =>
          }
        }
      }
    }
  }

  if (os.isDir(patchesDir)) rec(patchesDir)
  println()
  _patches = map
}

def patches: FilePatches = {
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

def patchFromTree(maxOrder: Int, order: Int, addToFilePatches: Boolean, uassetName: String, ast: JsonAst, 
                  origAst: JsonAst, origAstPath: JsonAst)(tree: UAssetPropertyChanges): Unit = {
  val (kfcMap, atKfcMap, t) = patchlet.kfcMap(maxOrder, order, addToFilePatches, uassetName, ast, origAst, origAstPath, tree)
  for (kfc <- atKfcMap.values) {
    kfc.applyChanges(ast)
  }
  val data = ast.json[JsonNode].at(dataTablePath).asInstanceOf[ArrayNode]
  val origData = origAst.json[JsonNode].at(dataTablePath).asInstanceOf[ArrayNode]
  val dataMap = toDataMap(data)
  def updateProperties(obj: uassetapi.Struct, orig: JsonNode, properties: PropertyChanges): Unit = {
    for ((property, valueOldValuePair) <- properties) {
      var value = valueOldValuePair.newValueOpt.orNull
      value match {
        case v: TextNode if patchlet.getKeyPrefix(v.textValue) == Some(patchlet.Constants.codePrefix) =>
          val code = v.textValue.substring(patchlet.Constants.codePrefix.length)
          value = patchlet.evalProperty(uassetName, addToFilePatches, dataMap, code, obj, property, orig, ast, origAst)
        case _ =>
      }
      obj.setJson(property, value)
    }
  }
  def applyKfcs(i: Int, obj: uassetapi.Struct, origObj: uassetapi.Struct, kfcs: Iterable[patchlet.KeyFilteredChanges]): Unit = {
    for (kfc <- kfcs) kfc.applyPathChanges(s"$dataTablePath/$i", obj.value, origObj.value)
  }

  val indices = for (i <- 0 until data.size) yield i
  val qs = for (i <- indices.par) yield {
    val obj = uassetapi.Struct(uassetName, data.get(i), addToFilePatches = addToFilePatches)
    val kfcs = for (kfc <- kfcMap.values if kfc(obj.name)) yield kfc
    val propertiesOpt = t.get(obj.name) match {
      case Some(ps) => Some(ps)
      case _ => None
    }
    (i, obj, kfcs, propertiesOpt)
  }

  for ((i, obj, kfcs, propertiesOpt) <- qs) {
    val orig = origData.get(i)
    val origObj = uassetapi.Struct(uassetName, orig, addToFilePatches = false)
    applyKfcs(i, obj, origObj, kfcs)
    propertiesOpt match {
      case Some(properties) => updateProperties(obj, orig, properties)
      case _ =>
    }
  }
}

def logPatch(uassetName: String, line: String, console: Boolean): Unit = {
  if (console) println(line)
  val log = logDir / s"$uassetName.log"
  os.write.append(log, line)
  os.write.append(log, util.Properties.lineSeparator)
}

def checkPatchesDir(): Unit = {
  if (!os.isDir(patchesDir)) exit(-1, s"Missing directory: $patchesDir")
}

def generateMod(addToFilePatches: Boolean,
                modNameOpt: Option[String], 
                gamePakDirOpt: Option[os.Path], 
                disableFilePatching: Boolean, 
                disableCodePatching: Boolean, 
                dryRun: Boolean,
                includePatches: Boolean,
                currentAstMap: collection.mutable.HashMap[String, (JsonAst, JsonAst, JsonAst)] = collection.mutable.HashMap.empty,
                origAstMap: collection.mutable.HashMap[String, JsonNode] = null,
                uassetNameRequests: Vector[String] = Vector())(): Unit = {
  val cacheKey = cacheDir / gameId / "key.properties"
  val output = workingDir / "out"
  os.remove.all(output)

  def computeCacheKey(): String = {
    if (gamePakDirOpt.isEmpty) return null
    val sbPakDir = gamePakDirOpt.get
    if (!os.exists(sbPakDir)) return ""
    var r = Vector.empty[String]
    r = r :+ s"retoc=$retocVersion"
    r = r :+ s"UAssetCLI=$uassetCliVersion"
    for (p <- os.list(sbPakDir).sortWith((p1, p2) => p1.last <= p2.last) if os.isFile(p)) {
      r = r :+ s"${p.last}=${p.toIO.lastModified}"
    }
    r.mkString(Properties.lineSeparator)
  }

  def recreateDir(dir: os.Path): Unit = {
    os.remove.all(dir)
    os.makeDir.all(dir)
  }

  val tempDir = workingDir / ".temp"
  recreateDir(tempDir)
  recreateDir(logDir)

  val modDirOpt = modNameOpt.map(tempDir / _)

  val cacheHit = {
    val key = computeCacheKey()
    if (key == null || os.exists(cacheKey) && os.read(cacheKey) == key) true else {
      recreateDir(cacheKey / os.up)
      if (key.nonEmpty) os.write(cacheKey, key)
      false
    }
  }

  def retoc(exe: os.Path, args: os.Shellable*): Vector[os.Shellable] = {
    var r = Vector[os.Shellable](exe)
    val aesKey = config.game.aesKey 
    if (aesKey.size > 1) {
      r = r :+ "--aes-key"
      r = r :+ (if (aesKey.head == '0' && Character.toLowerCase(aesKey(1)) == 'x') aesKey else s"0x$aesKey")
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

  def uassetCliFailed(title: String, pUassetGui: os.proc, at: os.Path, repack: Boolean): Nothing = {
    val moreInfo = if (!repack) "T" else 
      s"""First, check that the patched JSON file has been changed as intended with correct values. 
         |If everyhing looks proper, t""".stripMargin
    exit(-1, 
      s"""Failed to use UAssetCLI to $title with the following command in $at:
         |
         |${pUassetGui.commandChunks.mkString(" ")}
         |
         |${moreInfo}ry to see if this is a known UAssetAPI issue (or filing a new one) at:
         |https://github.com/atenfyr/UAssetAPI/issues""".stripMargin)
  }

  val uassetNamePathMap = new ConcurrentHashMap[String, os.RelPath]

  def unpackJson(name: String): os.Path = {
    val json = s"$name.json"

    def findCached(dir: os.Path): os.Path = {
      if (!os.isDir(dir)) return null
      for (p <- os.walk(dir) if p.last == json) return p
      null
    }

    var jsonCache: os.Path = null
    var r: os.Path = null

    def tryCacheDir(dir: os.Path): Boolean = {
      jsonCache = findCached(dir)
      r = if (jsonCache != null) tempDir / jsonCache.relativeTo(dir / os.up) else null
      if (jsonCache != null && os.exists(jsonCache)) {
        os.makeDir.all(r / os.up)
        os.copy.over(jsonCache, r)
        val relPath = jsonCache.relativeTo(dir / os.up)
        uassetNamePathMap.put(name, relPath / os.up / s"${relPath.baseName}.uasset")
        println(s"Using cached $jsonCache")
        return true
      }
      false
    }
     

    if (cacheHit && tryCacheDir(cacheDir / gameId)) return r

    if (gamePakDirOpt.isEmpty && usmapUri.startsWith(usmapUrlPrefix) && tryCacheDir(automodGameCacheDir)) return r

    if (gamePakDirOpt.isEmpty) exit(-1, s"$name.json is not cached; please supply the game directory")

    val gamePakDir = gamePakDirOpt.get

    val outputName = output / name
    val profileCopyDir = outputName / userName
    val uassetGuiSettingsCopyDir = profileCopyDir / "AppData" / "Local" / "UAssetGUI"
    val retocCopyDir = outputName / "retoc"
    val uassetCliCopyDir = outputName / "uassetgui"
    val uassetCliDll = uassetCliCopyDir / "UAssetCLI.dll"
    val retocExeCopy = retocCopyDir / retocExe.last
    val uassetFilename = s"$name.uasset"
    val uexpFilename = s"$name.uexp"

    os.makeDir.all(retocCopyDir)
    os.copy.over(retocExe, retocExeCopy)

    println(s"Extracting $uassetFilename ...")
    val pRetoc = os.proc(retoc(retocExeCopy, "to-legacy", "--no-shaders", "--no-compres-shaders", "--no-parallel", "--version", ueVersionCode, "--filter", uassetFilename, gamePakDir, retocCopyDir))
    if (pRetoc.call(check = false, cwd = retocCopyDir, stdout = os.Inherit, stderr = os.Inherit).exitCode != 0 || !os.exists(retocCopyDir / gameId) || os.walk(retocCopyDir / gameId).isEmpty)
      retocFailed(s"extract $uassetFilename (double check the .uasset name)", pRetoc, retocCopyDir)
    println(s"... done extracting $uassetFilename")
    
    var uasset: os.Path = null
    for (p <- os.walk(outputName) if os.isFile(p))
      if (p.last == uassetFilename) uasset = p 
      else if (p.last == uexpFilename) {}
      else os.remove(p)

    assert(uasset != null)
    val relPath = uasset.relativeTo(retocCopyDir)
    val jsonRelPath = relPath / os.up / s"${relPath.baseName}.json"
    jsonCache = cacheDir / jsonRelPath
    r = tempDir / jsonRelPath
    os.makeDir.all(r / os.up)
    os.makeDir.all(jsonCache / os.up)

    println(s"Converting to $r ...")
    os.makeDir.all(uassetGuiSettingsCopyDir / os.up)
    os.copy.over(localAppData / "UAssetGUI", uassetGuiSettingsCopyDir)
    os.copy.over(uassetCliDir, uassetCliCopyDir)
    val env = Map[String, String]("USERPROFILE" -> absPath(profileCopyDir), "LOCALAPPDATA" -> absPath(uassetGuiSettingsCopyDir))
    val pUassetGui = os.proc(dotnet, uassetCliDll, "tojson", uasset, json, s"VER_$ueVersionCode", usmap)
    if (pUassetGui.call(check = false, cwd = uassetCliCopyDir, stdout = os.Inherit, stderr = os.Inherit, env = env).exitCode != 0) 
      uassetCliFailed(s"convert $uasset to JSON", pUassetGui, uassetCliCopyDir, repack = false)
    os.copy.over(uassetCliCopyDir / json, r)
    os.copy.over(r, jsonCache)
    println(s"... done converting to $r")

    uassetNamePathMap.put(name, relPath)
    os.remove.all(outputName)

    r
  }

  def packJson(name: String, path: os.Path): Unit = {
    val outputName = output / name
    val profileCopyDir = outputName / userName
    val uassetGuiSettingsCopyDir = profileCopyDir / "AppData" / "Local" / "UAssetGUI"
    val uasset = outputName / uassetNamePathMap.get(name)
    val pathCopy = outputName / path.last
    val uassetCliDll = outputName / "UAssetCLI.dll"

    os.makeDir.all(uasset / os.up)
    os.makeDir.all(uassetGuiSettingsCopyDir / os.up)
    os.copy.over(localAppData / "UAssetGUI", uassetGuiSettingsCopyDir)
    for (p <- os.list(uassetCliDir)) os.copy.over(p, outputName / p.last)
    os.copy.over(path, pathCopy)

    println(s"Regenerating $uasset ...")
    val pUassetGui = os.proc(dotnet, uassetCliDll , "fromjson", pathCopy, uasset, usmap)
    val env = Map[String, String]("USERPROFILE" -> absPath(profileCopyDir), "LOCALAPPDATA" -> absPath(uassetGuiSettingsCopyDir))
    if (pUassetGui.call(check = false, cwd = outputName, stdout = os.Inherit, stderr = os.Inherit, env = env).exitCode != 0)
      uassetCliFailed(s"convert $uasset from JSON", pUassetGui, outputName, repack = true)
    println(s"... done regenerating $uasset")
    os.makeDir.all(output / gameId)
    val src = outputName / gameId
    for (p <- os.walk(src) if os.isFile(p)) {
      val dest = output / gameId / p.relativeTo(src)
      os.makeDir.all(dest / os.up)
      os.copy(p, dest)
    }
    os.remove.all(outputName)
  }

  def packMod(modName: String): os.Path = {
    val modDir = modDirOpt.get  
    if (os.exists(modDir)) {
      exit(-1, s"$modDir already exists")
    }

    val pack = workingDir / s"$modName.$modExt"
    os.remove.all(pack)

    os.makeDir.all(modDir)
    val utoc = modDir / s"${modName}_P.utoc"
    println(s"Converting to $utoc ...")
    val pRetoc = os.proc(retoc(retocExe, "to-zen", "--no-parallel", "--version", ueVersionCode, output, utoc))
    if (pRetoc.call(check = false, cwd = tempDir, stdout = os.Inherit, stderr = os.Inherit).exitCode != 0)
      retocFailed(s"pack $modName", pRetoc, tempDir)
    println()

    if (includePatches) {
      println()
      println(s"Copying patches ...")
      for (p <- os.walk(patchesDir) if os.isFile(p) && (p.ext == "toml" || p.ext == "patch") && p.relativeTo(patchesDir).segments.forall(_.head != '.')) {
        val relPath = p.relativeTo(patchesDir / os.up)
        val dest = tempDir / modName / relPath
        os.makeDir.all(dest / os.up)
        os.copy.over(p, dest)
      }
      println()
    }

    println(s"Archiving $pack ...")
    modExt match {
      case "zip" => os.proc(zipExe, "a", s"-t$modExt", "-mtm-", pack, modName).call(cwd = tempDir)
      case "7z" => os.proc(zipExe, "a", s"-t$modExt", "-mx=9", "-mfb=273", "-mtm=off", pack, modName).call(cwd = tempDir)
    }
    
    println()

    pack
  }

  var codePatches = emptyCodePatches

  val shouldPack = modNameOpt.nonEmpty
  var uassetNames = codePatches.keySet ++ uassetNameRequests
  if (!disableFilePatching) {
    uassetNames = uassetNames ++ (for (key <- patches.keys) yield key.value)
  }
  if (!disableCodePatching) {
    uassetNames = uassetNames ++ patchCustom.uassetNames
  }
  val jsonMap = Map.empty[String, os.Path] ++ (
    if (noPar) for (uassetName <- uassetNames) yield (uassetName, unpackJson(uassetName))
    else for (uassetName <- uassetNames.toSeq.par) yield (uassetName, unpackJson(uassetName)))
  println()

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

  def getAllPatches(uassetName: String): Seq[UAssetPropertyChanges] = {
    var r = Seq.empty[UAssetPropertyChanges]
    for ((name, changes) <- patches if name.value == uassetName) {
      r :+= changes
    } 
    r
  }

  def patchUasset(uassetName: String): Unit = {
    val file = jsonMap(uassetName)
    val (ast: JsonAst, origAst: JsonAst, origAstPath: JsonAst) = currentAstMap.get(uassetName) match {
      case Some(t) => 
        t
      case _ => 
        val ast = jp.parse(file.toIO)
        val origAst = jp.parse(file.toIO)
        val origAstPath = jpPathList.parse(file.toIO)
        val t = (ast, origAst, origAstPath)
        currentAstMap.put(uassetName, t)
        t
    }
    val json = ast.json[JsonNode]
    if (origAstMap != null && !origAstMap.contains(uassetName)) origAstMap.put(uassetName, origAst.json[JsonNode])
    val custom = patchCustom.uassetNames.contains(uassetName)
    if (!custom && !disableFilePatching) {
      var maxOrder = 0
      for ((uassetNameOrder, _) <- patches if maxOrder < uassetNameOrder.order) maxOrder = uassetNameOrder.order
      for ((uassetNameOrder, tree) <- patches if uassetNameOrder.order != 0 && uassetNameOrder.value == uassetName) {
        logPatch(uassetName, s"Patching $file by using ${uassetNameOrder.path} ...", console = true)
        patchFromTree(maxOrder, uassetNameOrder.order, addToFilePatches, uassetName, ast, origAst, origAstPath)(tree)
        writeJson(file, json)
        println(s"... done patching $file by using ${uassetNameOrder.path}")
        logPatch(uassetName, "", console = false)
      }
    }
    if (custom && !patchCustom.patch(uassetName, json, getAllPatches(uassetName))) skipUasset(uassetName)
  }

  if (noPar) uassetNames.foreach(patchUasset) 
  else uassetNames.toSeq.par.foreach(patchUasset)
  
  println()

  modNameOpt match {
    case Some(modName) if !dryRun =>
      val entries = (for (entry <- jsonMap if !skippedUassets.contains(entry._1)) yield entry).toSeq.sortWith((e1, e2) => e1._2.toIO.length <= e2._2.toIO.length())
      if (noPar) entries.foreach(entry => packJson(entry._1, entry._2))
      else entries.par.foreach(entry => packJson(entry._1, entry._2))
      println()
      if ((jsonMap.keySet -- skippedUassets).nonEmpty) packMod(modName)
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
  def shouldInclude(name: String): Boolean = {
    patchlet.getKeyPrefix(name) match {
      case Some(patchlet.Constants.atPrefix) => true
      case Some(_) => false
      case None => true
    }
  }
  val oldValueColumn = 61
  os.remove.all(path)
  val uassetName = path.baseName
  val sep = Properties.lineSeparator
  val objectMap = origAstOpt match {
    case Some(origAst) =>
      val map = collection.mutable.HashMap.empty[String, uassetapi.Struct]
      val array = origAst.at(dataTablePath).asInstanceOf[ArrayNode]
      for (i <- 0 until array.size) {
        val o = uassetapi.Struct(uassetName, array.get(i), addToFilePatches = true) 
        map.put(o.name, o)
      }
      map
    case _ => null
  }
  os.write.append(path, s"# ... ${(for (_ <- 0 until oldValueColumn - 7) yield ' ').mkString} # Game Original Value$sep")
  for ((name, properties) <- data if shouldInclude(name)) {
    var n = name
    if (!n.forall(c => c.isLetterOrDigit || c == '_')) n = s"'$n'"
    os.write.append(path, s"[$n]$sep")
    val obj = if (objectMap == null) null else objectMap.get(name).orNull
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

def toml(gamePakDirOpt: Option[os.Path], path: os.Path, disableCodePatching: Boolean)(): Unit = {
  if (os.exists(path) && !os.isDir(path)) {
    exit(-1, s"$path is not a directory")
  }

  val currMap = collection.mutable.HashMap.empty[String, (JsonAst, JsonAst, JsonAst)]
  val origMap = collection.mutable.HashMap.empty[String, JsonNode]
  if (!disableCodePatching)
    generateMod(addToFilePatches = true, None, gamePakDirOpt, disableFilePatching = true, disableCodePatching, dryRun = true, includePatches = false, currMap, origMap)()
  generateMod(addToFilePatches = true, None, gamePakDirOpt, disableFilePatching = false, disableCodePatching, dryRun = true, includePatches = false, currMap, origMap)()

  os.makeDir.all(path)
  var noPatch = true
  for (uassetName <- updatedPatches.keys.asScala.toIndexedSeq.sortWith(_ <= _)) {
    noPatch = false
    val p = path / s"$uassetName.toml"
    val data = updatedPatches.get(uassetName)
    writeToml(p, data, Some(origMap(uassetName)))
  }
  if (noPatch) println("No patches to write")
  else println()
}

def diff(from: os.Path, to: os.Path, out: os.Path): Unit = {
  var errors = Vector[String]()

  def rec(f: os.Path, t: os.Path): Unit = {
    if (os.isFile(f) && os.isFile(t) && f.ext.toLowerCase == "json" && t.ext.toLowerCase == "json") {
      val patch = out / s"${f.baseName}.patch"
      println(s"Diffing $f => $t ...")
      os.proc(jdExe, "-o", patch, f, t).call(check = false).exitCode match {
        case 0 =>
          println("No changes found")
        case 1 =>
          println(s"Wrote $patch")
          writeToml(out / s"${f.baseName}.toml", jdFilePatches(patch)(msgs => 
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
    val name = if (cmd.last == "code.cmd" || cmd.last == "code") "VSCode"  else "VSCodium"
    println(s"Setting up $name using ${absPath(cmd)} ...")
    println()
    val extensions = Vector(
      "scalameta.metals", 
      "tamasfe.even-better-toml",
      absPath(automodDir / "vscode" / "automod-vscode.vsix")
    )
    if (osKind.isWin) {
      os.proc("cmd.exe", "/D", "/C", cmd, "--force", "--uninstall-extension", "jpabscale.sbmod-vscode").
        call(check = false, stdout = discardProcessOutput, mergeErrIntoOut = true)
    }
    for (extension <- extensions) {
      println(s"Installing $extension ...")
      if (osKind.isWin) {
        os.proc("cmd.exe", "/D", "/C", cmd, "--force", "--install-extension", extension).
          call(check = false, stdout = os.Inherit, stderr = os.Inherit)
      } else {
        os.proc(cmd, "--force", "--install-extension", extension).
          call(check = false, stdout = os.Inherit, stderr = os.Inherit)
      }
      println()
    }
    println()
  }
  var cmds = if (osKind.isWin) Vector(
    os.Path(s"$localAppData\\Programs\\Microsoft VS Code\\bin\\code.cmd"),
    os.Path("C:\\Program Files\\Microsoft VS Code\\bin\\code.cmd"),
    os.Path("C:\\Program Files (x86)\\Microsoft VS Code\\bin\\code.cmd"),
    os.Path(s"$localAppData\\Programs\\VSCodium\\bin\\codium.cmd"),
    os.Path("C:\\Program Files\\VSCodium\\bin\\codium.cmd")
  ) else if (osKind.isLinux) Vector(
    os.Path("/usr/bin/code"), os.Path("/usr/bin/codium")
  ) else if (osKind.isMac) Vector(
    os.home / "Applications/Visual Studio Code.app/Contents/Resources/app/bin/code",
    os.home / "Applications/VSCodium.app/Contents/Resources/app/bin/codium",
    os.Path("/Applications/Visual Studio Code.app/Contents/Resources/app/bin/code"),
    os.Path("/Applications/VSCodium.app/Contents/Resources/app/bin/codium")
  ) else Vector()
  for (vsc <- vscOpt) cmds = (
    if (osKind.isWin) Vector(vsc / "bin" / "code.cmd", vsc / "bin" / "codium.cmd") 
    else if (osKind.isMac) Vector(vsc / "Contents"/ "Resources" / "app" / "bin" / "code", vsc / "Contents"/ "Resources" / "app" / "bin" / "codium")
    else Vector(vsc / "bin" / "code", vsc / "bin" / "codium")
  ) ++ cmds
  for (cmd <- cmds if os.isFile(cmd)) {
    setup(cmd)
    return
  }
  exit(-1, "Could not find a suitable VSCode/VSCodium to install into")
}

def demoSb(isAIO: Boolean, isHard: Boolean, isEffect: Boolean, gameDirOpt: Option[os.Path]): Unit = {
  def execute(p: os.proc): Unit = {
    println(s"Executing: ${p.commandChunks.mkString(" ")} ...")
    println()
    if (p.call(cwd = workingDir, check = false, stdout = os.Inherit, stderr = os.Inherit).exitCode != 0) exit(-1)
  }

  var modName = if (isAIO) "all-in-one" 
                else if (isEffect) "effect-table" 
                     else "beta-burst-recovery-scan"
  if (isHard) modName = s"$modName-hard"

  val modPatches = patchesDir / modName

  if (!os.exists(patchesDir)) {
    if (osKind.isWin) execute(os.proc("cmd.exe", "/d", "/c", "md", patchesDir))
    else execute(os.proc("mkdir", patchesDir))
  }
  
  val aioPatches = automodDir / "patches" / "SB" / ".all-in-one-patches"
  if (isAIO) {
    val dotAIO = aioPatches / os.up / ".all-in-one-patches-unified"
    if (!os.exists(dotAIO)) exit(-1, s"$dotAIO does not exist")
    os.remove.all(modPatches)
    println()
    if (osKind.isWin) execute(os.proc("xcopy", "/e", s"$dotAIO\\", s"$modPatches\\"))
    else execute(os.proc("cp", "-R", dotAIO, modPatches))
    if (isHard) {
      def hard: os.Path = {
        for (p <- os.list(aioPatches) if p.last.contains("1329") && os.exists(p / "goddess" / ".hard")) 
          return p / "goddess" / ".hard"
        exit(-1, s"Could not find the .hard patch")
      }
      println()
      if (osKind.isWin) execute(os.proc("xcopy", "/e", s"$hard\\", s"$modPatches\\hard\\"))
      else execute(os.proc("cp", "-R", hard, modPatches / "hard"))
    }
  } else if (isEffect) {
    var found = false
    for (p <- os.list(aioPatches) if p.last.contains("987") if !found) {
      found = true
      os.remove.all(modPatches)
      println()
      if (osKind.isWin) {
        execute(os.proc("xcopy", "/e", s"$p\\", s"$modPatches\\"))
        execute(os.proc("cmd.exe", "/d", "/c", "del", modPatches / "TargetFilterTable.toml"))
      } else {
        execute(os.proc("cp", "-R", p, modPatches))
        execute(os.proc("rm", modPatches / "TargetFilterTable.toml"))
      }
    }
  } else {
    var found = false
    for (p <- os.list(aioPatches) if p.last.contains("987") if !found) {
      found = true
      os.remove.all(modPatches)
      println()
      if (osKind.isWin) {
        execute(os.proc("cmd.exe", "/d", "/c", "md", modPatches))
        execute(os.proc("cmd.exe", "/d", "/c", "copy", p / modName / "EffectTable.toml", modPatches / "EffectTable.toml"))
      } else {
        execute(os.proc("mkdir", modPatches))
        execute(os.proc("cp", p / modName / "EffectTable.toml", modPatches / "EffectTable.toml"))
      }
    }
  }

  try {
    println()
    gameDirOpt match {
      case Some(gameDir) => execute(os.proc("scala-cli", "--suppress-outdated-dependency-warning", automodDir / "project.scala", "--", modName, gameDir, noCodePatching, includePatches))
      case _ => execute(os.proc("scala-cli", "--suppress-outdated-dependency-warning", automodDir / "project.scala", "--", modName, noCodePatching, includePatches))
    }
  } catch {
    case _: Throwable => exit(-1)
  } finally {
    if (osKind.isWin) execute(os.proc("cmd", "/D", "/C", "rmdir", "/s", "/q", modPatches))
    else execute(os.proc("rm", "-fR", modPatches))
  }
}

case class SearchPath(labelOpt: Option[String], path: String)
case class UAssetSearch(uassetName: String, searchPaths: Vector[SearchPath])

def search(gamePakDirOpt: Option[os.Path], pathsInput: os.Path, outDir: os.Path): Unit = {
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

  generateMod(addToFilePatches = false, None, gamePakDirOpt, disableFilePatching = true, disableCodePatching = true, 
              dryRun = true, includePatches = false, uassetNameRequests = uassetNames)()

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

def printUsage(): Nothing = {
  exit(0,
    s"""$header
       |
       |Usage: automod [-s] [ <mod-name> [ <path-to-game> ] option*
       |                    | .demo.sb [ <path-to-StellarBlade> ]
       |                    | .diff[.into] <from-path> <to-path> <out-path>
       |                    | .search [ <path-to-game> ] <paths-input>.sam <out-path>
       |                    | .setup[.vscode [ <path-to-vscode> ]]
       |                    | .toml[.all] [ <path-to-game> ] <out-path>
       |                    ]
       |
       | -s                   Disable Scala CLI server
       |
       |option:
       | --dry-run            Disable actual mod generation and just test patches
       | --include-patches    Include patches in the generated mod
       | --no-code-patching   Disable code patching
       | --ultra-compression  Use 7z ultra compression
       |
       |.demo.sb              Generate all Stellar Blade demonstration mods
       |.diff                 Recursively diff JSON files and write jd and TOML patch files
       |.diff.into            Use .diff between <from-path> with each sub-folder of <to-path>
       |.search               Query UAssetAPI JSON files using the JSONPaths in <paths-input>
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
  val hasGameDir: Boolean = argName match {
    case "" => printUsage(); false
    case ".diff" | ".diff.into" => if (cliArgs.length != 4) printUsage(); false
    case ".toml" | ".toml.all" => if (cliArgs.length == 2) false else if (cliArgs.length == 3) true else printUsage()
    case ".search" => if (cliArgs.length == 3) false else if (cliArgs.length == 4) true else printUsage()
    case ".setup" => if (cliArgs.length != 1) printUsage(); false
    case ".setup.vscode" => if (cliArgs.length != 1 && cliArgs.length != 2) printUsage(); false
    case _ if argName.startsWith(".demo.") => if (cliArgs.length == 1) false else if (cliArgs.length == 2) true else printUsage()
    case _ if argName.head != '.' => cliArgs.length >= 2 && !cliArgs(1).startsWith("--")
    case _ => if (cliArgs.length != 2) printUsage(); false
  }

  lazy val gameDir = absPath(cliArgs(1))
  val (gamePakDirOpt, gameDirOpt, next) = if (hasGameDir) (Some(checkDir(gameDir / os.RelPath(config.game.contentPaks))), Some(gameDir), 2) 
                                        else (None, None, 1)
  println(header)
  println(
    s"""* Platform: $osKind
       |* Automod directory: $automodDir
       |* Using: retoc v$retocVersion, UAssetCLI v$uassetCliVersion, jd v$jdVersion, $usmapFilename""".stripMargin)
  if (osKind.isWin) println(s"* Extra: FModel @$fmodelShortSha")
  println(
    s"""* Parallelization enabled: ${!noPar}
       |* Maximum task logs: $maxLogs""".stripMargin)
  if (gamePakDirOpt.nonEmpty) println(s"* Game directory: $gameDir")
  println(s"* Working directory: $workingDir")
  if (argName.head != '.') println(s"* Mod name to generate: $argName")
  println()
  val setup = init()

  def demoSbFirst(): Unit = demoSb(isAIO = false, isHard = false, isEffect = false, gameDirOpt)
  def demoSbAio(): Unit = demoSb(isAIO = true, isHard = false, isEffect = false, gameDirOpt)
  def demoSbAioHard(): Unit = demoSb(isAIO = true, isHard = true, isEffect = false, gameDirOpt)
  def demoSbEffect(): Unit = demoSb(isAIO = false, isHard = false, isEffect = true, gameDirOpt)
  def demoSbAll(): Unit = { demoSbFirst(); demoSbAio(); demoSbAioHard(); demoSbEffect() }

  argName match {
    case ".demo.sb" => demoSbAll()
    case ".diff" => diff(checkDir(absPath(cliArgs(1))), checkDir(absPath(cliArgs(2))), checkDirAvailable(absPath(cliArgs(3))))
    case ".diff.into" =>
      val out = checkDirAvailable(absPath(cliArgs(3)))
      val from = checkDir(absPath(cliArgs(1)))
      for (d <- os.list(checkDir(absPath(cliArgs(2)))) if os.isDir(d)) diff(from, d, out / d.last)
    case ".search" =>
      val input = checkFileExt(absPath(cliArgs(next)), "sam")
      val outDir = checkDirAvailable(absPath(cliArgs(next + 1)))
      search(gamePakDirOpt, input, outDir)
    case ".setup" => if (setup) println("All modding tools have been set up")
    case ".setup.vscode" => vscode(if (cliArgs.length == 2) Some(absPath(cliArgs(1))) else None)
    case ".toml" | ".toml.all" => 
      val outDir = checkDirAvailable(absPath(cliArgs(next)))
      checkPatchesDir()
      setUAssetGUIConfigAndRun(toml(gamePakDirOpt, outDir, argName == ".toml"))
    case _ =>
      if (argName.head == '.') exit(-1, s"Unrecognized command $argName")
      checkPatchesDir()
      val modName = argName
      var _dryRun = false
      var _includePatches = false
      var _noCodePatching = false
      var _ultraCompression = false
      def redundant(option: String): Nothing = exit(-1, s"Redundant option $option")
      for (i <- next until cliArgs.length) {
        cliArgs(i) match {
          case `dryRun` =>
            if (_dryRun) redundant(dryRun)
            _dryRun = true
          case `includePatches` =>
            if (_includePatches) redundant(includePatches)
            _includePatches = true
          case `noCodePatching` =>
            if (_noCodePatching) redundant(noCodePatching)
            _noCodePatching = true
          case `ultraCompression` =>
            if (_ultraCompression) redundant(ultraCompression)
            _ultraCompression = true
            modExt = "7z"
          case arg => exit(-1, s"Unrecognized $arg")
        }
      }
      setUAssetGUIConfigAndRun(generateMod(addToFilePatches = false, Some(modName), gamePakDirOpt, 
                                           disableFilePatching = false, _noCodePatching, _dryRun, _includePatches))
  }
  println("... done!")
}

run()