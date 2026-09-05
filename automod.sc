import com.fasterxml.jackson.core.util.{DefaultIndenter, DefaultPrettyPrinter}
import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper, ObjectWriter}
import com.fasterxml.jackson.databind.node.{JsonNodeFactory, ArrayNode, BooleanNode, DoubleNode, IntNode, NullNode, ObjectNode, TextNode}
import com.fasterxml.jackson.dataformat.toml.TomlMapper
import com.fasterxml.jackson.core.`type`.TypeReference
import com.github.jpabscale.uasset4j.api.UAssetService
import com.github.jpabscale.uasset4j.exporttypes.AnimSequenceExport
import com.github.jpabscale.uasset4j.unrealtypes.EngineVersion
import com.jayway.jsonpath
import java.util.{EnumSet, Map => JMap}
import java.util.concurrent.ConcurrentHashMap
import scala.beans.BeanProperty
import scala.collection.immutable.{ListMap, TreeMap, TreeSet}
import scala.collection.mutable.HashMap
import scala.collection.parallel.CollectionConverters._
import scala.jdk.CollectionConverters._
import scala.util.Properties

var version = "4.1.0"
val header = s"Auto Modding Script v$version"

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

val automodDir = {
  var file = new java.io.File(sourcecode.File())
  while (!new java.io.File(file, "automod.sc").exists) file = file.getParentFile
  os.Path(file.getCanonicalFile.getAbsolutePath)
}

val dataTablePath = "/Exports/0/Table/Data"
val noCodePatching = "--no-code-patching"
val dryRun = "--dry-run"
val includePatches = "--include-patches"
val ultraCompression = "--ultra-compression"
val uassetFilterSepChar = '$'

var gameId = "SB"
var testPatches = false
var maxLogs = 30
var noPar = false
var usePak = false
var jsonOutDir: os.Path = null
var licenses = Seq[os.Path]()
var cliArgs = {
  var r = args match {
    case Array("-s", _*) => args.tail
    case _ => args
  }
  var done = false
  while (!done && r.nonEmpty) {
    r match {
      case Array("-g", id, _*) =>
        gameId = id
        r = r.drop(2)
      case Array("-t", _*) =>
        testPatches = true
        r = r.tail
      case Array("--test-patches", _*) =>
        testPatches = true
        r = r.tail
      case Array("-p", _*) =>
        noPar = true
        r = r.tail
      case Array("--pak", _*) =>
        usePak = true
        r = r.tail
      case Array("--json-out", p, _*) =>
        jsonOutDir = absPath(p)
        r = r.drop(2)
      case Array("-l", num, _*) =>
        num.toIntOption match {
          case Some(n) if n > 0 => 
            maxLogs = n
            r = r.drop(2)
          case _ => exit(-1, s"$num is not a positive integer")
        }
      case Array("-c", lp, _*) =>
        val p = absPath(lp)
        if (!os.isFile(p)) exit(-1, s"$p is not a file")
        licenses = licenses :+ p
        r = r.drop(2)
      case _ => done = true
    }
  }
  r
}


def exit(code: Int, msg: String = null): Nothing = {
  Option(msg).foreach((if (code == 0) Console.out else Console.err).println(_))
  System.exit(code)
  throw new RuntimeException
}

val zipToolVersion = "25.01"
var modExt = "zip"
val usmapUrlPrefix = "https://github.com/jpabscale/automod/releases/download/usmap/"
val ttmapUrlPrefix = "https://github.com/jpabscale/automod/releases/download/ttmap/"
val autoupdateUsmaps = TreeSet[String]()

val sbGameId = "SB"
val soaGameId = "SandsOfAura"
val pal7GameId = "Pal7"
val kenaGameId = "Kena"
val wantedDeadGameId = "WDGame"
val warmSnowGameId = "WarmSnow"
val overcooked2GameId = "Overcooked2"
val bladedFuryGameId = "BladedFury"

@JsonIgnoreProperties(ignoreUnknown = true)
class Game {
  @BeanProperty var aesKey: String = ""
  @BeanProperty var contentPaks: String = ""
  @BeanProperty var directory: String = ""
  @BeanProperty var mapUri: String = ""
  @BeanProperty var repakPackOptions: String = ""
  @BeanProperty var unity: Boolean = false
  @BeanProperty var unrealEngine: String = ""
  @BeanProperty var zen: Boolean = true
}

val sbGame = {
  val g = new Game
  g.directory = ""
  g.contentPaks = s"$sbGameId/Content/Paks"
  g.unrealEngine = "4.26"
  g.mapUri = s"${usmapUrlPrefix}StellarBlade_1.4.1.usmap.7z"
  g.repakPackOptions = ""
  g.zen = true
  g
}
val soaGame = {
  val g = new Game
  g.directory = ""
  g.contentPaks = s"$soaGameId/Content/Paks"
  g.unrealEngine = "4.25"
  g.mapUri = s"${usmapUrlPrefix}SandsOfAura_1.01.25.usmap.7z"
  g.repakPackOptions = "--version V11"
  g.zen = false
  g
}
val pal7Game = {
  val g = new Game
  g.directory = ""
  g.contentPaks = s"$pal7GameId/Content/Paks"
  g.unrealEngine = "4.25"
  g.mapUri = ""
  g.repakPackOptions = ""
  g.zen = false
  g
}
val kenaGame = {
  val g = new Game
  g.directory = ""
  g.contentPaks = s"$kenaGameId/Content/Paks"
  g.unrealEngine = "4.27"
  g.mapUri = ""
  g.repakPackOptions = ""
  g.zen = false
  g
}
val wantedDeadGame = {
  val g = new Game
  g.directory = ""
  g.contentPaks = s"$wantedDeadGameId/Content/Paks"
  g.unrealEngine = "4.27"
  g.mapUri = ""
  g.repakPackOptions = ""
  g.zen = true
  g
}

// Unity games use asset4j instead of UAssetAPI; contentPaks is the bundle/data directory
// and mapUri is the ttmap release URL (empty for embedded-tree games like Bladed Fury).
val warmSnowGame = {
  val g = new Game
  g.directory = ""
  g.contentPaks = s"${warmSnowGameId}_Data"
  g.unity = true
  g.mapUri = s"${ttmapUrlPrefix}warmsnow_3.1.0.1.ttmap"
  g.repakPackOptions = ""
  g.zen = false
  g
}
val overcooked2Game = {
  val g = new Game
  g.directory = ""
  g.contentPaks = s"${overcooked2GameId}_Data"
  g.unity = true
  g.mapUri = s"${ttmapUrlPrefix}overcooked2_66.678012.ttmap"
  g.repakPackOptions = ""
  g.zen = false
  g
}
val bladedFuryGame = {
  val g = new Game
  g.directory = ""
  g.contentPaks = "chopghost_Data/StreamingAssets"
  g.unity = true
  g.mapUri = ""
  g.repakPackOptions = ""
  g.zen = false
  g
}

// Tool version pins; retoc/repak run in-process via zenpak4j and have no pins anymore.
// IgnoreUnknown keeps older .config.json files (which still carry the removed keys) loading.
@JsonIgnoreProperties(ignoreUnknown = true)
class Tools {
  @BeanProperty var fmodel: String = "5c0387f8eca2be04d1947c971af30eb67e808c4b"
  @BeanProperty var jd: String = "2.5.0"
}

@JsonIgnoreProperties(ignoreUnknown = true)
class Config {
  @BeanProperty var games: java.util.TreeMap[String, Game] = null
  @BeanProperty var tools: Tools = null
  lazy val game: Game = Option(games.get(gameId)) match {
    case Some(g) => g
    case _ => exit(-1, s"Could not find configuration for game identifier: $gameId")
  }
}

def initConfig: Config = {
  val r = new Config
  r.games = new java.util.TreeMap[String, Game]
  r.games.put(sbGameId, sbGame)
  r.games.put(soaGameId, soaGame)
  r.games.put(pal7GameId, pal7Game)
  r.games.put(kenaGameId, kenaGame)
  r.games.put(wantedDeadGameId, wantedDeadGame)
  r.games.put(warmSnowGameId, warmSnowGame)
  r.games.put(overcooked2GameId, overcooked2Game)
  r.games.put(bladedFuryGameId, bladedFuryGame)
  r.tools = new Tools
  r
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

type RawScriptPatches = TreeMap[OrderedString, os.Path]

val rawPatchExtensions = Vector("toml", "patch", "sc", "js", "ts", "lua", "py", "kt")
val rawPatchExtensionOrder = Map("toml" -> 0, "patch" -> 1, "sc" -> 2, "kt" -> 3, "js" -> 4, "ts" -> 5, "lua" -> 6, "py" -> 7)

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

def objectMapper: ObjectMapper = new ObjectMapper()

def writeConfig(config: Config): Option[Config] = {
  val oldOpt = if (os.exists(configPath)) Some(readConfig(configPath)) else None
  objectWriter.writeValue(configPath.toIO, config)
  println(s"Wrote $configPath")
  println()
  oldOpt
}

def readConfig(path: os.Path): Config = new ObjectMapper().readValue(path.toIO, classOf[Config])

def getTimestamp(): String = {
  import java.time.{ZonedDateTime, ZoneOffset}
  import java.time.format.DateTimeFormatter
  DateTimeFormatter.ISO_INSTANT.format(ZonedDateTime.now(ZoneOffset.UTC)).replace(":", "-").replace(".", "-")
}

def absPath(p: os.Path): String = p.toString
def absPath(p: String): os.Path = os.Path(new java.io.File(p).getCanonicalFile.getAbsolutePath)

val workingDir = os.pwd
var patchesDir = if (testPatches) workingDir / "patches-test" else workingDir / "patches"
val configPath = workingDir / ".config.json"
def getLogDir(relOpt: Option[String]): os.Path = {
  relOpt match {
    case Some(rel) => 
      logDir / rel
    case _ =>
      var d = workingDir / ".log"
      os.makeDir.all(d)
      for (p <- os.list(d).filter(os.isDir).sortWith((p1, p2) => p1.toIO.lastModified >= p2.toIO.lastModified).drop(maxLogs)) 
      os.remove.all(p)
      d / (if (cliArgs.nonEmpty) s"${cliArgs.head}-${getTimestamp()}" else s"${getTimestamp()}")
  }
}
var logDir = getLogDir(None)
val localAppData = if (osKind.isWin) os.Path(System.getenv("LOCALAPPDATA")) else os.home / ".local" / "share"
val userName = if (osKind.isWin) System.getenv("USERNAME") else new String(os.proc("whoami").call().out.bytes).trim
val config = {
  var ok = true
  val r: Config = if (os.exists(configPath)) try {
    readConfig(configPath)
  } catch {
    case _: Throwable => 
      ok = false
      initConfig
  } finally {
    if (!ok && os.exists(configPath)) {
      val backup = configPath / os.up / s".config-${getTimestamp()}.json"
      os.move.over(configPath, backup)
      println(s"Could not load $configPath; backed up to ${absPath(backup)}")
      println()
    }
  } else initConfig
  r
}

val unityMode = config.game.unity

val toolsDir = automodDir / "tools"
val usmapDir = toolsDir / "usmap"

var fmodelSha = config.tools.fmodel
var fmodelShortSha = fmodelSha.substring(0, 7)
val jdVersion = config.tools.jd
// asset4j's version (from project.scala's `using dep` directive) is also the GitHub
// release tag for ttmapgen.jar — keep them in lockstep by deriving, not hardcoding.
val asset4jVersion = {
  val src = os.read(automodDir / "project.scala")
  """.*com\.github\.jpabscale:asset4j:(\S+)""".r
    .findFirstMatchIn(src).map(_.group(1))
    .getOrElse(exit(-1, "Could not find asset4j version in project.scala"))
}
val ueVersion = config.game.unrealEngine
val ueVersionCode = s"UE${ueVersion.replace('.', '_')}"

val supportedVersions = Set("UE4_25","UE4_26","UE4_27","UE5_0","UE5_1","UE5_2","UE5_3","UE5_4","UE5_5","UE5_6","UE5_7")
if (ueVersion.nonEmpty && !supportedVersions.contains(ueVersionCode)) {
  throw new IllegalArgumentException(s"Unsupported engine version: $ueVersion (code=$ueVersionCode). Supported: ${supportedVersions.toSeq.sorted.mkString(", ")}")
}

val usmapUri = config.game.mapUri
val usmapFilename = { 
  var r = usmapUri.substring(usmapUri.lastIndexOf('/') + 1, usmapUri.length)
  if (r.endsWith(".7z")) r = r.substring(0, r.lastIndexOf('.'))
  r
}
val usmapPath = usmapDir / (if (usmapFilename.isEmpty || usmapFilename == "Mappings.usmap") s"$gameId.usmap" else usmapFilename)

// Unity games reuse mapUri for the ttmap (the per-game schema artifact). Unlike the
// usmap, the ttmap is gzip-wrapped JSON with a `.ttmap` extension (content is gzip;
// Ttmap.read auto-detects it, so the extension doesn't need to say .json.gz).
val ttmapDir = toolsDir / "ttmap"
val ttmapUri = config.game.mapUri
val ttmapFilename = {
  var r = ttmapUri.substring(ttmapUri.lastIndexOf('/') + 1, ttmapUri.length)
  if (r.endsWith(".7z")) r = r.substring(0, r.lastIndexOf('.'))
  r
}
val ttmapPath = ttmapDir / (if (ttmapFilename.isEmpty) s"$gameId.ttmap" else ttmapFilename)

def fmodelUrl(sha: String): String = s"https://github.com/4sval/FModel/releases/download/qa/$sha.zip"
val jdUrlPrefix = s"https://github.com/josephburnett/jd/releases/download/v$jdVersion"
val z7rUrl = s"https://github.com/ip7z/7zip/releases/download/$zipToolVersion/7zr.exe"
val z7UrlPrefix = s"https://github.com/ip7z/7zip/releases/download/$zipToolVersion"
def vsixUrl = s"https://github.com/jpabscale/automod/releases/download/automod-vsix/automod-vscode-$version.vsix"

// copy a tool binary + its native deps (retoc needs its Oodle codec .so beside it; without it retoc
// tries a network download that fails offline)
def copyTool(exe: os.Path, destDir: os.Path): os.Path = {
  os.makeDir.all(destDir)
  os.copy.over(exe, destDir / exe.last)
  val oodle = exe / os.up / "liboo2corelinux64.so.9"
  if (os.exists(oodle)) os.copy.over(oodle, destDir / "liboo2corelinux64.so.9")
  destDir / exe.last
}
val fmodelExe = toolsDir / "FModel.exe"
val jdExe = toolsDir / (if (osKind.isWin) "jd.exe" else "jd")
val ttmapgenExe = toolsDir / "ttmapgen.jar"
val zipExe = toolsDir / "7z" / (if (osKind.isWin) "7z.exe" else "7zz")
def automodVsix = automodDir / "vscode" / s"automod-vscode-$version.vsix"
val repakPackOptions: Seq[os.Shellable] = {
  val opts = config.game.repakPackOptions.trim
  if (opts.isEmpty) Vector.empty[os.Shellable]
  else {
    var r = Vector.empty[os.Shellable]
    for (opt <- opts.split(' ') if opt.trim.nonEmpty) r :+= opt
    r
  }
}
val automodGameCacheDir = automodDir / ".cache" / (if (usmapFilename.nonEmpty) usmapFilename.replace(".usmap", "") else gameId) / gameId
val cacheDir = workingDir / ".cache"
val tempDir = localAppData / "Temp" / "automod"
val setupVscodeDir = tempDir / ".setup.vscode.dir"
lazy val dotnet = if (os.exists(os.home / ".dotnet" / "dotnet")) absPath(os.home / ".dotnet" / "dotnet") else "dotnet"
lazy val javaExe = if (os.exists(os.Path(System.getProperty("java.home")) / "bin" / "java")) absPath(os.Path(System.getProperty("java.home")) / "bin" / "java") else "java"

val discardProcessOutput = new os.ProcessOutput {
  def redirectTo: ProcessBuilder.Redirect = ProcessBuilder.Redirect.DISCARD
  def processOutput(out: => os.SubProcess.OutputStream): Option[Runnable] = None
}

def sha256(path: os.Path, length: Int = 64): String =
  java.security.MessageDigest.getInstance("SHA-256").digest(os.read.bytes(path)).
    take(length).map(String.format("%02x", _)).mkString 

def download(uri: String, sha256TitleOpt: Option[String] = None): Option[os.Path] = {
  os.makeDir.all(tempDir)
  val cacheName = java.util.Base64.getEncoder().encodeToString(uri.getBytes(java.nio.charset.StandardCharsets.UTF_8))
  val cachePath = tempDir / cacheName
  val dest = toolsDir / uri.substring(uri.lastIndexOf('/') + 1)
  var redownload = !os.exists(cachePath)
  val cacheSha256Path = tempDir / s"$cacheName.sha256"
  val (cacheSha256LastModified, cached) = if (os.exists(cacheSha256Path)) (cacheSha256Path.toIO.lastModified, os.read(cacheSha256Path))
                                          else (0L, "")
  val dayMillis = 86400000
  if (sha256TitleOpt.nonEmpty && System.currentTimeMillis - cacheSha256LastModified > dayMillis * 7) {
    println(s"Checking for updated $uri ...")
    download(s"$uri.sha256") match {
      case Some(p) =>
        println()
        val pValue = os.read(p)
        os.remove.all(p)
        if (pValue != cached) {
          redownload = true
          os.write.over(cacheSha256Path, pValue)
        }
      case _ => println()
    }
  }
  if (cachePath.toIO.length <= 1024) {
    redownload = true
  }
  if (redownload) {
    sha256TitleOpt.foreach(println)
    os.remove.all(cachePath)
    if (uri.startsWith("https://")) os.proc("curl", "-JLo", cachePath, uri).call(cwd = toolsDir, stdout = os.Inherit, stderr = os.Inherit)
    else if (uri.startsWith("file://")) os.copy.over(os.Path(new java.io.File(new java.net.URI(uri)).getCanonicalFile.getAbsolutePath), cachePath)
    else if (os.exists(automodDir / os.RelPath(uri))) os.copy.over(automodDir / os.RelPath(uri), cachePath)
  } else {
    if (sha256TitleOpt.nonEmpty) return None
  }
  if (cachePath.toIO.length == 9) {
    os.remove.all(cachePath)
    return None
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

def init(gameDirOpt: Option[os.Path]): Boolean = {
  var setup = true

  os.makeDir.all(tempDir)
  os.write.over(tempDir / ".automod.dir", automodDir.toString)
  
  os.makeDir.all(usmapDir)

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


  if (usmapUri.nonEmpty && !os.exists(usmapPath) && !unityMode) {
    setup = false
    println(s"Setting up $usmapPath ...")
    val f = downloadCheck(usmapUri)
    if (f.ext == "7z") os.proc(zipExe, "x", f.last).call(cwd = f / os.up, stdout = os.Inherit, stderr = os.Inherit)
    os.move.over(f / os.up / usmapFilename, usmapPath)
    os.remove.all(f)
    println()
  }

  // Unity: mapUri is the ttmap URL (gzip-wrapped JSON with a .ttmap extension); download
  // it as-is (no 7z extract).
  if (ttmapUri.nonEmpty && !os.exists(ttmapPath) && unityMode) {
    setup = false
    os.makeDir.all(ttmapDir)
    println(s"Setting up $ttmapPath ...")
    val f = downloadCheck(ttmapUri)
    os.move.over(f, ttmapPath)
    println()
  }

  if (osKind.isWin && !os.exists(fmodelExe)) {
    setup = false
    println(s"Setting up FModel @$fmodelShortSha in $toolsDir ...")
    val devSha = new String(os.proc("curl", "-s", "-H", "Accept: application/vnd.github.VERSION.sha", 
      "https://api.github.com/repos/4sval/FModel/commits/dev").call().out.bytes).trim
    val fmodelZip = download(fmodelUrl(devSha)) match {
      case Some(f) => 
        fmodelSha = devSha
        fmodelShortSha = fmodelSha.substring(0, 7)
        config.tools.fmodel = devSha
        writeConfig(config)
        f
      case _ => downloadCheck(fmodelUrl(fmodelSha))
    }
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

  if (!os.exists(ttmapgenExe)) {
    setup = false
    println(s"Setting up ttmapgen v$asset4jVersion in $toolsDir ...")
    downloadCheck(s"https://github.com/jpabscale/asset4j/releases/download/$asset4jVersion/ttmapgen.jar")
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
private var _rawJsonPatches = emptyFilePatches
private var _rawScriptPatches: RawScriptPatches = TreeMap.empty
// IL-patch (.il.toml) rules keyed by target asset, applied via the dnlib4j IL engine (ilengine.sc).
private var _ilPatches: RawScriptPatches = TreeMap.empty
// Copy-from patches: `$dest!$source.toml` — copy the SOURCE asset, auto-rename its package
// identity to dest, then apply the toml body and ship at dest. Keyed destName -> sourceName.
private var _copyFromPatches = Map.empty[String, String]
// Unity first-class class-scoped patches: `<ClassName>@<bundle>.toml` -> (className, .@ tree).
// Keyed by the target bundle (e.g. `resources.assets`); applied via the targeted per-object
// decode (no whole-file decode), with `.@` paths scoped relative to each object's Data.
private var _rawClassTomlPatches: Map[String, List[(String, UAssetPropertyChanges)]] = Map.empty
private var _rawClassScriptPatches: Map[String, List[(String, os.Path)]] = Map.empty

def rawJsonPatches: FilePatches = {
  if (!patchesInitialized) patches
  _rawJsonPatches
}

def rawScriptPatches: RawScriptPatches = {
  if (!patchesInitialized) patches
  _rawScriptPatches
}

def ilPatches: RawScriptPatches = {
  if (!patchesInitialized) patches
  _ilPatches
}

def rawClassTomlPatches: Map[String, List[(String, UAssetPropertyChanges)]] = {
  if (!patchesInitialized) patches
  _rawClassTomlPatches
}

def rawClassScriptPatches: Map[String, List[(String, os.Path)]] = {
  if (!patchesInitialized) patches
  _rawClassScriptPatches
}

def langForRawExt(ext: String): patchlet.Lang = ext.toLowerCase match {
  case "sc" => patchlet.Lang.Scala
  case "js" => patchlet.Lang.Js
  case "ts" => patchlet.Lang.Typescript
  case "py" => patchlet.Lang.Python
  case "lua" => patchlet.Lang.Lua
  case "kt" => patchlet.Lang.Kotlin
  case _ => exit(-1, s"Unsupported raw script patch extension: $ext")
}

val updatedPatches = new ConcurrentHashMap[(String, String, String), JsonNode]

def updatePatch(uassetName: String, objName: String, property: String, valuePair: ValuePair): Unit = {
  val key = (uassetName, objName, property)
  updatedPatches.put(key, valuePair.newValueOpt.get)
}

def updatePatches(uassetName: String, objName: String, properties: Iterable[(String, ValuePair)]): Unit = {
  for ((property, valuePair) <- properties) updatePatch(uassetName, objName, property, valuePair)
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
  var rawJsonMap = if (_rawJsonPatches == null) emptyFilePatches else _rawJsonPatches
  var rawScriptMap = if (_rawScriptPatches == null) TreeMap.empty[OrderedString, os.Path] else _rawScriptPatches
  var ilPatchMap = if (_ilPatches == null) TreeMap.empty[OrderedString, os.Path] else _ilPatches
  var classTomlMap: Map[String, List[(String, UAssetPropertyChanges)]] = Map.empty
  var classScriptMap: Map[String, List[(String, os.Path)]] = Map.empty
  var copyFromMap = _copyFromPatches

  def rec(path: os.Path, isRoot: Boolean = false): Unit =   {
    if (!isRoot && path.last.headOption == Some('.')) {
      println(s"Ignoring $path ...")
      return
    }
    for (p <- os.list(path).sortWith((p1, p2) =>
      if (os.isDir(p1) && os.isDir(p2)) p1.last <= p2.last
      else if (os.isDir(p1)) false
      else if (os.isDir(p2)) true
      else {
        val r1 = rawPatchExtensionOrder.getOrElse(p1.ext.toLowerCase, Int.MaxValue)
        val r2 = rawPatchExtensionOrder.getOrElse(p2.ext.toLowerCase, Int.MaxValue)
        if (r1 != r2) r1 < r2 else p1.last <= p2.last
      }
    )) {
      if (os.isDir(p)) {
        rec(p)
      } else if (os.isFile(p)) {
        if (p.last.startsWith(".")) {
          println(s"Ignoring $path ...")
        } else {
          p.ext.toLowerCase match {
            case ext if rawPatchExtensions.contains(ext) =>
              val uassetName = p.baseName
              logPatch(uassetName, s"Loading $p ...", console = true)
              var relPath = p.relativeTo(patchesDir).toString
              if (osKind.isWin) relPath = relPath.replace('/' , '\\')
              if (ext == "toml" && uassetName.endsWith(".json")) {
                rawJsonMap = applyChanges(relPath, rawJsonMap, uassetName, tomlFilePatches(p))
              } else if (ext == "toml" && uassetName.contains("@")) {
                val parts = uassetName.split("@", 2)
                val className = parts(0)
                val bundle = parts(1)
                val tree = tomlFilePatches(p)
                classTomlMap = classTomlMap + (bundle -> (classTomlMap.getOrElse(bundle, Nil) :+ (className, tree)))
              } else if (ext == "toml" && uassetName.contains('!')) {
                // Copy-from patch: `$dest!$source.toml` — the toml body patches the SOURCE asset
                // copied (and auto-renamed to dest); the result ships at the dest path.
                val parts = uassetName.split("!", 2)
                if (parts.length != 2 || parts(0).isEmpty || parts(1).isEmpty)
                  exit(-1, s"copy-from patch name must be `$$dest!$$source`: $p")
                if (!parts(0).contains(uassetFilterSepChar) || !parts(1).contains(uassetFilterSepChar))
                  exit(-1, s"copy-from patch dest/source must be $$-encoded paths: $p")
                copyFromMap = copyFromMap + (parts(0) -> parts(1))
                map = applyChanges(relPath, map, parts(0), tomlFilePatches(p))
              } else if (ext == "toml" && uassetName.endsWith(".il")) {
                // IL-patch rule file: target assembly is the name minus the trailing ".il".
                val targetName = uassetName.stripSuffix(".il")
                ilPatchMap = ilPatchMap + (OrderedString(targetName, relPath) -> p)
              } else if (ext == "toml") {
                map = applyChanges(relPath, map, uassetName, tomlFilePatches(p))
              } else if (ext == "patch") {
                map = applyChanges(relPath, map, uassetName, jdFilePatches(p)())
              } else if (uassetName.contains("@")) {
                val parts = uassetName.split("@", 2)
                val className = parts(0)
                val bundle = parts(1)
                classScriptMap = classScriptMap + (bundle -> (classScriptMap.getOrElse(bundle, Nil) :+ (className, p)))
              } else {
                rawScriptMap = rawScriptMap + (OrderedString(uassetName, relPath) -> p)
              }
              logPatch(uassetName, "", console = false)
            case _ =>
          }
        }
      }
    }
  }

  if (os.isDir(patchesDir)) rec(patchesDir, isRoot = true)
  println()
  _patches = map
  _rawJsonPatches = rawJsonMap
  _rawScriptPatches = rawScriptMap
  _ilPatches = ilPatchMap
  _rawClassTomlPatches = classTomlMap
  _rawClassScriptPatches = classScriptMap
  _copyFromPatches = copyFromMap
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

  val data = ast.json[JsonNode].at(dataTablePath) match {
    case d: ArrayNode => d
    case _ => return
  }
  val origData = origAst.json[JsonNode].at(dataTablePath).asInstanceOf[ArrayNode]
  val dataMap = toDataMap(data)
  def updateProperties(obj: uassetapi.Struct, orig: JsonNode, properties: PropertyChanges): Unit = {
    for ((property, valueOldValuePair) <- properties) {
      var value = valueOldValuePair.newValueOpt.orNull
      value match {
        case v: TextNode =>
          def code(codePrefix: String, lang: patchlet.Lang): Unit =
            value = patchlet.evalStructProperty(lang, uassetName, addToFilePatches, dataMap, 
                                                v.textValue.substring(codePrefix.length), obj, property, orig, ast, origAst)
          patchlet.getKeyPrefix(v.textValue) match {
            case Some(patchlet.Constants.codePrefixScala) => code(patchlet.Constants.codePrefixScala, patchlet.Lang.Scala)
            case Some(patchlet.Constants.codePrefixJavascript) => code(patchlet.Constants.codePrefixJavascript, patchlet.Lang.Js)
            case Some(patchlet.Constants.codePrefixPython) => code(patchlet.Constants.codePrefixPython, patchlet.Lang.Python)
            case Some(patchlet.Constants.codePrefixKotlin) => code(patchlet.Constants.codePrefixKotlin, patchlet.Lang.Kotlin)
            case _ =>
          }
        case _ =>
      }
      obj.setJson(property, value)
    }
  }
  def applyKfcs(i: Int, obj: uassetapi.Struct, origObj: uassetapi.Struct, kfcs: Iterable[patchlet.KeyFilteredChanges]): Unit = {
    for (kfc <- kfcs) kfc.applyStructChanges(s"$dataTablePath/$i", obj.value, origObj.value)
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

val logs = new java.util.concurrent.ConcurrentHashMap[String, java.io.BufferedWriter]

def logPatch(uassetName: String, l: String, console: Boolean): Unit = {
  val line = if (l.length > 1024) l.substring(0, 1024) else l
  if (console) println(line)
  val logName = s"${uassetName.replace('/', uassetFilterSepChar)}.log"
  val p = logDir / logName
  val key = absPath(p)
  var q = logs.get(absPath(logDir / logName))
  if (q == null) {
    os.makeDir.all(logDir)
    q = new java.io.BufferedWriter(new java.io.FileWriter(key))
    logs.put(key, q)
  }
  q.write(s"$line${util.Properties.lineSeparator}")
}

def logFlush(uassetName: String): Unit = {
  val key = absPath(logDir / s"${uassetName.replace('/', uassetFilterSepChar)}.log")
  val q = logs.get(key)
  if (q != null) {
    logs.remove(key)
    q.flush()
    q.close()
  }
}

def logFlush(): Unit = {
  for (w <- logs.values.asScala) try w.flush() finally w.close()
  logs.clear()
}

def checkPatchesDir(): Unit = if (!os.isDir(patchesDir)) exit(-1, s"Missing directory: $patchesDir")

def retocPakCmd(exe: os.Path, args: os.Shellable*): Vector[os.Shellable] = {
  var r = Vector[os.Shellable](exe, "-g", gameId)
  val aesKey = config.game.aesKey
  if (aesKey.size > 1) {
    r = r :+ "--aes-key"
    r = r :+ (if (aesKey.head == '0' && Character.toLowerCase(aesKey(1)) == 'x') aesKey else s"0x$aesKey")
  }
  r = r ++ args
  r
}

def extractAssetPath(name: String, gamePakDir: os.Path, workDir: os.Path): os.Path = {
  val bare = name.substring(name.lastIndexOf(uassetFilterSepChar) + 1)
  // fresh temp dir per call (the kotlin port does the same): callers extract several
  // assets into a shared workDir and keep every path alive until after conversion
  val retocPakCopyDir = os.temp.dir()
  val filterName = if (config.game.zen) s"${name.replace(uassetFilterSepChar, '/')}.uasset" else name.replace(uassetFilterSepChar, '/')
  val aesKeyOpt = Some(config.game.aesKey).filter(_.nonEmpty)
  try {
    if (config.game.zen) {
      // positional args: Scala can't see Kotlin parameter names
      com.github.jpabscale.zenpak4j.ZenPakService.INSTANCE.retoc_to_legacy(
        List(gamePakDir.toNIO).asJava, retocPakCopyDir.toNIO,
        com.github.jpabscale.zenpak4j.retoc.EngineVersion.valueOf(ueVersionCode),
        filterName, aesKeyOpt.orNull, gameId)
    } else {
      // (pakFiles, outputDir, strip_prefix, aes_key, game_id, include, verbose)
      com.github.jpabscale.zenpak4j.ZenPakService.INSTANCE.repak_unpack(
        os.list(gamePakDir).filter(_.ext == "pak").map(_.toNIO).toList.asJava,
        retocPakCopyDir.toNIO, "../../../", aesKeyOpt.orNull, gameId,
        List(s"**/$filterName.*", s"$filterName.*").asJava, false)
    }
  } catch { case err: Throwable => exit(-1, s"Failed to extract $filterName (${err.getClass.getName}): ${err.getMessage}") }
  if (name.contains(uassetFilterSepChar)) {
    val rel = name.replace(uassetFilterSepChar, '/').stripPrefix("/")
    val exact = retocPakCopyDir / os.RelPath(rel + ".uasset")
    if (os.isFile(exact)) exact else exit(-1, s"Failed to extract $name (exact path not found: $exact)")
  } else {
    // Bare-name resolution must be deterministic regardless of os.walk order: accumulate ALL
    // same-named matches and sort by path, never the first walk hit.
    os.walk(retocPakCopyDir).filter(p => os.isFile(p) && p.last == s"$bare.uasset")
      .toSeq.sortBy(_.toString).headOption
      .getOrElse(exit(-1, s"Failed to extract $name (no .uasset result)"))
  }
}

def extractAndDecodeAsset(name: String, gamePakDir: os.Path, workDir: os.Path): ObjectNode = {
  val uasset = extractAssetPath(name, gamePakDir, workDir)
  UAssetService.toJsonNode(uasset.toNIO, EngineVersion.FromString(s"VER_$ueVersionCode"), usmapPath.toString)
}

/**
 * A single `.retarget` config job (one `[[retarget]]` block). `base` present => merge; absent =>
 * standalone. Every element is required (no defaults); `retime` is merge-only.
 */
case class RetargetJob(
  anim: String,
  base: Option[String],
  from: String,
  to: String,
  mode: String,
  bakeFormat: String,
  eliminate: Boolean,
  retime: Option[Int],
  as: String,
  out: String,
  bones: String)

val retargetModes = Set("delta", "scale", "copy")
val retargetBakeFormats = Set("float96", "fixed48")
val boneOverrideValues = Set("freeze", "copy", "delta", "scale")

def boneOverrideOf(v: String): retargeter.BoneOverride = v match {
  case "freeze" => retargeter.BoneOverride(null, true)
  case "copy"   => retargeter.BoneOverride(retargeter.RetargetMode.COPY, false)
  case "delta"  => retargeter.BoneOverride(retargeter.RetargetMode.DELTA, false)
  case "scale"  => retargeter.BoneOverride(retargeter.RetargetMode.SCALE, false)
  case other    => throw new IllegalArgumentException(s"unknown bone override value: $other")
}

/**
 * Parse a `.retarget <config.toml>`. Schema: `[[retarget]]` jobs (all elements required; `base`
 * present => merge) + `[bones.<id>]` configs mapping bone name -> "freeze"|"copy"|"delta"|"scale"
 * (per-bone overrides of the job `mode`; a bone not listed uses the job's mode). Structural
 * validation (required fields, value validity, merge/standalone consistency, `bones` id and value
 * validity) runs HERE — all errors reported, nothing executes. Bone-name-in-skeleton and output
 * collisions are checked separately (they need the target skeletons loaded).
 */
def parseRetargetConfig(path: os.Path): (Vector[RetargetJob], Map[String, Map[String, retargeter.BoneOverride]]) = {
  val toml: JMap[String, Object] =
    new TomlMapper().readValue(path.toIO, new TypeReference[JMap[String, Object]] {})
  val errors = scala.collection.mutable.ArrayBuffer.empty[String]

  val bonesRaw = Option(toml.get("bones"))
    .map(_.asInstanceOf[JMap[String, JMap[String, String]]])
    .getOrElse(java.util.Collections.emptyMap[String, JMap[String, String]]())
  val boneConfigs: Map[String, Map[String, retargeter.BoneOverride]] = bonesRaw.asScala.map {
    case (id, m) =>
      val parsed = scala.collection.mutable.HashMap.empty[String, retargeter.BoneOverride]
      for ((bone, v) <- m.asScala) {
        if (boneOverrideValues.contains(v)) parsed.put(bone, boneOverrideOf(v))
        else errors += s"$path: [bones.$id] bone '$bone' has invalid value '$v' (expected freeze|copy|delta|scale)"
      }
      id -> parsed.toMap
  }.toMap

  val jobsRaw = Option(toml.get("retarget"))
    .map(_.asInstanceOf[java.util.List[JMap[String, Object]]])
    .getOrElse {
      errors += s"$path: no [[retarget]] jobs found"
      java.util.Collections.emptyList[JMap[String, Object]]()
    }
  val jobs = jobsRaw.asScala.zipWithIndex.map { case (j, idx) =>
    val where = s"$path: [[retarget]] #${idx + 1}"
    def str(k: String): Option[String] = Option(j.get(k)).map(_.toString)
    def req(k: String): Option[String] = str(k) match {
      case Some(v) if v.nonEmpty => Some(v)
      case _ => errors += s"$where: missing required '$k'"; None
    }
    val anim = req("anim")
    val from = req("from")
    val to = req("to")
    val mode = req("mode")
    val bakeFormat = req("bake-format")
    val as = req("as")
    val out = req("out")
    val bones = req("bones")
    val base = str("base")
    val retime = str("retime") match {
      case Some(v) => v.toIntOption match {
        case Some(n) => Some(n)
        case None => errors += s"$where: 'retime' must be an integer"; None
      }
      case None => None
    }
    val eliminate = str("eliminate") match {
      case Some("true") => true
      case Some("false") => false
      case Some(v) => errors += s"$where: 'eliminate' must be true|false (got '$v')"; false
      case None => errors += s"$where: missing required 'eliminate'"; false
    }
    mode.foreach(m => if (!retargetModes.contains(m)) errors += s"$where: 'mode' must be delta|scale|copy (got '$m')")
    bakeFormat.foreach(b => if (!retargetBakeFormats.contains(b)) errors += s"$where: 'bake-format' must be float96|fixed48 (got '$b')")
    if (base.isEmpty && retime.isDefined) errors += s"$where: 'retime' requires 'base' (merge job)"
    bones.foreach(b => if (!boneConfigs.contains(b)) errors += s"$where: 'bones' references undefined [bones.$b]")
    RetargetJob(
      anim.getOrElse(""), base, from.getOrElse(""), to.getOrElse(""),
      mode.getOrElse(""), bakeFormat.getOrElse(""), eliminate, retime,
      as.getOrElse(""), out.getOrElse(""), bones.getOrElse(""))
  }.toVector

  if (errors.nonEmpty) {
    errors.foreach(e => System.err.println(e))
    exit(-1, s"$path: retarget config failed validation (${errors.size} error(s)) — nothing was run")
  }
  (jobs, boneConfigs)
}

def renameAsset(exp: ObjectNode, tree: ObjectNode, targetPath: String): Unit = {  val oldName = exp.get("ObjectName").asText
  val newName = targetPath.substring(targetPath.lastIndexOf('/') + 1)
  // The retarget output must ship as a NEW package at targetPath — set the FolderName too, so the
  // written asset needs no post-processing rename (rename_wave_full.sc used to do this).
  tree.put("FolderName", targetPath)
  val nameMap = tree.get("NameMap").asInstanceOf[ArrayNode]
  for (i <- 0 until nameMap.size) {
    val p = nameMap.get(i).asText
    if (p.endsWith("/" + oldName)) nameMap.set(i, TextNode.valueOf(targetPath))
    else if (p == oldName) nameMap.set(i, TextNode.valueOf(newName))
  }
  val existing = nameMap.asScala.map(_.asText).toSet
  // FName.FromStringFragments parses a trailing `_<digits>` as base+number, so the NameMap must
  // hold the STRIPPED base (`..._JumpAttack2` for `..._JumpAttack2_1`) or serialization throws
  // DummyFNameSerializationException. Add order matters (FName indices) and matches the wire scripts.
  val strippedBase = newName.replaceFirst("(_[0-9]+)$", "")
  if (!existing.contains(strippedBase)) nameMap.add(strippedBase)
  if (!existing.contains(newName)) nameMap.add(newName)
  if (!existing.contains(targetPath)) nameMap.add(targetPath)
  exp.put("ObjectName", newName)
}

/** Stored `$`-encoded patch path -> UE game path (`$SB$Content$Art$X` -> `/Game/Art/X`). */
def gamePathOf(name: String): String = {
  val stored = name.stripPrefix("$").replace(uassetFilterSepChar, '/')
  val idx = stored.indexOf("/Content/")
  if (idx >= 0) "/Game/" + stored.substring(idx + "/Content/".length)
  else "/" + stored
}

/**
 * Auto-rename a copy-from asset's package identity from [src] to [dest] (both `$`-encoded paths).
 * Renames every export whose ObjectName == the source's bare name, the exact source package-path
 * and bare-name NameMap entries, and adds the dest names (incl. the `_<digits>`-stripped base).
 * References to OTHER assets (e.g. an AnimResourcePath pointing at the source's animation) are left
 * untouched — those are patched deliberately in the toml body.
 */
def autoRenameCopiedAsset(tree: ObjectNode, src: String, dest: String): Unit = {
  val srcBare = src.substring(src.lastIndexOf(uassetFilterSepChar) + 1)
  val destBare = dest.substring(dest.lastIndexOf(uassetFilterSepChar) + 1)
  val srcGame = gamePathOf(src)
  val destGame = gamePathOf(dest)
  // The copy ships as a NEW package at dest — set the FolderName so the copied asset needs no
  // post-processing rename (rename_rm.sc/rename_show.sc used to do this for RM anims/shows).
  tree.put("FolderName", destGame)
  val exports = tree.get("Exports").asInstanceOf[ArrayNode]
  for (i <- 0 until exports.size) {
    val e = exports.get(i).asInstanceOf[ObjectNode]
    if (e.get("ObjectName") != null && e.get("ObjectName").asText == srcBare)
      e.put("ObjectName", destBare)
  }
  val nameMap = tree.get("NameMap").asInstanceOf[ArrayNode]
  for (i <- 0 until nameMap.size) {
    val p = nameMap.get(i).asText
    if (p == srcGame) nameMap.set(i, TextNode.valueOf(destGame))
    else if (p == srcBare) nameMap.set(i, TextNode.valueOf(destBare))
  }
  val existing = nameMap.asScala.map(_.asText).toSet
  for (n <- Seq(destGame, destBare, destBare.replaceFirst("(_[0-9]+)$", "")) if !existing.contains(n)) nameMap.add(n)
}

def retarget(animName: String, fromMesh: String, toMesh: String, mode: String, bakeFormat: String, eliminate: Boolean, overrides: Map[String, retargeter.BoneOverride], asPath: String, outDirOpt: Option[os.Path], gamePakDir: os.Path): Unit = {  val workDir = os.temp.dir(prefix = "retarget")
  try {
    val animUassetPath = extractAssetPath(animName, gamePakDir, workDir)
    val engine = EngineVersion.FromString(s"VER_$ueVersionCode")
    val usmap = usmapPath.toString
    val animAsset = UAssetService.load(animUassetPath.toNIO, engine, usmap)

    val fromUassetPath = extractAssetPath(fromMesh, gamePakDir, workDir)
    val toUassetPath = extractAssetPath(toMesh, gamePakDir, workDir)

    val to = UAssetService.toJsonNode(toUassetPath.toNIO, EngineVersion.FromString(s"VER_$ueVersionCode"), usmapPath.toString)
    val (skeletonPath, skeletonName) = meshSkeletonRef(to, gamePathOf(toMesh), toMesh.substring(toMesh.lastIndexOf(uassetFilterSepChar) + 1))

    val fmt = if (bakeFormat == "fixed48") com.github.jpabscale.uasset4j.animation.AnimationCompressionFormat.ACF_Fixed48NoW else com.github.jpabscale.uasset4j.animation.AnimationCompressionFormat.ACF_Float96NoW
    val srcRef = UAssetService.skeletonBoneMap(fromUassetPath.toNIO, engine, usmap)
    if (srcRef == null) exit(-1, s"$fromMesh has no reference skeleton")
    val dstRef = UAssetService.skeletonBoneMap(toUassetPath.toNIO, engine, usmap)
    if (dstRef == null) exit(-1, s"$toMesh has no reference skeleton")
    val animExport = animAsset.getExports.asScala.collectFirst { case e: AnimSequenceExport => e }
      .getOrElse(exit(-1, s"$animName is not an AnimSequence"))
    val modeEnum = mode match {
      case "copy" => retargeter.RetargetMode.COPY
      case "scale" => retargeter.RetargetMode.SCALE
      case _ => retargeter.RetargetMode.DELTA
    }
    val r = retargeter.Retargeter.retarget(animExport, srcRef, dstRef, modeEnum, 3000, fmt, eliminate, overrides)
    if (!r.applied) exit(-1, s"Keyframe retarget failed for $animName")
    val retree = UAssetService.toJsonNode(animAsset)
    val rexp = (0 until retree.get("Exports").size)
      .map(i => retree.get("Exports").get(i).asInstanceOf[ObjectNode])
      .find(_.has("CompressedTrackToSkeletonMapTable"))
      .getOrElse(exit(-1, s"$animName is not an AnimSequence"))
    val rdata = rexp.get("Data").asInstanceOf[ArrayNode]
    val rimports = retree.get("Imports").asInstanceOf[ArrayNode]
    val rnameMap = retree.get("NameMap").asInstanceOf[ArrayNode]
    swapSkeleton(rdata, rimports, rnameMap, skeletonPath, skeletonName)
    println(s"Swapping Skeleton reference to $skeletonPath")
    renameAsset(rexp, retree, asPath)
    val asset = UAssetService.fromJsonNode(retree, usmapPath.toString)

    val outDir = outDirOpt.getOrElse(workingDir / "retarget")
    // Output under <out>/SB/Content/<as-path>.uasset — the `as` path already ends with the anim
    // name, so it maps directly to the game file path. Droppable straight into a `.included` dir.
    val gameRel = asPath.stripPrefix("/Game/")
    val uasset = outDir / "SB" / "Content" / os.RelPath(gameRel + ".uasset")
    os.makeDir.all(uasset / os.up)
    asset.Write(uasset.toString)
    println(s"Wrote retargeted animation to $uasset")
  } finally {
    os.remove.all(workDir)
  }
}

/**
 * Merge-retarget: use the target character's OWN animation for the slot ([baseAnim]) as the base
 * (full body coverage: legs, weapon constraints, cosmetics) and overwrite only the tracks whose
 * bones the source ([animName]) drives. Bones the source doesn't animate stay as the base, so the
 * result keeps the target's complete pose. Output is named after the base (it replaces it).
 */
def mergeRetarget(animName: String, baseAnim: String, fromMesh: String, toMesh: String, mode: String, bakeFormat: String, eliminate: Boolean, retime: Option[Int], overrides: Map[String, retargeter.BoneOverride], asPath: Option[String], outDirOpt: Option[os.Path], gamePakDir: os.Path): Unit = {
  val workDir = os.temp.dir(prefix = "retarget")
  try {
    val animUassetPath = extractAssetPath(animName, gamePakDir, workDir)
    val baseUassetPath = extractAssetPath(baseAnim, gamePakDir, workDir)
    val fromUassetPath = extractAssetPath(fromMesh, gamePakDir, workDir)
    val toUassetPath = extractAssetPath(toMesh, gamePakDir, workDir)

    val to = UAssetService.toJsonNode(toUassetPath.toNIO, EngineVersion.FromString(s"VER_$ueVersionCode"), usmapPath.toString)
    val (skeletonPath, skeletonName) = meshSkeletonRef(to, gamePathOf(toMesh), toMesh.substring(toMesh.lastIndexOf(uassetFilterSepChar) + 1))
    val fmt = if (bakeFormat == "fixed48") com.github.jpabscale.uasset4j.animation.AnimationCompressionFormat.ACF_Fixed48NoW else com.github.jpabscale.uasset4j.animation.AnimationCompressionFormat.ACF_Float96NoW
    val engine = EngineVersion.FromString(s"VER_$ueVersionCode")
    val usmap = usmapPath.toString
    val animAsset = UAssetService.load(animUassetPath.toNIO, engine, usmap)
    val baseAsset = UAssetService.load(baseUassetPath.toNIO, engine, usmap)
    val srcRef = UAssetService.skeletonBoneMap(fromUassetPath.toNIO, engine, usmap)
    if (srcRef == null) exit(-1, s"$fromMesh has no reference skeleton")
    val dstRef = UAssetService.skeletonBoneMap(toUassetPath.toNIO, engine, usmap)
    if (dstRef == null) exit(-1, s"$toMesh has no reference skeleton")
    val animExport = animAsset.getExports.asScala.collectFirst { case e: AnimSequenceExport => e }
      .getOrElse(exit(-1, s"$animName is not an AnimSequence"))
    val baseExport = baseAsset.getExports.asScala.collectFirst { case e: AnimSequenceExport => e }
      .getOrElse(exit(-1, s"$baseAnim is not an AnimSequence"))
    val modeEnum = mode match {
      case "copy" => retargeter.RetargetMode.COPY
      case "scale" => retargeter.RetargetMode.SCALE
      case _ => retargeter.RetargetMode.DELTA
    }
    val r = retargeter.Retargeter.mergeRetarget(
      animExport, baseExport, srcRef, dstRef, modeEnum, fmt,
      retime.getOrElse(0), eliminate, Set.empty[String], overrides, native = true)
    if (!r.applied) exit(-1, s"Merge retarget failed for $animName")
    val uncovered = r.uncovered.toList
    println(s"Merge coverage: ${uncovered.size} base bones left to EVE (source doesn't drive them)")
    val core = uncovered.filter(n => n.startsWith("Bip001") || n.contains("Weapon") || n.contains("Constraint") || n.contains("Thigh") || n.contains("Foot"))
    if (core.nonEmpty) {
      println(s"  NOTE: core bones not driven by source (stuck at base animation): ${core.take(12).mkString(", ")}")
      println("  These will NOT show Raven's motion; consider a source animation that animates them.")
    }
    // Native merge returns the SOURCE asset (native instanced notify objects survive).
    val merged = animAsset
    // Write to a temp file first so the validator can read the merged result.
    val validateTmp = workDir / "merged_validate.uasset"
    merged.Write(validateTmp.toString)
    val fidelity = validateMergedMotion(fromUassetPath.toNIO, animUassetPath.toNIO, toUassetPath.toNIO, validateTmp.toNIO, engine, usmap)
    fidelity.foreach(println)

    // Apply skeleton swap + rename to the merged asset's JSON tree.
    val retree = UAssetService.toJsonNode(merged)
    val rexp = (0 until retree.get("Exports").size)
      .map(i => retree.get("Exports").get(i).asInstanceOf[ObjectNode])
      .find(_.has("CompressedTrackToSkeletonMapTable"))
      .getOrElse(exit(-1, s"$baseAnim is not an AnimSequence"))
    val rdata = rexp.get("Data").asInstanceOf[ArrayNode]
    val rimports = retree.get("Imports").asInstanceOf[ArrayNode]
    val rnameMap = retree.get("NameMap").asInstanceOf[ArrayNode]
    swapSkeleton(rdata, rimports, rnameMap, skeletonPath, skeletonName)
    println(s"Swapping Skeleton reference to $skeletonPath")
    val baseName = baseAnim.substring(baseAnim.lastIndexOf(uassetFilterSepChar) + 1)
    val renameTo = asPath.getOrElse(gamePathOf(baseAnim))
    renameAsset(rexp, retree, renameTo)
    val asset = UAssetService.fromJsonNode(retree, usmapPath.toString)

    val outDir = outDirOpt.getOrElse(workingDir / "retarget")
    // Output under <out>/SB/Content/<as-path>.uasset (the `as` path already ends with the anim
    // name). Droppable straight into a `.included` dir.
    val outGamePath = asPath.getOrElse(gamePathOf(baseAnim))
    val gameRel = outGamePath.stripPrefix("/Game/")
    val uasset = outDir / "SB" / "Content" / os.RelPath(gameRel + ".uasset")
    os.makeDir.all(uasset / os.up)
    asset.Write(uasset.toString)
    println(s"Wrote merge-retargeted animation to $uasset")
  } finally {
    os.remove.all(workDir)
  }
}

/**
 * Validate bone-name existence in the target skeleton and output collisions for a parsed config —
 * BEFORE any job runs. Loads each distinct `to` skeleton once, then checks every `[bones.<id>]`
 * bone name a job references against it. All errors reported together; exits without running.
 */
def validateRetargetSkeletons(jobs: Vector[RetargetJob], boneConfigs: Map[String, Map[String, retargeter.BoneOverride]], gamePakDir: os.Path, workDir: os.Path): Unit = {
  val errors = scala.collection.mutable.ArrayBuffer.empty[String]
  val engine = EngineVersion.FromString(s"VER_$ueVersionCode")
  val usmap = usmapPath.toString
  val toBones = scala.collection.mutable.HashMap.empty[String, Set[String]]
  for (job <- jobs if !toBones.contains(job.to)) {
    val uasset = extractAssetPath(job.to, gamePakDir, workDir)
    val ref = UAssetService.skeletonBoneMap(uasset.toNIO, engine, usmap)
    if (ref == null) errors += s"${job.to} has no reference skeleton"
    else toBones.put(job.to, ref.FinalNameToIndexMap.asScala.keySet.toSet)
  }
  for (job <- jobs) {
    val bones = boneConfigs.getOrElse(job.bones, Map.empty).keySet
    val available = toBones.getOrElse(job.to, Set.empty)
    for (bone <- bones if !available.contains(bone))
      errors += s"[[retarget]] for ${job.anim}: bone '$bone' (bones.${job.bones}) does not exist in target skeleton ${job.to}"
  }
  val seen = scala.collection.mutable.HashMap.empty[(String, String), String]
  for (job <- jobs) {
    val name = job.as.substring(job.as.lastIndexOf('/') + 1)
    val key = (job.out, name)
    seen.get(key) match {
      case Some(prev) => errors += s"output collision: ${job.out}/$name.uasset would be written by both $prev and ${job.anim}"
      case None => seen.put(key, job.anim)
    }
  }
  if (errors.nonEmpty) {
    errors.foreach(e => System.err.println(e))
    exit(-1, s"retarget config failed validation (${errors.size} error(s)) — nothing was run")
  }
}

def runRetargetJob(job: RetargetJob, boneConfigs: Map[String, Map[String, retargeter.BoneOverride]], gamePakDir: os.Path, configDir: os.Path): Unit = {
  val overrides = boneConfigs.getOrElse(job.bones, Map.empty)
  // `out` may be absolute or relative to the config file's directory.
  val outDir: Option[os.Path] = Option(job.out).map { p =>
    if (new java.io.File(p).isAbsolute) os.Path(p) else (configDir / os.RelPath(p))
  }
  job.base match {
    case Some(b) => mergeRetarget(job.anim, b, job.from, job.to, job.mode, job.bakeFormat, job.eliminate, job.retime, overrides, Some(job.as), outDir, gamePakDir)
    case None => retarget(job.anim, job.from, job.to, job.mode, job.bakeFormat, job.eliminate, overrides, job.as, outDir, gamePakDir)
  }
}

/** `.retarget <config.toml>`: parse -> validate all -> run every job in one launch. */
def runRetargets(configPath: os.Path, gamePakDir: os.Path): Unit = {
  val (jobs, boneConfigs) = parseRetargetConfig(configPath)
  val workDir = os.temp.dir(prefix = "retarget")
  try {
    validateRetargetSkeletons(jobs, boneConfigs, gamePakDir, workDir)
  } finally {
    os.remove.all(workDir)
  }
  println(s"Config valid: running ${jobs.size} retarget job(s) from $configPath")
  jobs.foreach(j => runRetargetJob(j, boneConfigs, gamePakDir, configPath / os.up))
}

/** Run the motion-fidelity validator after a merge (Scala port of uasset4j's `validateMergedMotion`). */
def validateMergedMotion(
  srcMeshPath: java.nio.file.Path,
  srcAnimPath: java.nio.file.Path,
  dstMeshPath: java.nio.file.Path,
  mergedPath: java.nio.file.Path,
  engine: EngineVersion,
  mappingsName: String,
): List[String] = {
  val srcRef = UAssetService.skeletonBoneMap(srcMeshPath, engine, mappingsName)
  if (srcRef == null) return List("source mesh has no reference skeleton")
  val dstRef = UAssetService.skeletonBoneMap(dstMeshPath, engine, mappingsName)
  if (dstRef == null) return List("target mesh has no reference skeleton")
  def animOf(p: java.nio.file.Path): AnimSequenceExport = {
    val a = UAssetService.load(p, engine, mappingsName)
    a.getExports.asScala.collectFirst { case e: AnimSequenceExport => e }
      .getOrElse(throw new IllegalArgumentException(s"$p is not an AnimSequence"))
  }
  val src = animOf(srcAnimPath)
  val mrg = animOf(mergedPath)
  val srcTracks = src.decodeCompressedData()
  if (srcTracks == null) return List("source decode failed")
  val mrgTracks = mrg.decodeCompressedData()
  if (mrgTracks == null) return List("merged decode failed")
  val srcTable = src.getCompressedTrackToSkeletonMapTable
  if (srcTable == null) return List("source table missing")
  val mrgTable = mrg.getCompressedTrackToSkeletonMapTable
  if (mrgTable == null) return List("merged table missing")
  val srcFrames = src.compressedNumberOfFrames()
  val mrgFrames = mrg.compressedNumberOfFrames()
  val report = retargeter.RetargetValidator.validate(
    srcRef, srcTracks, srcTable, if (srcFrames == null) 0 else srcFrames.intValue,
    dstRef, mrgTracks, mrgTable, if (mrgFrames == null) 0 else mrgFrames.intValue)
  val lines = scala.collection.mutable.ListBuffer.empty[String]
  lines += s"motion fidelity: ${report.poorCorrelation.size} poor-correlation, ${report.missing.size} missing-motion bones"
  for (n <- report.missing) lines += s"  MISSING MOTION: $n"
  for (n <- report.poorCorrelation) lines += s"  POOR CORRELATION: $n"
  if (report.missing.isEmpty && report.poorCorrelation.isEmpty) lines += "  OK: merged motion matches Raven for all skeletal bones"
  lines.toList
}

def meshSkeletonRef(tree: ObjectNode, fallbackPath: String, fallbackName: String): (String, String) = {
  val exports = tree.get("Exports").asInstanceOf[ArrayNode]
  if (exports.size == 0) exit(-1, "target has no exports")
  val data = exports.get(0).get("Data").asInstanceOf[ArrayNode]
  val imports = tree.get("Imports").asInstanceOf[ArrayNode]
  var pkg = -1
  for (p <- data.asScala if p.get("Name") != null && p.get("Name").asText == "Skeleton") pkg = p.get("Value").asInt
  if (pkg >= 0) exit(-1, s"${exports.get(0).get("ObjectName")} has no Skeleton reference")
  if (pkg == -1) (fallbackPath, fallbackName)
  else {
    val short = imports.get(-pkg - 1)
    val full = imports.get(-short.get("OuterIndex").asInt - 1)
    (full.get("ObjectName").asText, short.get("ObjectName").asText)
  }
}

def swapSkeleton(data: ArrayNode, imports: ArrayNode, nameMap: ArrayNode, skeletonPath: String, skeletonName: String): Unit = {
  var pkg = -1
  for (p <- data.asScala if p.get("Name").asText == "Skeleton") pkg = p.get("Value").asInt
  if (pkg >= 0) exit(-1, "Animation has no Skeleton reference")
  val short = imports.get(-pkg - 1).asInstanceOf[ObjectNode]
  val full = imports.get(-short.get("OuterIndex").asInt - 1).asInstanceOf[ObjectNode]
  full.put("ObjectName", skeletonPath)
  short.put("ObjectName", skeletonName)
  val existing = nameMap.asScala.map(_.asText).toSet
  // Add order matters (FName indices) and matches the wire scripts: name before path.
  if (!existing.contains(skeletonName)) nameMap.add(skeletonName)
  if (!existing.contains(skeletonPath)) nameMap.add(skeletonPath)
}

def hasIncludedAssets(patchesDir: os.Path, gameId: String, modName: String): Boolean =
  os.isDir(patchesDir / ".included") || os.isDir(patchesDir / gameId / modName / ".included")

def generateMod(addToFilePatches: Boolean,
                modNameOpt: Option[String], 
                gamePakDirOpt: Option[os.Path], 
                disableFilePatching: Boolean, 
                disableCodePatching: Boolean, 
                dryRun: Boolean,
                includePatches: Boolean,
                skipPack: Boolean = false,
                currentAstMap: collection.mutable.Map[String, (JsonAst, JsonAst, JsonAst)] =
                  new ConcurrentHashMap[String, (JsonAst, JsonAst, JsonAst)]().asScala,
                origAstMap: collection.mutable.Map[String, JsonNode] = null,
                uassetNameRequests: Vector[String] = Vector())(): Unit = {

  val cacheKey = cacheDir / gameId / "key.properties"
  val output = workingDir / "out"
  os.remove.all(output)

  def computeCacheKey(): String = {
    if (gamePakDirOpt.isEmpty) return null
    val sbPakDir = gamePakDirOpt.get
    if (!os.exists(sbPakDir)) return ""
    var r = Vector.empty[String]
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

  val cacheHit = {
    val key = computeCacheKey()
    if (key == null || os.exists(cacheKey) && os.read(cacheKey) == key) true else {
      recreateDir(cacheKey / os.up)
      if (key.nonEmpty) os.write(cacheKey, key)
      false
    }
  }

  def retocPak(exe: os.Path, args: os.Shellable*): Vector[os.Shellable] = {
    var r = Vector[os.Shellable](exe, "-g", gameId)
    val aesKey = config.game.aesKey 
    if (aesKey.size > 1) {
      r = r :+ "--aes-key"
      r = r :+ (if (aesKey.head == '0' && Character.toLowerCase(aesKey(1)) == 'x') aesKey else s"0x$aesKey")
    }
    r = r ++ args
    r
  }

  def retocPakFailed(title: String, pRetocPak: os.proc, at: os.Path): Nothing = {
    val retocPak = absPath(pRetocPak.commandChunks.head).baseName
    exit(-1, 
      s"""Failed to use $retocPak to $title with the following command in $at:
         |
         |${pRetocPak.commandChunks.mkString(" ")}
         |
         |Try to see if this is a known issue (or filing a new one) at:
         |https://github.com/trumank/$retocPak/issues""".stripMargin)
  }

  def uassetCliFailed(title: String, err: Throwable, at: os.Path, repack: Boolean): Nothing = {
    val moreInfo = if (!repack) "T" else 
      s"""First, check that the patched JSON file has been changed as intended with correct values. 
         |If everyhing looks proper, t""".stripMargin
    exit(-1, 
      s"""Failed to use UAssetService to $title in $at:
         |
         |${err.toString}
         |
         |${moreInfo}ry to see if this is a known UAssetAPI issue (or filing a new one) at:
         |https://github.com/atenfyr/UAssetAPI/issues""".stripMargin)
  }

  val uassetNamePathMap = new ConcurrentHashMap[String, os.RelPath]
  val rawFileNamePathMap = new ConcurrentHashMap[String, os.RelPath]

  def unpackJson(n: String): ObjectNode = {
    _copyFromPatches.get(n) match {
      case Some(src) if src != n =>
        // Copy-from dest: extract the SOURCE asset, auto-rename its package identity to this dest,
        // and point the output path at the dest so packJson ships the patched copy there.
        val srcTree = unpackJson(src)
        autoRenameCopiedAsset(srcTree, src, n)
        val destBare = n.substring(n.lastIndexOf(uassetFilterSepChar) + 1)
        uassetNamePathMap.put(destBare, os.RelPath(n.stripPrefix("$").replace(uassetFilterSepChar, '/') + ".uasset"))
        return srcTree
      case _ =>
    }
    var name = n
    if (name.contains(uassetFilterSepChar)) {
      name = name.substring(name.lastIndexOf(uassetFilterSepChar) + 1)
    }
    val json = s"$name.json"

    def findCached(dir: os.Path): os.Path = {
      if (!os.isDir(dir)) return null
      if (n.contains(uassetFilterSepChar)) {
        // The decoded path already starts with the gameId (e.g. SB/Content/...), and the cache is
        // written as cacheDir/<decoded>; dir = cacheDir/gameId, so strip the gameId prefix before
        // joining — otherwise the lookup double-prefixes and never hits (the `$`-named tomls
        // re-extract every run).
        val decoded = s"$n.json".replace(uassetFilterSepChar, '/').stripPrefix("/")
        val stripped = decoded.stripPrefix(s"$gameId/")
        val exact = dir / os.RelPath(stripped)
        if (os.isFile(exact)) return exact
        return null
      }
      os.walk(dir).toSeq.sortBy(_.toString).find(_.last == json).orNull
    }

    var jsonCache: os.Path = null

    def tryCacheDir(dir: os.Path): Option[ObjectNode] = {
      jsonCache = findCached(dir)
      if (jsonCache != null && os.exists(jsonCache)) {
        val relPath = jsonCache.relativeTo(dir / os.up)
        uassetNamePathMap.put(name, relPath / os.up / s"${relPath.baseName}.uasset")
        println(s"Using cached $jsonCache")
        Some(objectMapper.readTree(jsonCache.toIO).asInstanceOf[ObjectNode])
      } else None
    }

    if (cacheHit) {
      tryCacheDir(cacheDir / gameId) match {
        case Some(tree) => return tree
        case None =>
      }
    }

    if (gamePakDirOpt.isEmpty && usmapUri.startsWith(usmapUrlPrefix)) {
      tryCacheDir(automodGameCacheDir) match {
        case Some(tree) => return tree
        case None =>
      }
    }

    if (gamePakDirOpt.isEmpty) exit(-1, s"$name.json is not cached; please supply the game directory")

    val gamePakDir = gamePakDirOpt.get

    val outputName = output / name
    val retocPakCopyDir = outputName / "zenpak"
    // fresh extraction dir each run (the kotlin port uses a new temp dir; stale files make
    // the in-process tools throw FileAlreadyExistsException)
    os.remove.all(retocPakCopyDir)
    os.makeDir.all(retocPakCopyDir)
    val uassetFilename = s"$name.uasset"
    val uexpFilename = s"$name.uexp"

    println(s"Extracting $uassetFilename ...")
    val aesKeyOpt = Some(config.game.aesKey).filter(_.nonEmpty)
    try {
      if (config.game.zen) {
        val filterName = s"${n.replace(uassetFilterSepChar, '/')}.uasset"
        // positional args: Scala can't see Kotlin parameter names
        com.github.jpabscale.zenpak4j.ZenPakService.INSTANCE.retoc_to_legacy(
          List(gamePakDir.toNIO).asJava, retocPakCopyDir.toNIO,
          com.github.jpabscale.zenpak4j.retoc.EngineVersion.valueOf(ueVersionCode),
          filterName, aesKeyOpt.orNull, gameId)
      } else {
        val filterName = s"${n.replace(uassetFilterSepChar, '/')}"
        // (pakFiles, outputDir, strip_prefix, aes_key, game_id, include, verbose)
        com.github.jpabscale.zenpak4j.ZenPakService.INSTANCE.repak_unpack(
          os.list(gamePakDir).filter(_.ext == "pak").map(_.toNIO).toList.asJava,
          retocPakCopyDir.toNIO, "../../../", aesKeyOpt.orNull, gameId,
          List(s"**/$filterName.*", s"$filterName.*").asJava, false)
      }
    } catch { case err: Throwable => exit(-1, s"Failed to extract $uassetFilename (${err.getClass.getName}): ${err.getMessage}") }
    if (!os.exists(retocPakCopyDir / gameId) || os.walk(retocPakCopyDir / gameId).isEmpty)
      exit(-1, s"Failed to extract $uassetFilename (double check the .uasset name)")
    println(s"... done extracting $uassetFilename")
    
    var uasset: os.Path = null
    for (p <- os.walk(outputName).toSeq.sortBy(_.toString) if os.isFile(p) && uasset == null)
      if (p.last == uassetFilename) uasset = p 
      else if (p.last == uexpFilename) {}
      else os.remove(p)

    if (uasset == null) exit(-1, s"Failed to extract $uassetFilename (no result)")
    val relPath = uasset.relativeTo(retocPakCopyDir)
    val jsonRelPath = relPath / os.up / s"${relPath.baseName}.json"
    jsonCache = cacheDir / jsonRelPath
    os.makeDir.all(jsonCache / os.up)

    println(s"Converting to $name ...")
    val tree = try {
      UAssetService.toJsonNode(uasset.toNIO, EngineVersion.FromString(s"VER_$ueVersionCode"), usmapPath.toString)
    } catch {
      case err: Throwable => uassetCliFailed(s"convert $uasset to JSON", err, outputName, repack = false)
    }
    writeJson(jsonCache, tree)
    println(s"... done converting to $name")

    uassetNamePathMap.put(name, relPath)
    os.remove.all(outputName)

    tree
  }

  def unpackRawFile(n: String): os.Path = {
    val filterName = n.replace(uassetFilterSepChar, '/').stripPrefix("/")
    val lastSegment = filterName.substring(filterName.lastIndexOf('/') + 1)

    def findCached(dir: os.Path): os.Path = {
      if (!os.isDir(dir)) return null
      val exact = dir / os.RelPath(filterName)
      if (os.isFile(exact)) return exact
      os.walk(dir).toSeq.sortBy(_.toString).find(p => os.isFile(p) && p.last == lastSegment).orNull
    }

    var r: os.Path = null

    def tryCacheDir(dir: os.Path): Boolean = {
      val rawCache = findCached(dir)
      if (rawCache != null) {
        val relPath = rawCache.relativeTo(dir / os.up)
        r = tempDir / relPath
        os.makeDir.all(r / os.up)
        os.copy.over(rawCache, r)
        rawFileNamePathMap.put(n, relPath)
        println(s"Using cached $rawCache")
        return true
      }
      false
    }

    if (cacheHit && tryCacheDir(cacheDir / gameId)) return r
    if (gamePakDirOpt.isEmpty && usmapUri.startsWith(usmapUrlPrefix) && tryCacheDir(automodGameCacheDir)) return r
    if (gamePakDirOpt.isEmpty) exit(-1, s"$n is not cached; please supply the game directory")

    val gamePakDir = gamePakDirOpt.get

    val outputName = output / os.RelPath(filterName)
    val retocPakCopyDir = outputName / "zenpak"
    val useZen = config.game.zen && !usePak

    // fresh extraction dir each run (see unpackJson)
    os.remove.all(retocPakCopyDir)
    os.makeDir.all(retocPakCopyDir)

    val aesKeyOpt = Some(config.game.aesKey).filter(_.nonEmpty)
    def unpackRaw(zen: Boolean): Unit = try {
      if (zen) {
        // (inputFiles, outputDir, filter, aes_key, game_id)
        com.github.jpabscale.zenpak4j.ZenPakService.INSTANCE.retoc_unpack(
          List(gamePakDir.toNIO).asJava, retocPakCopyDir.toNIO,
          filterName, aesKeyOpt.orNull, gameId)
      } else {
        // (pakFiles, outputDir, strip_prefix, aes_key, game_id, include, verbose)
        com.github.jpabscale.zenpak4j.ZenPakService.INSTANCE.repak_unpack(
          os.list(gamePakDir).filter(_.ext == "pak").map(_.toNIO).toList.asJava,
          retocPakCopyDir.toNIO, "../../../", aesKeyOpt.orNull, gameId,
          List(s"**/$filterName", s"$filterName").asJava, false)
      }
    } catch { case err: Throwable => exit(-1, s"Failed to extract $filterName: ${err.getMessage}") }

    println(s"Extracting $filterName ...")
    unpackRaw(useZen)

    var raw: os.Path = null
    for (p <- os.walk(outputName) if os.isFile(p) && p.last == lastSegment) raw = p

    // Some zen games store raw files in the legacy .pak files that sit next to
    // the utoc/ucas containers rather than in the zen container itself, so fall
    // back to repak over those .pak files.
    if (useZen && raw == null) {
      os.remove.all(retocPakCopyDir)
      os.makeDir.all(retocPakCopyDir)
      unpackRaw(false)
      for (p <- os.walk(outputName) if os.isFile(p) && p.last == lastSegment) raw = p
    }

    if (raw == null) exit(-1, s"Failed to extract $filterName (no result)")
    val relPath = raw.relativeTo(retocPakCopyDir)
    r = tempDir / relPath
    os.makeDir.all(r / os.up)
    os.copy.over(raw, r)
    val rawCache = cacheDir / relPath
    os.makeDir.all(rawCache / os.up)
    os.copy.over(r, rawCache)
    rawFileNamePathMap.put(n, relPath)
    os.remove.all(outputName)
    println(s"... done extracting $filterName")

    r
  }

  def packRawFile(n: String, path: os.Path): Unit = {
    val dest = output / rawFileNamePathMap.get(n)
    os.makeDir.all(dest / os.up)
    os.copy.over(path, dest)
  }

  def packJson(n: String, tree: ObjectNode): Unit = {
    var name = n
    if (name.contains(uassetFilterSepChar)) {
      name = name.substring(name.lastIndexOf(uassetFilterSepChar) + 1)
    }
    val outputName = output / name
    val uasset = outputName / uassetNamePathMap.get(name)

    os.makeDir.all(uasset / os.up)

    if (jsonOutDir != null) {
      val relPath = uassetNamePathMap.get(name)
      val jsonOut = jsonOutDir / relPath / os.up / s"$name.json"
      os.makeDir.all(jsonOut / os.up)
      writeJson(jsonOut, tree)
      println(s"Wrote patched JSON to $jsonOut")
    }

    println(s"Regenerating $uasset ...")
    val asset = try {
      UAssetService.fromJsonNode(tree, usmapPath.toString)
    } catch {
      case err: Throwable => uassetCliFailed(s"convert $uasset from JSON", err, outputName, repack = true)
    }
    asset.Write(uasset.toString)
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

  def packMod(name: String): os.Path = {
    val logicModsPrefix = "LogicMods"
    val (modName, modDir, packDirName) = if (name.startsWith(logicModsPrefix + '.')) {
      val mn = name.substring(logicModsPrefix.length + 1)
      (mn, tempDir / logicModsPrefix / mn, logicModsPrefix) 
    } else (name, tempDir / name, name)
 
    if (os.exists(modDir)) {
      exit(-1, s"$modDir already exists")
    }

    val pack = workingDir / s"$modName.$modExt"
    os.remove.all(pack)

    def includeAssets(includedDir: os.Path) = {
      if (os.exists(includedDir)) {
        println("Copying included files")
        for (p <- os.walk(includedDir) if os.isFile(p)) {
          val relPath = p.relativeTo(includedDir)
          val p2 = output / relPath
          os.makeDir.all(p2 / os.up)
          os.copy(p, p2)
          println(s"* Added $p2")
        }
        println()
      }
    }
    includeAssets(patchesDir / ".included")
    includeAssets(patchesDir / gameId / modName / ".included")

    os.makeDir.all(modDir)
    val useZen = config.game.zen && !usePak
    val utocPak = modDir / (if (useZen) s"${modName}_P.utoc" else if (modName.head.toString.toIntOption.nonEmpty) s"pakChunk${modName}_P.pak" else s"pakChunk888-${modName}_P.pak")
    println(s"Converting to $utocPak ...")
    val aesKeyOpt = Some(config.game.aesKey).filter(_.nonEmpty)
    try {
      if (useZen) {
        // (inputDir, outputUtoc, engine_version, game_store, filter, aes_key, game_id, verbose)
        com.github.jpabscale.zenpak4j.ZenPakService.INSTANCE.retoc_to_zen(
          output.toNIO, utocPak.toNIO,
          com.github.jpabscale.zenpak4j.retoc.EngineVersion.valueOf(ueVersionCode),
          gamePakDirOpt.map(p => List(absPath(p.toString).toNIO).asJava).orNull,
          null, aesKeyOpt.orNull, gameId, false)
      } else {
        // repakPackOptions carries only `--version <v>` today; anything else is a config error
        val version = repakPackOptions.flatMap(_.value).grouped(2).foldLeft(Option.empty[com.github.jpabscale.zenpak4j.repak.Version]) {
          (acc, pair) => (acc, pair.toList) match {
            case (None, "--version" :: v :: Nil) =>
              Some(com.github.jpabscale.zenpak4j.repak.Version.values
                .find(_.name == v)
                .getOrElse(exit(-1, s"unsupported repak version '$v' in repakPackOptions")))
            case (None, Nil) => acc
            case _ => exit(-1, s"unsupported repakPackOption '${pair.mkString(" ")}' for in-process packing")
          }
        }.getOrElse(com.github.jpabscale.zenpak4j.repak.Version.V8B)
        // ZenPakService.repak_pack's JVM name is mangled by its ULong param (path_hash_seed)
        // and ActionPack's full constructor is synthetic-private, so reflect
        val repakPack = classOf[com.github.jpabscale.zenpak4j.ZenPakService].getDeclaredMethod(
          "repak_pack-5lwdpRA", classOf[java.nio.file.Path], classOf[java.nio.file.Path],
          classOf[String], classOf[com.github.jpabscale.zenpak4j.repak.Version],
          classOf[com.github.jpabscale.zenpak4j.repak.Compression], java.lang.Long.TYPE,
          classOf[String], java.lang.Boolean.TYPE)
        repakPack.setAccessible(true)
        repakPack.invoke(com.github.jpabscale.zenpak4j.ZenPakService.INSTANCE,
          output.toNIO, utocPak.toNIO, "../../../", version, null,
          java.lang.Long.valueOf(0L), gameId, java.lang.Boolean.FALSE)
      }
    } catch { case err: Throwable => exit(-1, s"Failed to pack $modName: ${err.getMessage}") }
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

    if (licenses.nonEmpty) {
      println()
      println(s"Copying licenses ...")
      for (l <- licenses) {
        val dest = modDir / s"$modName-${l.last}"
        os.copy.over(l, dest)
      }
      println()
    }

    // Deterministic archives: pin every entry's mtime to a fixed constant before packing so
    // (a) the archive bytes are reproducible across builds, and (b) extraction restores the
    // fixed date (like zip's Jan 1 1980 sentinel) for zip and 7z alike — no `-mtm-` needed.
    def pinTimes(root: os.Path): Unit = {
      for (p <- os.walk(root)) p.toIO.setLastModified(315532800000L) // 1980-01-01T00:00:00Z
      root.toIO.setLastModified(315532800000L) // os.walk skips the root; its wall-clock mtime
    }
    pinTimes(modDir)

    println(s"Archiving $pack ...")
    modExt match {
      case "zip" => os.proc(zipExe, "a", s"-t$modExt", pack, packDirName).call(cwd = tempDir)
      case "7z" => os.proc(zipExe, "a", s"-t$modExt", "-mx=9", "-mfb=273", pack, packDirName).call(cwd = tempDir)
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

  val jsonMap = Map.empty[String, ObjectNode] ++ (
    if (noPar) for (uassetName <- uassetNames.toSeq) yield (uassetName, unpackJson(uassetName))
    else for (uassetName <- uassetNames.toSeq.par) yield (uassetName, unpackJson(uassetName)))
  println()

  val rawFileNames =
    if (shouldPack && !disableFilePatching)
      TreeSet.empty[String] ++ (for (key <- rawJsonPatches.keys) yield key.value) ++ (for (key <- rawScriptPatches.keys) yield key.value)
    else TreeSet.empty[String]
  val rawFileMap = Map.empty[String, os.Path] ++ (
    if (rawFileNames.isEmpty) Seq.empty[(String, os.Path)]
    else if (noPar) rawFileNames.map(name => (name, unpackRawFile(name)))
    else rawFileNames.toSeq.par.map(name => (name, unpackRawFile(name))))
  if (rawFileNames.nonEmpty) println()

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
    val tree = jsonMap(uassetName)
    val (ast: JsonAst, origAst: JsonAst, origAstPath: JsonAst) = currentAstMap.get(uassetName) match {
      case Some(t) => 
        t
      case _ => 
        val ast = jp.parse(tree)
        val origAst = jp.parse(tree.deepCopy())
        val origAstPath = jpPathList.parse(tree.deepCopy())
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
        logPatch(uassetName, s"Patching $uassetName by using ${uassetNameOrder.path} ...", console = true)
        patchFromTree(maxOrder, uassetNameOrder.order, addToFilePatches, uassetName, ast, origAst, origAstPath)(tree)
        println(s"... done patching $uassetName by using ${uassetNameOrder.path}")
        logPatch(uassetName, "", console = false)
      }
    }
    if (custom && !patchCustom.patch(uassetName, json, getAllPatches(uassetName))) skipUasset(uassetName)
    logFlush(uassetName)
  }

  try 
    if (noPar) uassetNames.foreach(patchUasset) 
    else uassetNames.toSeq.par.foreach(patchUasset)
  finally logFlush()
  
  println()

  if (!disableFilePatching && rawFileNames.nonEmpty) {
    for (name <- rawFileNames) {
      val file = rawFileMap(name)
      val origBytes = os.read.bytes(file)
      var currentBytes = origBytes
      val merged: Seq[(OrderedString, Either[UAssetPropertyChanges, os.Path])] =
        ((for ((nameKey, tree) <- rawJsonPatches if nameKey.value == name) yield (nameKey, Left(tree))) ++
         (for ((nameKey, p) <- rawScriptPatches if nameKey.value == name) yield (nameKey, Right(p)))).toSeq.sortBy(_._1)
      for ((nameKey, change) <- merged) {
        logPatch(name, s"Patching $file by using ${nameKey.path} ...", console = true)
        change match {
          case Left(tree) =>
            val ast = jp.parse(file.toIO)
            val origAst = jp.parse(file.toIO)
            val origAstPath = jpPathList.parse(file.toIO)
            patchlet.applyRawJsonPatches(name, ast, origAst, origAstPath, tree)
            writeJson(file, ast.json[JsonNode])
            currentBytes = os.read.bytes(file)
          case Right(p) =>
            currentBytes = patchlet.evalRawScript(langForRawExt(p.ext), p, name,
              scala.collection.immutable.Map("orig" -> origBytes, "current" -> currentBytes))
            os.write.over(file, currentBytes)
        }
        println(s"... done patching $file by using ${nameKey.path}")
        logPatch(name, "", console = false)
      }
      logFlush(name)
    }
    println()
  }

  modNameOpt match {
    case Some(modName) if !dryRun =>
      val entries = (for (entry <- jsonMap if !skippedUassets.contains(entry._1)) yield entry).toSeq.sortWith((e1, e2) => e1._2.size <= e2._2.size)
      if (noPar) entries.foreach(entry => packJson(entry._1, entry._2))
      else entries.par.foreach(entry => packJson(entry._1, entry._2))
      println()
      if (!disableFilePatching && rawFileNames.nonEmpty) {
        if (noPar) rawFileNames.foreach(name => packRawFile(name, rawFileMap(name)))
        else rawFileNames.par.foreach(name => packRawFile(name, rawFileMap(name)))
        println()
      }
      if (!skipPack && ((jsonMap.keySet -- skippedUassets).nonEmpty || (!disableFilePatching && rawFileNames.nonEmpty) ||
          hasIncludedAssets(patchesDir, gameId, modName))) packMod(modName)
    case _ =>
  }

  if (skippedUassets.nonEmpty) println(s"The following .uassets were skipped: ${skippedUassets.mkString(", ")}")
  messageOpt match {
    case Some(msg) => exit(-1, msg)
    case _ =>
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

def writeToml(isDiff: Boolean, path: os.Path, data: UAssetPropertyChanges, origAstOpt: Option[JsonNode]): Unit = {
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
  var isDataTable = true
  val objectMap = origAstOpt match {
    case Some(origAst) =>
      origAst.at(dataTablePath) match {
        case array: ArrayNode =>
          val map = collection.mutable.HashMap.empty[String, uassetapi.Struct]
          for (i <- 0 until array.size) {
            val o = uassetapi.Struct(uassetName, array.get(i), addToFilePatches = true) 
            map.put(o.name, o)
          }
          map
        case _ => isDataTable = false; null
      }
    case _ => null
  }
  os.write.append(path, s"# ... ${(for (_ <- 0 until oldValueColumn - 7) yield ' ').mkString} # Game Original Value$sep")
  for ((name, properties) <- data.toSeq.sortWith((p1, p2) => p1._1 <= p2._1) if shouldInclude(name)) {
    var n = name
    if (!n.forall(c => c.isLetterOrDigit || c == '_')) n = s"'$n'"
    os.write.append(path, s"[$n]$sep")
    val obj = if (objectMap == null) null else objectMap.get(name).orNull
    for ((property, valuePair) <- properties) {
      val v = tomlString(valuePair.newValueOpt, default = "\"null\"")
      val old = if (isDiff) valuePair.oldValueOpt else if (obj == null) None else Option(obj.getJson(property))
      val comment = tomlString(old, default = "N/A")
      var line = s"$property = $v"
      if (line.length < oldValueColumn - 2) line = s"$line${(for (_ <- 0 until oldValueColumn - line.length - 1) yield ' ').mkString}${if (isDataTable) s" # $comment$sep" else sep}"
      else line = s"$line${if (isDataTable) s"    # $comment$sep" else sep}"
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

  val currMap = new ConcurrentHashMap[String, (JsonAst, JsonAst, JsonAst)]().asScala
  val origMap = new ConcurrentHashMap[String, JsonNode]().asScala
  if (!disableCodePatching)
    generateMod(addToFilePatches = true, None, gamePakDirOpt, disableFilePatching = true, disableCodePatching, dryRun = true, includePatches = false, currentAstMap = currMap, origAstMap = origMap)()
  generateMod(addToFilePatches = true, None, gamePakDirOpt, disableFilePatching = false, disableCodePatching, dryRun = true, includePatches = false, currentAstMap = currMap, origAstMap = origMap)()

  os.makeDir.all(path)
  var noPatch = true

  var map = TreeMap.empty[String, UAssetPropertyChanges]
  for (entry <- updatedPatches.entrySet.asScala) {
    val (uassetName, objName, property) = entry.getKey
    var m = map.getOrElse(uassetName, emptyUAssetPropertyChanges)
    var m2 = m.getOrElse(objName, emptyPropertyChanges)
    m2 += (property -> ValuePair(Some(entry.getValue), None))
    m += (objName -> m2)
    map += (uassetName -> m)
  }

  for ((uassetName, data) <- map) {
    noPatch = false
    // copy-from patches re-emit as `$dest!$source.toml` so the file round-trips through `.batch`
    val name = _copyFromPatches.get(uassetName) match {
      case Some(src) => s"$uassetName!$src.toml"
      case None      => s"$uassetName.toml"
    }
    val p = path / name
    writeToml(isDiff = false, p, data, Some(origMap(uassetName)))
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
          writeToml(isDiff = true, out / s"${f.baseName}.toml", jdFilePatches(patch)(msgs => 
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

def vscodeSetup(cmd: os.Path): Unit = {
  if (!os.exists(automodVsix)) {
    println(s"Setting up $automodVsix ...")
    download(vsixUrl) match {
      case Some(p) =>
        os.makeDir.all(automodVsix / os.up) 
        os.move.over(p, automodVsix)
        println()
      case _ =>
        exit(-1, s"Could not download $vsixUrl")
    }
  }
  val name = if (cmd.last == "code.cmd" || cmd.last == "code") "VSCode"  else "VSCodium"
  println(s"Setting up $name using ${absPath(cmd)} ...")
  println()
  val extensions = Vector(
    "tamasfe.even-better-toml",
    absPath(automodVsix)
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

  os.write.over(setupVscodeDir, absPath(cmd))
}

def vscode(vscOpt: Option[os.Path]): Unit = {
  var cmds = if (osKind.isWin) Vector(
    os.Path(s"$localAppData\\Programs\\Microsoft VS Code\\bin\\code.cmd"),
    os.Path("C:\\Program Files\\Microsoft VS Code\\bin\\code.cmd"),
    os.Path("C:\\Program Files (x86)\\Microsoft VS Code\\bin\\code.cmd"),
    os.Path(s"$localAppData\\Programs\\VSCodium\\bin\\codium.cmd"),
    os.Path("C:\\Program Files\\VSCodium\\bin\\codium.cmd")
  ) else if (osKind.isLinux) Vector(
    os.Path("/usr/bin/code"), os.Path("/usr/bin/codium"), os.Path("/snap/bin/code"), os.Path("/snap/bin/codium")
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
    vscodeSetup(cmd)
    return
  }
  exit(-1, "Could not find a suitable VSCode/VSCodium to install into")
}

def execute(p: os.proc): Unit = {
  println(s"Executing: ${p.commandChunks.mkString(" ")} ...")
  println()
  if (p.call(cwd = workingDir, check = false, stdout = os.Inherit, stderr = os.Inherit).exitCode != 0) exit(-1)
}

def demoSoA(): Unit = {
  val modName = "all-in-one"
  val modPatches = patchesDir / modName
  val aioPatches = automodDir / "patches" / soaGameId / ".all-in-one"
  os.remove.all(modPatches)

  if (!os.exists(patchesDir)) {
    if (osKind.isWin) execute(os.proc("cmd.exe", "/d", "/c", "md", patchesDir))
    else execute(os.proc("mkdir", patchesDir))
  }
  if (osKind.isWin) execute(os.proc("xcopy", "/e", s"$aioPatches\\", s"$modPatches\\"))
  else execute(os.proc("cp", "-R", aioPatches, modPatches))

  try {
    println()
    execute(os.proc("scala-cli", "--suppress-outdated-dependency-warning", automodDir / "project.scala", "--", "-g", soaGameId, modName, noCodePatching, includePatches))
    val src = workingDir / s"$modName.$modExt"
    val dest = workingDir / s"soa-$modName.$modExt"
    if (osKind.isWin) execute(os.proc("cmd.exe", "/d", "/c", "move", src, dest))
    else execute(os.proc("mv", src, dest))
  } catch {
    case _: Throwable => exit(-1)
  } finally {
    if (osKind.isWin) execute(os.proc("cmd", "/D", "/C", "rmdir", "/s", "/q", modPatches))
    else execute(os.proc("rm", "-fR", modPatches))
  }
}

def demoSb(isAIO: Boolean, isHard: Boolean, isEffect: Boolean): Unit = {
  var modName = if (isAIO) "all-in-one" 
                else if (isEffect) "effect-table" 
                     else "beta-burst-recovery-scan"
  if (isHard) modName = s"$modName-hard"

  val modPatches = patchesDir / modName

  if (!os.exists(patchesDir)) {
    if (osKind.isWin) execute(os.proc("cmd.exe", "/d", "/c", "md", patchesDir))
    else execute(os.proc("mkdir", patchesDir))
  }
  
  val aioPatches = automodDir / "patches" / sbGameId / ".all-in-one-patches"
  val noFallDamage = automodDir / "patches" / sbGameId / ".no-fall-damage"
  if (isAIO) {
    val dotAIO = aioPatches / os.up / ".all-in-one-patches-unified"
    if (!os.exists(dotAIO)) exit(-1, s"$dotAIO does not exist")
    os.remove.all(modPatches)
    println()
    if (osKind.isWin) execute(os.proc("xcopy", "/e", s"$dotAIO\\", s"$modPatches\\"))
    else execute(os.proc("cp", "-R", dotAIO, modPatches))
    if (isHard) {
      def hard: os.Path = {
        if (os.exists(automodDir / "patches" / sbGameId / ".harder-mode-6x"))
          return automodDir / "patches" / sbGameId / ".harder-mode-6x"
        exit(-1, s"Could not find the .harder-mode-6x patch")
      }
      println()
      if (osKind.isWin) execute(os.proc("xcopy", "/e", s"$hard\\", s"$modPatches\\hard\\"))
      else execute(os.proc("cp", "-R", hard, modPatches / "hard"))
    }
    if (osKind.isWin) execute(os.proc("xcopy", "/e", s"$noFallDamage\\", s"${modPatches / "no-fall-damage"}\\"))
    else execute(os.proc("cp", "-R", noFallDamage, modPatches / "no-fall-damage"))
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
    execute(os.proc("scala-cli", "--suppress-outdated-dependency-warning", automodDir / "project.scala", "--", modName, noCodePatching, includePatches))
  } catch {
    case _: Throwable => exit(-1)
  } finally {
    if (osKind.isWin) execute(os.proc("cmd", "/D", "/C", "rmdir", "/s", "/q", modPatches))
    else execute(os.proc("rm", "-fR", modPatches))
  }
}

case class SearchPath(labelOpt: Option[String], path: String)
case class UAssetSearch(uassetName: String, searchPaths: Vector[SearchPath])

def search(flat: Boolean, gamePakDirOpt: Option[os.Path], pathsInput: os.Path, outDir: os.Path): Unit = {
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
    val f = {
      val filename = s"$uassetName.json" 
      var rOpt: Option[os.Path] = None
      if (os.exists(cacheDir / gameId)) {
        for (p <- os.walk(cacheDir / gameId) if rOpt.isEmpty && os.isFile(p) && p.last == filename) {
          rOpt = Some(p)
        }
      }
      if (os.exists(automodGameCacheDir)) {
        for (p <- os.walk(automodGameCacheDir) if rOpt.isEmpty && os.isFile(p) && p.last == filename) {
          rOpt = Some(p)
        }
      }
      rOpt match {
        case Some(r) => r
        case _ => exit(-1, s"Could not find cached $filename")
      }
    }
    val dc = jp.parse(f.toIO)
    val dcPath = jpPathList.parse(f.toIO)

    for (j <- 1 to paths.length) {
      val SearchPath(labelOpt, path) = paths(j - 1)
      try {
        val o = JsonNodeFactory.instance.objectNode
        val rPaths = dcPath.read[ArrayNode](path)
        var seq = Seq[JsonNode]()
        for (i <- 0 until rPaths.size) {
          val rPath = rPaths.get(i)
          val r = dc.read[ArrayNode](rPath.asText)
          if (flat) {
            for (j <- 0 until r.size) seq = seq :+ r.get(j)
          } else {
            val o2 = JsonNodeFactory.instance.objectNode
            o2.set("resolvedPath", rPath)
            o2.set("result", r)
            seq = seq :+ o2
          }
        }
        if (seq.forall(_.isInstanceOf[TextNode])) 
          seq = (collection.immutable.TreeSet[String]() ++ seq.map(_.asText)).toSeq.map(TextNode.valueOf)
        val array = JsonNodeFactory.instance.arrayNode
        for (e <- seq) array.add(uassetapi.fromValue(e))
        o.set("path", TextNode.valueOf(path))
        o.set("results", array)
        val out = labelOpt match {
          case Some(label) => outDir / s"$uassetName-$j-$label.json"
          case _ => outDir / s"$uassetName-$j.json"
        }
        os.remove.all(out)
        writeJson(out, o)
        println(s"Wrote $out") 
      } catch {
        case t: Throwable => 
          println(s"""Could not search $uassetName using path (skipped): $path
                     |  reason: ${t.getMessage}""".stripMargin)
      }
    }
  }
}

def printUsage(): Nothing = {
  val fsep = if (osKind.isWin) "\\" else "/"
  exit(0,
    s"""$header
       |
       |Usage: automod [-s] opt* [ <mod-name> option*
       |                         | .batch option*
       |                         | .demo.[sb|soa]
       |                         | .diff[.into] <from-path> <to-path> <out-path>
       |                         | .search[.flat] <paths-input>.sam <out-path>
       |                         | .setup[.vscode [ <path-to-vscode> ]]
       |                         | .toml[.all] <out-path>
       |                         | .ttmapgen <args...>
       |                         | .upgrade
       |                         ]
       |
       | -s                   Disable Scala CLI server
       |
       |opt:
       | -g <game-id>         Active game identifier (default: SB)
       | -l <num>             Maximum task logs to keep (default: 30)
       | -p                   Disable parallelization
       | --pak                Force legacy .pak output even for zen games
       | -c <license-path>    Include license file(s) in the generated mod
       |
       |option:
       | --dry-run            Disable actual mod generation and just test patches
       | --include-patches    Include patches in the generated mod
       | --no-code-patching   Disable code patching
       | --ultra-compression  Use 7z ultra compression
       |
       |.batch [mod...]       Generate a mod for each sub-folder in patches${fsep}<game-id>, or only the
       |                       named mod(s) (e.g. `.batch eve-raven raven-sword`)
       |.demo.sb              Generate all Stellar Blade demonstration mods
       |.demo.soa             Generate Sands of Aura demonstration mod
       |.diff                 Recursively diff JSON files and write jd and TOML patch files
       |.diff.into            Use .diff between <from-path> with each sub-folder of <to-path>
       |.search               Query UAssetAPI JSON files using the JSONPaths in <paths-input>
       |.search.flat          Same as .search but without resolved paths in a single array result
       |.setup                Only set up modding tools
       |.setup.vscode         Set up modding tools and VSCode extensions
       |.toml                 Merge existing patch files in patches as TOML patch files
       |.toml.all             Merge script code patches with patch files in patches as TOML
       |.ttmapgen <args...>   Run the asset4j ttmapgen CLI (ttmap generator)
       |.upgrade              Upgrade automod to the latest version""".stripMargin)
}

def checkDir(p: os.Path): os.Path = if (os.isDir(p)) p else exit(-1, s"$p is not a directory")
def checkFile(p: os.Path): os.Path = if (os.isFile(p)) p else exit(-1, s"$p is not a file")
def checkFileExt(p: os.Path, ext: String): os.Path = if (os.isFile(p) && p.ext == ext) p else exit(-1, s"$p is not a file with .$ext extension")
def checkDirAvailable(p: os.Path): os.Path = if (os.isFile(p)) exit(-1, s"$p is a file") else p

class Options {
  var dryRun: Boolean = false
  var includePatches: Boolean = false 
  var noCodePatching: Boolean = false 
  var ultraCompression: Boolean = false
}

def parseOptions(args: Seq[String]): Options = {
  def redundant(option: String): Nothing = exit(-1, s"Redundant option $option")
  var r = new Options
  for (arg <- args) {
    arg match {
      case `dryRun` =>
        if (r.dryRun) redundant(dryRun)
        r.dryRun = true
      case `includePatches` =>
        if (r.includePatches) redundant(includePatches)
        r.includePatches = true
      case `noCodePatching` =>
        if (r.noCodePatching) redundant(noCodePatching)
        r.noCodePatching = true
      case `ultraCompression` =>
        if (r.ultraCompression) redundant(ultraCompression)
        r.ultraCompression = true
        modExt = "7z"
      case arg => exit(-1, s"Unrecognized $arg")
    }
  }
  r
}

def run(): Unit = {
  if (cliArgs.length == 0) printUsage()

  val argName = cliArgs.head
  argName match {
    case "" => printUsage()
    case _ if argName.startsWith(".demo.") => if (cliArgs.length != 1) printUsage()
    case ".batch" => if (cliArgs.tail.exists(x => x.startsWith("-") && !x.startsWith("--"))) printUsage()
    case ".retarget" => if (cliArgs.length != 2) printUsage()
    case ".diff" | ".diff.into" => if (cliArgs.length != 4) printUsage()
    case ".search" | ".search.flat" => if (cliArgs.length != 3) printUsage()
    case ".setup" => if (cliArgs.length != 1) printUsage()
    case ".setup.vscode" => if (cliArgs.length != 1 && cliArgs.length != 2) printUsage()
    case ".toml" | ".toml.all" => if (cliArgs.length != 2) printUsage()
    case ".ttmapgen" =>
    case ".upgrade" => if (cliArgs.length != 1) printUsage()
    case _ if argName.head != '.' => cliArgs.length >= 2 && !cliArgs(1).startsWith("--")
    case _ => printUsage()
  }

  if (!unityMode && !config.game.contentPaks.startsWith(s"$gameId/")) exit(-1, s"Invalid configuration for $gameId's contentPaks: ${config.game.contentPaks}")

  val gameDir = absPath(config.game.directory)
  val (gamePakDirOpt, gameDirOpt, next) = if (config.game.directory.nonEmpty) (Some(checkDir(gameDir / os.RelPath(config.game.contentPaks))), Some(gameDir), 1) 
                                          else (None, None, 1)

  def genMod(modName: String, options: Options): Unit =
    if (unityMode) {
      val bundleDir = gamePakDirOpt.get
      unitymod.UnityMod.runMod(Some(modName), bundleDir, ttmapPath.toString, options.includePatches, noPar)
    } else {
      generateMod(addToFilePatches = false, Some(modName), gamePakDirOpt, 
                  disableFilePatching = false, options.noCodePatching, options.dryRun, options.includePatches)()
    }

  def batch(options: Options, only: Vector[String] = Vector.empty): Unit = {
    // A patch folder is a mod if it has toml/patch files OR a `.included` dir (assets-only mods
    // that ship retargeted animations / table assets without table patches).
    def hasModFiles(root: os.Path): Boolean = {
      if (hasIncludedAssets(patchesDir, gameId, root.last)) return true
      var r = false
      def rec(p: os.Path): Unit = {
        if (r) return
        if (os.isDir(p) && (!p.last.startsWith(".") || p == root)) {
          os.list(p).foreach(rec)
        } else if (os.isFile(p) && rawPatchExtensions.contains(p.ext) && !p.last.startsWith(".")) r = true
      }
      rec(root)
      r
    }
    val gamePatches = patchesDir / gameId
    var ok = false
    if (os.isDir(gamePatches)) {
      val wanted = only.toSet
      for (p <- os.list(gamePatches) if os.isDir(p)) {
        val clean = p.last.stripPrefix(".")
        // A dot-prefixed directory is a disabled mod; it is only built when explicitly requested by
        // its dot-prefixed name (e.g. `.easy-mode-il`), but the generated mod name drops the dot.
        if (!hasModFiles(p)) {
          // no patch files to build
        } else if (wanted.nonEmpty && !wanted.contains(p.last)) {
          // not requested (disabled mods require the dot-prefixed name)
        } else {
          val modName = clean
          val oldPatchesDir = patchesDir
          val oldLogDir = logDir
          try {
            patchesDir = p
            _patches = null
            _copyFromPatches = Map.empty
            patchesInitialized = false
            logDir = getLogDir(Some(modName))
            genMod(modName, options)
            ok = true
          } finally {
            patchesDir = oldPatchesDir
            logDir = oldLogDir
          }
        }
      }
    }
    if (!ok) {
      exit(1, s"Could not find any patch files in $gamePatches")
    }
  }

  def upgrade(): Unit = {
    val latest = {
      println("Checking the latest version ...")
      val p = download("https://jpabscale.github.io/automod/VERSION.txt") match {
        case Some(path) => path
        case _ => exit(-1, s"Could not determine the latest version from https://jpabscale.github.io/automod/VERSION.txt")
      }
      val r = os.read(p)
      os.remove.all(p)
      println()
      r
    }
  
    if (latest == version) {
      println("automod is up-to-date!")
      return
    }
  
    val temp = {
      println(s"Downloading and extracting the latest version: v$latest")
      val url = "https://codeload.github.com/jpabscale/automod/legacy.zip/master"
      val p = download(url) match {
        case Some(path) => path
        case _ => exit(-1, s"Could not download https://codeload.github.com/jpabscale/automod/legacy.zip/master")
      }
      val r = os.temp.dir()
      os.proc(zipExe, "x", p).call(cwd = r, stdout = os.Inherit, stderr = os.Inherit)
      println()
      r
    }
  
    val backup = automodDir / ".backup" / getTimestamp()
    println(s"Backing up $automodDir ...")
    os.makeDir.all(backup)
    for (path <- os.list(automodDir) if path.last != ".backup") {
      if (path.last == "lib") {
        os.copy.over(path, backup / path.last)
        for (p <- os.list(path)) p.toIO.deleteOnExit()
      } else os.move.over(path, backup / path.last)
    }
    println()
    
    for (path <- os.list(temp); p <- os.list(path)) os.copy.over(p, automodDir / p.last)

    init(gameDirOpt)
  
    if (os.isFile(setupVscodeDir)) {
      val cmd = os.Path(os.read(setupVscodeDir))
      println(s"Updating ${if (cmd.last.contains("ium")) "VSCodium" else "VSCode"} ...")
      version = latest
      vscodeSetup(cmd)
      println()
    }
  
    println()
    println(s"The previous version has been backed up to $backup")
    println(s"automod has been updated to v$latest!")
  }

  println(header)
  println(
    s"""* Platform: $osKind
       |* Automod directory: $automodDir""".stripMargin)
  // jd only matters for the diff commands; the usmap only for UE (non-unity) games —
  // skip the line when neither applies
  val using =
    (if (cliArgs.head == ".diff" || cliArgs.head == ".diff.into") Seq(s"jd v$jdVersion") else Seq.empty) ++
    (if (usmapUri.nonEmpty && !unityMode) Seq(usmapFilename) else Seq.empty)
  if (using.nonEmpty) println(s"* Using: ${using.mkString(", ")}")
  println(
    s"""* Parallelization enabled: ${!noPar}
       |* Maximum task logs: $maxLogs""".stripMargin)
  if (gamePakDirOpt.nonEmpty) println(s"* Game directory: $gameDir")
  println(s"* Working directory: $workingDir")
  if (argName.head != '.') println(s"* Mod name to generate: $argName")
  if (argName.head != '.' || argName.startsWith(".toml")) println(s"* Log directory: $logDir")
  println()
  val setup = init(gameDirOpt)

  def demoSbFirst(): Unit = demoSb(isAIO = false, isHard = false, isEffect = false)
  def demoSbAio(): Unit = demoSb(isAIO = true, isHard = false, isEffect = false)
  def demoSbAioHard(): Unit = demoSb(isAIO = true, isHard = true, isEffect = false)
  def demoSbEffect(): Unit = demoSb(isAIO = false, isHard = false, isEffect = true)
  def demoSbAll(): Unit = { demoSbFirst(); demoSbAio(); demoSbAioHard(); demoSbEffect() }

  argName match {
    case ".batch" =>
      checkPatchesDir()
      batch(
        parseOptions(cliArgs.drop(next).filter(_.startsWith("--")).toIndexedSeq),
        cliArgs.drop(next).filter(!_.startsWith("--")).toVector,
      )
    case ".retarget" =>
      if (gamePakDirOpt.isEmpty) exit(-1, ".retarget requires the game directory (supply -g and the data dir)")
      runRetargets(absPath(cliArgs(1)), gamePakDirOpt.get)
    case ".demo.sb" => demoSbAll()
    case ".demo.soa" => demoSoA()
    case ".diff" => diff(checkDir(absPath(cliArgs(1))), checkDir(absPath(cliArgs(2))), checkDirAvailable(absPath(cliArgs(3))))
    case ".diff.into" =>
      val out = checkDirAvailable(absPath(cliArgs(3)))
      val from = checkDir(absPath(cliArgs(1)))
      for (d <- os.list(checkDir(absPath(cliArgs(2)))) if os.isDir(d)) diff(from, d, out / d.last)
    case ".search" =>
      val input = checkFileExt(absPath(cliArgs(next)), "sam")
      val outDir = checkDirAvailable(absPath(cliArgs(next + 1)))
      search(flat = false, gamePakDirOpt, input, outDir)
    case ".search.flat" =>
      val input = checkFileExt(absPath(cliArgs(next)), "sam")
      val outDir = checkDirAvailable(absPath(cliArgs(next + 1)))
      search(flat = true, gamePakDirOpt, input, outDir)
    case ".setup" => if (setup) println("All modding tools have been set up")
    case ".setup.vscode" => vscode(if (cliArgs.length == 2) Some(absPath(cliArgs(1))) else None)
    case ".toml" | ".toml.all" => 
      val outDir = checkDirAvailable(absPath(cliArgs(next)))
      checkPatchesDir()
      toml(gamePakDirOpt, outDir, argName == ".toml")()
    case ".ttmapgen" =>
      os.proc(Seq[os.Shellable]("java", "-jar", ttmapgenExe) ++ (for (e <- cliArgs.tail) yield (e: os.Shellable))).call(stdout = os.Inherit, stderr = os.Inherit)
    case ".upgrade" => upgrade()
    case _ =>
      if (argName.head == '.') exit(-1, s"Unrecognized command $argName")
      checkPatchesDir()
      val modName = argName
      val option = parseOptions(cliArgs.drop(next).toIndexedSeq)
      genMod(modName, option)
  }
  println("... done!")
}

run()
