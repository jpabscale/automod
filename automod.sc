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

var version = "3.3.8"
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

if (osKind.isMac) exit(-1, s"Unsupported platform: .NET 8 for macOS does not currently work well enough for UAssetCLI")

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
var maxLogs = 30
var noPar = false
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
      case Array("-p", _*) =>
        noPar = true
        r = r.tail
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

noPar = noPar || osKind.isLinux || osKind.isMac || osKind.isArm

def exit(code: Int, msg: String = null): Nothing = {
  Option(msg).foreach((if (code == 0) Console.out else Console.err).println(_))
  System.exit(code)
  throw new RuntimeException
}

val zipToolVersion = "25.01"
var modExt = "zip"
val usmapUrlPrefix = "https://github.com/jpabscale/automod/releases/download/usmap/"
val autoupdateUsmaps = TreeSet[String]()

val sbGameId = "SB"
val soaGameId = "SandsOfAura"
val pal7GameId = "Pal7"
val kenaGameId = "Kena"
val wantedDeadGameId = "WDGame"

class Game {
  @BeanProperty var aesKey: String = ""
  @BeanProperty var contentPaks: String = ""
  @BeanProperty var directory: String = ""
  @BeanProperty var mapUri: String = ""
  @BeanProperty var repakPackOptions: String = ""
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

class Tools {
  @BeanProperty var fmodel: String = "fe4529908ee38e128f789f7e54b72310aac99dfb"
  @BeanProperty var jd: String = "2.3.0"
  @BeanProperty var repak: String = "0.2.3-pre.3"
  @BeanProperty var retoc: String = "0.1.5-pre.2"
  @BeanProperty var uassetCli: String = "1.0.3"
}

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
var patchesDir = workingDir / "patches"
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

val toolsDir = automodDir / "tools"
val usmapDir = toolsDir / "usmap"

val retocVersion = config.tools.retoc
val repakVersion = config.tools.repak
val uassetCliVersion = config.tools.uassetCli
var fmodelSha = config.tools.fmodel
var fmodelShortSha = fmodelSha.substring(0, 7)
val jdVersion = config.tools.jd
val ueVersion = config.game.unrealEngine
val ueVersionCode = s"UE${ueVersion.replace('.', '_')}"

val usmapUri = config.game.mapUri
val usmapFilename = { 
  var r = usmapUri.substring(usmapUri.lastIndexOf('/') + 1, usmapUri.length)
  if (r.endsWith(".7z")) r = r.substring(0, r.lastIndexOf('.'))
  r
}
val usmapPath = usmapDir / (if (usmapFilename.isEmpty || usmapFilename == "Mappings.usmap") s"$gameId.usmap" else usmapFilename)

val retocUrlPrefix = s"https://github.com/jpabscale/retoc/releases/download/v$retocVersion"
val repakUrlPrefix = s"https://github.com/jpabscale/repak/releases/download/v$repakVersion"
val uassetCliUrl = s"https://github.com/jpabscale/UAssetCLI/releases/download/v$uassetCliVersion/UAssetCLI.zip"
def fmodelUrl(sha: String): String = s"https://github.com/4sval/FModel/releases/download/qa/$sha.zip"
val jdUrlPrefix = s"https://github.com/josephburnett/jd/releases/download/v$jdVersion"
val z7rUrl = s"https://github.com/ip7z/7zip/releases/download/$zipToolVersion/7zr.exe"
val z7UrlPrefix = s"https://github.com/ip7z/7zip/releases/download/$zipToolVersion"
def vsixUrl = s"https://github.com/jpabscale/automod/releases/download/automod-vsix/automod-vscode-$version.vsix"
val retocExe = toolsDir / "retoc" / (if (osKind.isWin) "retoc.exe" else "retoc")
val repakExe = toolsDir / "repak" / (if (osKind.isWin) "repak.exe" else "repak")
val retocPakExe = if (config.game.zen) retocExe else repakExe
val fmodelExe = toolsDir / "FModel.exe"
val jdExe = toolsDir / (if (osKind.isWin) "jd.exe" else "jd")
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
val uassetCliDir = toolsDir / "UAssetCLI"
val uassetGuiSettingsDir = localAppData / "UAssetGUI"
val uassetGuiConfig = uassetGuiSettingsDir / "config.json"
val uassetGuiMappingsDir = uassetGuiSettingsDir / "Mappings"
val automodGameCacheDir = automodDir / ".cache" / (if (usmapFilename.nonEmpty) usmapFilename.replace(".usmap", "") else gameId) / gameId
val cacheDir = workingDir / ".cache"
val tempDir = localAppData / "Temp" / "automod"
val setupVscodeDir = tempDir / ".setup.vscode.dir"
lazy val dotnet = if (os.exists(os.home / ".dotnet" / "dotnet")) absPath(os.home / ".dotnet" / "dotnet") else "dotnet"

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

  if (!os.exists(retocExe)) {
    setup = false
    println(s"Setting up retoc v$retocVersion in $toolsDir ...")
    val retocBundleName = osKind match {
      case OsKind.WinAmd64 | OsKind.WinArm64 => "retoc_cli-x86_64-pc-windows-msvc.zip"
      case OsKind.MacAmd64 => "retoc_cli-x86_64-apple-darwin.tar.xz"
      case OsKind.LinuxAmd64 => "retoc_cli-x86_64-unknown-linux-gnu.tar.xz"
      case OsKind.LinuxArm64 => "retoc_cli-aarch64-unknown-linux-gnu.tar.xz"
      case OsKind.MacArm64 => "retoc_cli-aarch64-apple-darwin.tar.xz"
    }
    val retocBundle = downloadCheck(s"$retocUrlPrefix/$retocBundleName")
    os.remove.all(retocExe / os.up)
    if (osKind.isWin) {
      os.makeDir.all(retocExe / os.up)
      os.proc(zipExe, "x", retocBundle).call(cwd = retocExe / os.up)
    } else {
      os.proc("tar", "xf", retocBundle).call(cwd = toolsDir)
      for (p <- os.list(toolsDir) if os.isDir(p) && p.last.startsWith("retoc_cli-")) {
        os.move.over(p, toolsDir / "retoc")
      }
    }
    os.remove.all(retocBundle)
    println()
  }

  if (!os.exists(repakExe)) {
    setup = false
    println(s"Setting up repak v$repakVersion in $toolsDir ...")
    val repackBundleName = osKind match {
      case OsKind.WinAmd64 | OsKind.WinArm64 => "repak_cli-x86_64-pc-windows-msvc.zip"
      case OsKind.MacAmd64 => "repak_cli-x86_64-apple-darwin.tar.xz"
      case OsKind.LinuxAmd64 => "repak_cli-x86_64-unknown-linux-gnu.tar.xz"
      case OsKind.LinuxArm64 => "repak_cli-aarch64-unknown-linux-gnu.tar.xz"
      case OsKind.MacArm64 => "repak_cli-aarch64-apple-darwin.tar.xz"
    }
    val repakBundle = downloadCheck(s"$repakUrlPrefix/$repackBundleName")
    os.remove.all(repakExe / os.up)
    if (osKind.isWin) {
      os.makeDir.all(repakExe / os.up)
      os.proc(zipExe, "x", repakBundle).call(cwd = repakExe / os.up)
    } else {
      os.proc("tar", "xf", repakBundle).call(cwd = toolsDir)
      for (p <- os.list(toolsDir) if os.isDir(p) && p.last.startsWith("repak_")) {
        os.move.over(p, toolsDir / "repak")
      }
    }
    os.remove.all(repakBundle)
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

  if (usmapUri.nonEmpty && !os.exists(usmapPath)) {
    setup = false
    println(s"Setting up $usmapPath ...")
    val f = downloadCheck(usmapUri)
    if (f.ext == "7z") os.proc(zipExe, "x", f.last).call(cwd = f / os.up, stdout = os.Inherit, stderr = os.Inherit)
    os.move.over(f / os.up / usmapFilename, usmapPath)
    os.remove.all(f)
    println()
  }

  {
    val dest = uassetGuiMappingsDir / usmapPath.last
    if (usmapUri.nonEmpty && !os.exists(dest)) {
      setup = false
      os.makeDir.all(uassetGuiMappingsDir)
      os.copy.over(usmapPath, dest)
      println(s"Copied map file to $dest")
      println()
    }
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

  if (!os.exists(configPath) && automodDir.toString == workingDir.toString) writeConfig(config)

  setup
}

def initCache(hasGameDir: Boolean): Unit = {
  if (usmapUri.startsWith(usmapUrlPrefix) && (autoupdateUsmaps.contains(usmapPath.baseName) || !os.exists(automodGameCacheDir) && !hasGameDir)) {
    val msg = s"Setting up $automodGameCacheDir ..."
    download(usmapUri.replace("/usmap/", "/cache/").replace(".usmap.7z", ".7z"), 
             if (os.exists(automodGameCacheDir)) Some(msg) else None) match {
      case Some(p) =>
        if (!os.exists(automodGameCacheDir)) println(msg)
        os.remove.all(automodGameCacheDir) 
        os.proc(zipExe, "x", p).call(cwd = automodDir, stdout = os.Inherit, stderr = os.Inherit)
        os.remove.all(p)
        println()
      case _ =>
    }
    println()
  }
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
              logPatch(uassetName, "", console = false)
            case "toml" =>
              val uassetName = p.baseName
              logPatch(uassetName, s"Loading $p ...", console = true)
              var relPath = p.relativeTo(patchesDir).toString
              if (osKind.isWin) relPath = relPath.replace('/' , '\\')
              map = applyChanges(relPath, map, uassetName, tomlFilePatches(p))
              logPatch(uassetName, "", console = false)
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
  val p = logDir / s"$uassetName.log"
  val key = absPath(p)
  var q = logs.get(absPath(logDir / s"$uassetName.log"))
  if (q == null) {
    os.makeDir.all(logDir)
    q = new java.io.BufferedWriter(new java.io.FileWriter(key))
    logs.put(key, q)
  }
  q.write(s"$line${util.Properties.lineSeparator}")
}

def logFlush(uassetName: String): Unit = {
  val key = absPath(logDir / s"$uassetName.log")
  val q = logs.get(key)
  if (q != null) {
    logs.remove(key)
    q.flush()
    q.close()
  }
}

def logFlush(): Unit = {
  for (w <- logs.values.asScala) try w.flush() finally w.close()
}

def checkPatchesDir(): Unit = if (!os.isDir(patchesDir)) exit(-1, s"Missing directory: $patchesDir")

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
  initCache(gamePakDirOpt.nonEmpty)

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

  def unpackJson(n: String): os.Path = {
    var name = n
    if (name.contains(uassetFilterSepChar)) {
      name = name.substring(name.lastIndexOf(uassetFilterSepChar) + 1)
    }
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
    val retocPakCopyDir = outputName / retocPakExe.baseName
    val uassetCliCopyDir = outputName / "uassetgui"
    val uassetCliDll = uassetCliCopyDir / "UAssetCLI.dll"
    val retocPakExeCopy = retocPakCopyDir / retocPakExe.last
    val uassetFilename = s"$name.uasset"
    val uexpFilename = s"$name.uexp"

    os.makeDir.all(retocPakCopyDir)
    os.copy.over(retocPakExe, retocPakExeCopy)

    println(s"Extracting $uassetFilename ...")
    val args = if (config.game.zen) {
                 val filterName = s"${n.replace(uassetFilterSepChar, '/')}.uasset"
                 retocPak(retocPakExeCopy, "to-legacy", "--verbose", "--no-shaders", "--no-compres-shaders", "--no-parallel", 
                          "--version", ueVersionCode, "--filter", filterName, gamePakDir, retocPakCopyDir)
               } else {
                 val filterName = s"${n.replace(uassetFilterSepChar, '/')}"
                 var r = retocPak(retocPakExeCopy, "unpack", "-o", retocPakCopyDir, "-i", s"**/$filterName.*")
                 for (p <- os.list(gamePakDir) if p.ext == "pak") r :+= p
                 r
               }
    val pRetocPak = os.proc(args: _*)
    if (pRetocPak.call(check = false, cwd = retocPakCopyDir, stdout = os.Inherit, stderr = os.Inherit).exitCode != 0 || !os.exists(retocPakCopyDir / gameId) || os.walk(retocPakCopyDir / gameId).isEmpty)
      retocPakFailed(s"extract $uassetFilename (double check the .uasset name)", pRetocPak, retocPakCopyDir)
    println(s"... done extracting $uassetFilename")
    
    var uasset: os.Path = null
    for (p <- os.walk(outputName) if os.isFile(p))
      if (p.last == uassetFilename) uasset = p 
      else if (p.last == uexpFilename) {}
      else os.remove(p)

    if (uasset == null) retocPakFailed(s"extract $uassetFilename (no result)", pRetocPak, retocPakCopyDir)
    val relPath = uasset.relativeTo(retocPakCopyDir)
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
    val pUassetGui = os.proc(dotnet, uassetCliDll, "tojson", uasset, json, s"VER_$ueVersionCode", usmapPath.baseName)
    if (pUassetGui.call(check = false, cwd = uassetCliCopyDir, stdout = os.Inherit, stderr = os.Inherit, env = env).exitCode != 0) 
      uassetCliFailed(s"convert $uasset to JSON", pUassetGui, uassetCliCopyDir, repack = false)
    os.copy.over(uassetCliCopyDir / json, r)
    os.copy.over(r, jsonCache)
    println(s"... done converting to $r")

    uassetNamePathMap.put(name, relPath)
    os.remove.all(outputName)

    r
  }

  def packJson(n: String, path: os.Path): Unit = {
    var name = n
    if (name.contains(uassetFilterSepChar)) {
      name = name.substring(name.lastIndexOf(uassetFilterSepChar) + 1)
    }
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
    val pUassetGui = os.proc(dotnet, uassetCliDll , "fromjson", pathCopy, uasset, usmapPath.baseName)
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

    val includedDir = patchesDir / ".included"
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

    os.makeDir.all(modDir)
    val utocPak = modDir / (if (config.game.zen) s"${modName}_P.utoc" else if (modName.head.toString.toIntOption.nonEmpty) s"pakChunk${modName}_P.pak" else s"pakChunk888-${modName}_P.pak")
    println(s"Converting to $utocPak ...")
    val args: Seq[os.Shellable] = if (config.game.zen) retocPak(retocPakExe, "to-zen", "--no-parallel", "--version", ueVersionCode, output, utocPak)
                                  else retocPak(retocPakExe, (Seq[os.Shellable]("pack") ++ repakPackOptions ++ Seq[os.Shellable](output, utocPak)): _*)
    val pRetocPak = os.proc(args: _*)
    if (pRetocPak.call(check = false, cwd = tempDir, stdout = os.Inherit, stderr = os.Inherit).exitCode != 0)
      retocPakFailed(s"pack $modName", pRetocPak, tempDir)
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

    println(s"Archiving $pack ...")
    modExt match {
      case "zip" => os.proc(zipExe, "a", s"-t$modExt", "-mtm-", pack, packDirName).call(cwd = tempDir)
      case "7z" => os.proc(zipExe, "a", s"-t$modExt", "-mx=9", "-mfb=273", "-mtm=off", pack, packDirName).call(cwd = tempDir)
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
    logFlush(uassetName)
  }

  try 
    if (noPar) uassetNames.foreach(patchUasset) 
    else uassetNames.toSeq.par.foreach(patchUasset)
  finally logFlush()
  
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
         |  "PreferredMappings": "${usmapPath.baseName}"
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

  val currMap = collection.mutable.HashMap.empty[String, (JsonAst, JsonAst, JsonAst)]
  val origMap = collection.mutable.HashMap.empty[String, JsonNode]
  if (!disableCodePatching)
    generateMod(addToFilePatches = true, None, gamePakDirOpt, disableFilePatching = true, disableCodePatching, dryRun = true, includePatches = false, currMap, origMap)()
  generateMod(addToFilePatches = true, None, gamePakDirOpt, disableFilePatching = false, disableCodePatching, dryRun = true, includePatches = false, currMap, origMap)()

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
    val p = path / s"$uassetName.toml"
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
  val oldConfigOpt = writeConfig(initConfig)

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
    oldConfigOpt.foreach(writeConfig)
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
       |                         | .upgrade
       |                         ]
       |
       | -s                   Disable Scala CLI server
       |
       |opt:
       | -g <game-id>         Active game identifier (default: SB)
       | -l <num>             Maximum task logs to keep (default: 30)
       | -p                   Disable parallelization
       | -c <license-path>    Include license file(s) in the generated mod
       |
       |option:
       | --dry-run            Disable actual mod generation and just test patches
       | --include-patches    Include patches in the generated mod
       | --no-code-patching   Disable code patching
       | --ultra-compression  Use 7z ultra compression
       |
       |.batch                Generate a mod for each sub-folder in patches${fsep}<game-id> 
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
       |.upgrade              Upgrade automod to the latest version""".stripMargin)
}

def checkDir(p: os.Path): os.Path = if (os.isDir(p)) p else exit(-1, s"$p is not a directory")
def checkFileExt(p: os.Path, ext: String): os.Path = if (os.isFile(p) && p.ext == ext) p else exit(-1, s"$p is not a file with .$ext extension")
def checkDirAvailable(p: os.Path): os.Path = if (os.isFile(p)) exit(-1, s"$p is a file") else p

class Options {
  var dryRun: Boolean = false
  var includePatches: Boolean = false 
  var noCodePatching: Boolean = false 
  var ultraCompression: Boolean = false
}

def parseOptions(next: Int): Options = {
  def redundant(option: String): Nothing = exit(-1, s"Redundant option $option")
  var r = new Options
  for (i <- next until cliArgs.length) {
    cliArgs(i) match {
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
    case ".batch" => if (cliArgs.tail.exists(!_.startsWith("--"))) printUsage()
    case ".diff" | ".diff.into" => if (cliArgs.length != 4) printUsage()
    case ".search" | ".search.flat" => if (cliArgs.length != 3) printUsage()
    case ".setup" => if (cliArgs.length != 1) printUsage()
    case ".setup.vscode" => if (cliArgs.length != 1 && cliArgs.length != 2) printUsage()
    case ".toml" | ".toml.all" => if (cliArgs.length != 2) printUsage()
    case ".upgrade" => if (cliArgs.length != 1) printUsage()
    case _ if argName.head != '.' => cliArgs.length >= 2 && !cliArgs(1).startsWith("--")
    case _ => printUsage()
  }

  if (!config.game.contentPaks.startsWith(s"$gameId/")) exit(-1, s"Invalid configuration for $gameId's contentPaks: ${config.game.contentPaks}")

  val gameDir = absPath(config.game.directory)
  val (gamePakDirOpt, gameDirOpt, next) = if (config.game.directory.nonEmpty) (Some(checkDir(gameDir / os.RelPath(config.game.contentPaks))), Some(gameDir), 1) 
                                          else (None, None, 1)

  def genMod(modName: String, options: Options): Unit =
    setUAssetGUIConfigAndRun(generateMod(addToFilePatches = false, Some(modName), gamePakDirOpt, 
                             disableFilePatching = false, options.noCodePatching, options.dryRun, options.includePatches))

  def batch(options: Options): Unit = {
    def hasTomlOrPatchFiles(root: os.Path): Boolean = {
      var r = false
      def rec(p: os.Path): Unit = {
        if (r) return
        if (os.isDir(p) && !p.last.startsWith(".")) {
          os.list(p).foreach(rec)
        } else if (os.isFile(p) && (p.ext == "toml" || p.ext == "patch") && !p.last.startsWith(".")) r = true
      }
      rec(root)
      r
    }
    val gamePatches = patchesDir / gameId
    var ok = false
    if (os.isDir(gamePatches)) {
      for (p <- os.list(gamePatches) if os.isDir(p) && !p.last.startsWith(".") && hasTomlOrPatchFiles(p)) {
        val modName = p.last
        val oldPatchesDir = patchesDir
        val oldLogDir = logDir
        try {
          patchesDir = p
          _patches = null
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
       |* Automod directory: $automodDir
       |* Using: retoc v$retocVersion, repak v$repakVersion, UAssetCLI v$uassetCliVersion, jd v$jdVersion, $usmapFilename""".stripMargin)
  if (osKind.isWin) println(s"* Extra: FModel @$fmodelShortSha")
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
    case ".batch" => checkPatchesDir(); batch(parseOptions(next))
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
      setUAssetGUIConfigAndRun(toml(gamePakDirOpt, outDir, argName == ".toml"))
    case ".upgrade" => upgrade()
    case _ =>
      if (argName.head == '.') exit(-1, s"Unrecognized command $argName")
      checkPatchesDir()
      val modName = argName
      val option = parseOptions(next)
      genMod(modName, option)
  }
  println("... done!")
}

run()