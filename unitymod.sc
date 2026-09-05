import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.ObjectNode
import com.github.jpabscale.asset4j.api.AssetService
import scala.collection.parallel.CollectionConverters._
import scala.jdk.CollectionConverters._

// Unity mod pipeline (plan §2.4 / M6). Unlike UE (retocPak extract .uasset, UAssetService
// to/from JSON, repak), asset4j handles a whole Unity AssetBundle (UnityFS) directly:
//   toJson: AssetService.toJsonNode(bundle, ttmap) -> JSON tree (Files[]/Asset/Objects[])
//   patch:  .@ JSONPath TOML files applied via patchlet.applyRawJsonPatches(unityMode=true)
//           (Unity has no name-keyed data table, so every patch is a .@ JSONPath)
//   toBin:  AssetService.fromJsonNode(tree, ttmap) -> bytes written back to the bundle
//
// Patch discovery reuses automod's standard patches/ scan: a patch file named
// `patches/<modName>/<bundle>.json.toml` is routed into rawJsonPatches keyed by
// `<bundle>.json` (automod.updatePatches), which is exactly what Unity needs (`.@`-only).
// The bundle name is that key minus the trailing `.json`.
object UnityMod {

  /** Maps a rawJsonPatches key (e.g. `data.unity3d.json`) to the bundle file name. */
  def bundleNameFromPatchKey(key: String): String = key.stripSuffix(".json")

  /**
   * Decodes a Unity bundle to JSON via asset4j. [bundle] is the bundle file path;
   * [ttmapPath] may be empty for type-tree games (no external schema).
   */
  def toJson(bundle: os.Path, ttmapPath: String): ObjectNode = {
    AssetService.toJsonNode(bundle.toNIO, if (ttmapPath.isEmpty) null else ttmapPath)
      .asInstanceOf[ObjectNode]
  }

  /**
   * Applies the .@ JSONPath patch trees for [bundleName] to [tree] (mutates in place).
   * asset4j's Jackson-backed JsonAst navigates the actual JsonNode tree, so the .@
   * replacements land directly on [tree] — no rebuild needed.
   */
  def patchJson(bundleName: String, tree: ObjectNode, patchTrees: Seq[automod.UAssetPropertyChanges]): Unit = {
    val ast = automod.jp.parse(tree)
    val origAst = automod.jp.parse(tree.deepCopy())
    val origAstPath = automod.jpPathList.parse(tree.deepCopy())
    for (patches <- patchTrees) {
      patchlet.applyRawJsonPatches(bundleName, ast, origAst, origAstPath, patches, unityMode = true)
    }
  }

  /** Re-encodes a patched JSON tree back to bundle bytes via asset4j. */
  def fromJson(tree: ObjectNode, ttmapPath: String): Array[Byte] =
    AssetService.fromJsonNode(tree, if (ttmapPath.isEmpty) null else ttmapPath)

  /**
   * The Unity mod driver, mirroring generateMod's skeleton for the UE pipeline. Reuses
   * automod's patch discovery (rawJsonPatches from `patches/<modName>/`), decodes each
   * bundle from [bundleDir], applies .@ patches, and writes the patched bundle to [output].
   */
  def runMod(
    modNameOpt: Option[String],
    bundleDir: os.Path,
    ttmapPath: String,
    includePatches: Boolean,
    noPar: Boolean,
  ): Unit = {
    val output = automod.workingDir / "out"
    os.remove.all(output)
    os.makeDir.all(output)

    // rawJsonPatches keys: "<bundle>.json" -> the .@ TOML trees for that bundle.
    // rawScriptPatches keys: "<file>" -> the raw .sc/.kt patch path.
    // Mirror UE's raw-file composition (automod.generateMod): merge every patch that
    // targets the same file (JSON `.@` patches, first-class class-scoped TOMLs, and raw
    // script patches alike), order them by discovery order (OrderedString), and apply them
    // sequentially threading `currentBytes` so each patch sees the previous one's output.
    sealed trait UnityChange
    case class JsonChange(tree: automod.UAssetPropertyChanges) extends UnityChange
    case class ClassChange(className: String, tree: automod.UAssetPropertyChanges) extends UnityChange
    case class RawChange(path: os.Path) extends UnityChange
    case class ClassRawChange(className: String, path: os.Path) extends UnityChange
    case class IlChange(path: os.Path) extends UnityChange

    val mergedByTarget = collection.mutable.LinkedHashMap.empty[
      String, Seq[(automod.OrderedString, UnityChange)]]
    def add(targetRel: String, nameKey: automod.OrderedString, change: UnityChange): Unit = {
      val cur = mergedByTarget.getOrElse(targetRel, Seq.empty)
      mergedByTarget.update(targetRel, cur :+ (nameKey, change))
    }
    for (key <- automod.rawJsonPatches.keys) {
      // bundle name is the rawJsonPatches key minus the trailing `.json`; normalize the
      // `$` subfolder separator just like raw script patches (processRaw).
      val targetRel = bundleNameFromPatchKey(key.value).replace(automod.uassetFilterSepChar, '/')
      add(targetRel, key, JsonChange(automod.rawJsonPatches(key)))
    }
    for ((bundle, patches) <- automod.rawClassTomlPatches; (className, tree) <- patches) {
      val targetRel = bundle.replace(automod.uassetFilterSepChar, '/')
      add(targetRel, automod.OrderedString(className, s"$bundle@$className"), ClassChange(className, tree))
    }
    for ((bundle, patches) <- automod.rawClassScriptPatches; (className, p) <- patches) {
      val targetRel = bundle.replace(automod.uassetFilterSepChar, '/')
      add(targetRel, automod.OrderedString(className, s"$bundle@$className"), ClassRawChange(className, p))
    }
    for ((nameKey, patch) <- automod.rawScriptPatches) {
      val targetRel = nameKey.value.replace(automod.uassetFilterSepChar, '/')
      add(targetRel, nameKey, RawChange(patch))
    }
    for ((nameKey, patch) <- automod.ilPatches) {
      val targetRel = nameKey.value.replace(automod.uassetFilterSepChar, '/')
      add(targetRel, nameKey, IlChange(patch))
    }

    if (mergedByTarget.isEmpty)
      automod.exit(-1, s"Could not find any Unity bundle or raw patches in ${automod.patchesDir}")

    def processFile(targetRel: String, merged: Seq[(automod.OrderedString, UnityChange)]): Unit = {
      val file = bundleDir / os.RelPath(targetRel)
      if (!os.isFile(file)) automod.exit(-1, s"Could not find file $targetRel in $bundleDir")
      println(s"Patching $targetRel ...")
      val origBytes = os.read.bytes(file)
      var currentBytes = origBytes
      val ttmapOpt = if (ttmapPath.nonEmpty && os.isFile(os.Path(ttmapPath))) ttmapPath else ""
      for ((nameKey, change) <- merged.sortBy(_._1)) {
        change match {
          case ClassChange(className, tree) =>
            // First-class `<ClassName>@<bundle>.toml`: decode ONLY the matching objects'
            // Data (cached per `<ClassName>@<file>`, no whole-file decode), apply each `.@`
            // section relative to the object's Data, re-encode via the targeted round-trip.
            val srcPath = file.toNIO
            val decoded = AssetService.decodeMatchingObjectsByScriptNameBytes(currentBytes, srcPath,
              if (ttmapOpt.isEmpty) null else ttmapOpt, className)
            val edited = new java.util.HashMap[java.lang.Long, com.fasterxml.jackson.databind.node.ObjectNode]()
            val dit = decoded.entrySet.iterator
            // `.@` paths in the class TOML are JSONPaths relative to the object's Data, and
            // must be `$`-rooted exactly like UE (no implicit root): `.@: $` = the object's
            // root, `.@: $.field.Array[*]` = nested. patchlet enforces the `$`/`/` prefix.
            val scoped = tree
            while (dit.hasNext) {
              val e = dit.next
              val node = e.getValue
              val ast = automod.jp.parse(node)
              val origAst = automod.jp.parse(node.deepCopy)
              val origAstPath = automod.jpPathList.parse(node.deepCopy)
              patchlet.applyRawJsonPatches(className, ast, origAst, origAstPath, scoped, unityMode = true)
              edited.put(e.getKey, ast.json[JsonNode].asInstanceOf[ObjectNode])
            }
            currentBytes = AssetService.patchObjectsFromDataByScriptNameBytes(currentBytes, srcPath,
              if (ttmapOpt.isEmpty) null else ttmapOpt, className, edited)
          case JsonChange(tree) =>
            // JSON `.@` patch (whole-file, e.g. `resources.json.toml`): decode the current
            // bytes, apply the .@ replacements, re-encode.
            val node = AssetService.toJsonNodeBytes(currentBytes, file.toNIO,
              if (ttmapOpt.isEmpty) null else ttmapOpt).asInstanceOf[ObjectNode]
            patchJson(bundleNameFromPatchKey(nameKey.value), node, Seq(tree))
            currentBytes = AssetService.fromJsonNode(node, if (ttmapOpt.isEmpty) null else ttmapOpt)
          case ClassRawChange(className, p) =>
            // First-class `<ClassName>@<bundle>.kt/.sc/...`: decode ONLY the matching objects'
            // Data, run the script body as the transform over `v.objects`, re-encode via the
            // targeted round-trip. The script never sees file locations — automod owns them.
            val srcPath = file.toNIO
            val decoded = AssetService.decodeMatchingObjectsByScriptNameBytes(currentBytes, srcPath,
              if (ttmapOpt.isEmpty) null else ttmapOpt, className)
              .asInstanceOf[java.util.Map[java.lang.Long, com.fasterxml.jackson.databind.node.ObjectNode]]
            if (decoded.isEmpty) automod.exit(-1,
              s"Class patch $p matched no $className objects in $targetRel")
            val ctx = scala.collection.immutable.Map[String, Any](
              "orig" -> origBytes, "current" -> currentBytes, "ttmap" -> ttmapPath,
              "path" -> file.toString, "originalPath" -> file.toString,
              "patchDir" -> (p / os.up).toString, "className" -> className)
            val edited = patchlet.evalRawScriptScoped(
              automod.langForRawExt(p.ext), p, nameKey.value, ctx, decoded)
            currentBytes = AssetService.patchObjectsFromDataByScriptNameBytes(currentBytes, srcPath,
              if (ttmapOpt.isEmpty) null else ttmapOpt, className, edited)
          case RawChange(p) =>
            // Raw script patch: `v.orig` is the file's original bytes, `v.current` is the
            // accumulated bytes from the patches so far — same contract as UE's raw files.
            // File-location/external/ttmap plumbing is consumed inside `v.toJson`/`v.fromJson`
            // /`v.resource` (no temp files, no paths exposed to the script).
            val ctx = scala.collection.immutable.Map[String, Any](
              "orig" -> origBytes, "current" -> currentBytes, "ttmap" -> ttmapPath,
              "path" -> file.toString, "originalPath" -> file.toString,
              "patchDir" -> (p / os.up).toString)
            currentBytes = patchlet.evalRawScript(automod.langForRawExt(p.ext), p, nameKey.value, ctx)
          case IlChange(p) =>
            // IL patch rule file (.il.toml) applied via the dnlib4j IL engine (ilengine.sc).
            val root = ilengine.IlEngine.parseToml(p)
            currentBytes = ilengine.IlEngine.applyIlPatch(root, currentBytes)
        }
      }
      val dest = output / os.RelPath(targetRel)
      os.makeDir.all(dest / os.up)
      os.write.over(dest, currentBytes)
      // Unity Mono DLLs must be executable on Linux (a plain overwrite drops the +x bit,
      // which breaks loading the modded assembly). No permission inherit — only chmod DLLs.
      if (dest.ext.toLowerCase == "dll") dest.toIO.setExecutable(true)
      println(s"... done. Wrote $dest")
    }

    val targets = mergedByTarget.toSeq
    if (noPar) {
      for ((targetRel, merged) <- targets) processFile(targetRel, merged)
    } else {
      targets.par.foreach { case (targetRel, merged) => processFile(targetRel, merged) }
    }
    println()

    modNameOpt match {
      case Some(modName) =>
        packMod(modName, output, includePatches, bundleDir)
      case _ =>
    }
  }

  /**
   * Assembles the mod archive: copies the patched bundles + .included assets + patches
   * (when requested) into a temp mod dir, then zips/7zs it — mirroring automod.packMod's
   * tail but without the UE repak step.
   */
  def packMod(modName: String, output: os.Path, includePatches: Boolean, bundleDir: os.Path): os.Path = {
    val tempDir = automod.workingDir / ".temp"
    val modDir = tempDir / modName
    os.remove.all(modDir)
    val pack = automod.workingDir / s"$modName.${automod.modExt}"
    os.remove.all(pack)
    os.makeDir.all(modDir)

    def includeAssets(includedDir: os.Path): Unit = {
      if (os.exists(includedDir)) {
        println("Copying included files")
        for (p <- os.walk(includedDir) if os.isFile(p)) {
          val relPath = p.relativeTo(includedDir)
          val dest = output / relPath
          os.makeDir.all(dest / os.up)
          os.copy(p, dest)
          println(s"* Added $dest")
        }
        println()
      }
    }
    includeAssets(automod.patchesDir / ".included")
    includeAssets(automod.patchesDir / automod.gameId / modName / ".included")

    if (os.exists(output)) {
      for (p <- os.walk(output) if os.isFile(p)) {
        val relPath = p.relativeTo(output)
        val dest = modDir / relPath
        os.makeDir.all(dest / os.up)
        os.copy(p, dest)
      }
    }

    // Embed the cross-platform installer (backs up originals, --restore undoes).
    val installer = automod.workingDir / "tools" / "install.py"
    if (os.isFile(installer)) {
      os.copy(installer, modDir / "install.py")
    }

    if (includePatches) {
      println()
      println(s"Copying patches ...")
      for (p <- os.walk(automod.patchesDir) if os.isFile(p) &&
             (p.ext == "toml" || p.ext == "patch") &&
             p.relativeTo(automod.patchesDir).segments.forall(_.head != '.')) {
        val relPath = p.relativeTo(automod.patchesDir / os.up)
        val dest = modDir / relPath
        os.makeDir.all(dest / os.up)
        os.copy.over(p, dest)
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
    automod.modExt match {
      case "zip" => os.proc(automod.zipExe, "a", s"-t${automod.modExt}", pack, modName).call(cwd = tempDir)
      case "7z" => os.proc(automod.zipExe, "a", s"-t${automod.modExt}", "-mx=9", "-mfb=273", pack, modName).call(cwd = tempDir)
    }
    println()
    pack
  }
}
