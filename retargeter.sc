// Port of uasset4j's animation retarget code into automod (decision #14). uasset4j stays a pure
// de/serializer (bytes <-> DecodedTrack); automod owns the retarget math, the merge policy and the
// fidelity validator. Mechanically ported from uasset4j:
//   animation/RetargetMath.kt, animation/Retargeter.kt, animation/RetargetValidator.kt
// Object name is `retargeter` (from the file name) to avoid colliding with automod.sc's
// `def retarget` CLI handler.

import com.github.jpabscale.uasset4j.animation.{AnimationCompressionFormat, DecodedTrack}
import com.github.jpabscale.uasset4j.exporttypes.AnimSequenceExport
import com.github.jpabscale.uasset4j.bones.FReferenceSkeleton
import com.github.jpabscale.uasset4j.unrealtypes.objects.core.math.{FQuat, FTransform, FVector}
import scala.jdk.CollectionConverters._

object RetargetMath {

  /** A local-space transform (rotation + translation + scale). */
  case class T(r: FQuat, t: FVector, s: FVector)

  def quatMultiply(a: FQuat, b: FQuat): FQuat = {
    val (ax, ay, az, aw) = (a.getX, a.getY, a.getZ, a.getW)
    val (bx, by, bz, bw) = (b.getX, b.getY, b.getZ, b.getW)
    new FQuat(
      aw * bx + ax * bw + ay * bz - az * by,
      aw * by - ax * bz + ay * bw + az * bx,
      aw * bz + ax * by - ay * bx + az * bw,
      aw * bw - ax * bx - ay * by - az * bz)
  }

  def quatConjugate(q: FQuat): FQuat = new FQuat(-q.getX, -q.getY, -q.getZ, q.getW)

  /** Rotate [v] by unit quaternion [q] (q·v·q⁻¹). */
  def rotateVector(q: FQuat, v: FVector): FVector = {
    val qv = new FVector(q.getX, q.getY, q.getZ)
    val t = cross(qv, v) * 2.0
    v + t * q.getW + cross(qv, t)
  }

  def fromTransform(x: FTransform): T = T(x.Rotation, x.Translation, x.Scale3D)

  /** UE `a * b` (VQS, no negative-scale matrix path). */
  def mult(a: T, b: T): T = {
    val r = quatMultiply(b.r, a.r)
    val s = new FVector(b.s.getX * a.s.getX, b.s.getY * a.s.getY, b.s.getZ * a.s.getZ)
    val t = rotateVector(b.r, new FVector(b.s.getX * a.t.getX, b.s.getY * a.t.getY, b.s.getZ * a.t.getZ)) + b.t
    T(r, t, s)
  }

  /** UE `FTransform.Inverse()`: translation = rotate(conj(r), -t ⊙ (1/s)). */
  def inverse(x: T): T = {
    val rInv = quatConjugate(x.r)
    val sInv = new FVector(1.0 / x.s.getX, 1.0 / x.s.getY, 1.0 / x.s.getZ)
    val neg = new FVector(-x.t.getX * sInv.getX, -x.t.getY * sInv.getY, -x.t.getZ * sInv.getZ)
    val tInv = rotateVector(rInv, neg)
    T(rInv, tInv, sInv)
  }

  /**
   * Delta retarget: `targetLocal = targetRestLocal · (sourceRestLocal⁻¹ · sourceLocal)`,
   * matching UE `FTransform` semantics exactly (relative = sourceLocal · sourceRest⁻¹,
   * then targetLocal = targetRest · relative). The keyframe's own scale channel passes through.
   */
  def retargetDelta(sourceRest: T, targetRest: T, sourceLocal: T): T = {
    val rel = mult(inverse(sourceRest), sourceLocal)
    val out = mult(targetRest, rel)
    T(out.r, out.t, sourceLocal.s)
  }

  def cross(a: FVector, b: FVector): FVector =
    new FVector(a.getY * b.getZ - a.getZ * b.getY, a.getZ * b.getX - a.getX * b.getZ, a.getX * b.getY - a.getY * b.getX)

  /** Length of the local-space translation vector (bone length for scale mode). */
  def length(v: FVector): Double = math.sqrt(v.getX * v.getX + v.getY * v.getY + v.getZ * v.getZ)

  implicit class VecOps(val v: FVector) {
    def +(o: FVector): FVector = new FVector(v.getX + o.getX, v.getY + o.getY, v.getZ + o.getZ)
    def -(o: FVector): FVector = new FVector(v.getX - o.getX, v.getY - o.getY, v.getZ - o.getZ)
    def *(k: Double): FVector = new FVector(v.getX * k, v.getY * k, v.getZ * k)
    def *(o: FVector): FVector = new FVector(v.getX * o.getX, v.getY * o.getY, v.getZ * o.getZ)
  }
}

/** Retarget mode. */
sealed trait RetargetMode
object RetargetMode {
  case object DELTA extends RetargetMode
  case object SCALE extends RetargetMode
  case object COPY extends RetargetMode
}

/** Per-bone override: which retarget mode to use, and whether the bone is held at identity. */
final case class BoneOverride(mode: RetargetMode = null, freeze: Boolean = false)

/**
 * Retargets an AnimSequence's keyframes from one skeleton to another by name, then re-encodes.
 * See uasset4j `Retargeter` for the mode semantics (DELTA/SCALE/COPY + per-bone overrides).
 */
object Retargeter {

  /** Result of [retarget]: whether it applied, plus track stats. */
  class Result {
    var applied: Boolean = false
    var shared: Int = 0
    var unmapped: Int = 0
    /** Target bones the source animation does not drive (left to the base animation in merge mode). */
    var uncovered: scala.collection.mutable.ListBuffer[String] = scala.collection.mutable.ListBuffer.empty
  }

  def retarget(
    srcExport: AnimSequenceExport,
    sourceRef: FReferenceSkeleton,
    targetRef: FReferenceSkeleton,
    mode: RetargetMode,
    dummyBase: Int,
    bakeFormat: AnimationCompressionFormat = AnimationCompressionFormat.ACF_Float96NoW,
    eliminate: Boolean = true,
    overrides: Map[String, BoneOverride] = Map.empty,
  ): Result = {
    val result = new Result
    val originalTable = srcExport.getCompressedTrackToSkeletonMapTable
    if (originalTable == null) return result
    val tracks = srcExport.decodeCompressedData()
    if (tracks == null) return result
    // Capture the compressed-table offset BEFORE setRawTrackTable discards the scan: the rebuilt
    // Extras keeps the table at this same offset, so encode re-uses it via explicitTableOffset
    // (the post-remap table can't be re-found by scanning the still-original Extras).
    val tableOffset = srcExport.getCompressedTrackToSkeletonMapTableOffset
    val sourceInfo = sourceRef.FinalRefBoneInfo
    val targetInfo = targetRef.FinalRefBoneInfo
    val targetNameToIdx = targetRef.FinalNameToIndexMap

    val remapped: java.util.ArrayList[Integer] = new java.util.ArrayList[Integer]()
    for (i <- 0 until originalTable.size) {
      val srcIdx = originalTable.get(i).intValue
      val name = if (srcIdx >= 0 && srcIdx < sourceInfo.size) sourceInfo.get(srcIdx).Name else null
      val dstIdx = if (name != null) targetNameToIdx.get(name) else null
      if (name != null && dstIdx != null) {
        remapped.add(dstIdx)
        val o = overrides.getOrElse(name, null)
        val useMode = if (o == null || o.mode == null) mode else o.mode
        if (o != null && o.freeze) {
          // Hold at identity (a bone the source animates but the override freezes).
          val frozen = new DecodedTrack
          frozen.KeyQuat = new java.util.ArrayList[FQuat](java.util.Collections.singletonList(new FQuat(0.0, 0.0, 0.0, 1.0)))
          frozen.KeyPos = new java.util.ArrayList[FVector](java.util.Collections.singletonList(new FVector(0.0, 0.0, 0.0)))
          frozen.KeyScale = new java.util.ArrayList[FVector](java.util.Collections.singletonList(new FVector(1.0, 1.0, 1.0)))
          tracks.set(i, frozen)
        } else if (useMode != RetargetMode.COPY) {
          val srcRest = if (srcIdx >= 0 && srcIdx < sourceRef.FinalRefBonePose.size) RetargetMath.fromTransform(sourceRef.FinalRefBonePose.get(srcIdx)) else null
          val tgtRest = if (dstIdx >= 0 && dstIdx < targetRef.FinalRefBonePose.size) RetargetMath.fromTransform(targetRef.FinalRefBonePose.get(dstIdx)) else null
          if (srcRest != null && tgtRest != null && tracks.get(i).getHasKeys) {
            useMode match {
              case RetargetMode.DELTA => retargetDelta(tracks.get(i), srcRest, tgtRest)
              case RetargetMode.SCALE => retargetScale(tracks.get(i), srcRest, tgtRest)
              case RetargetMode.COPY => {}
            }
          }
        }
        result.shared += 1
      } else {
        remapped.add(dummyBase + i)
        result.unmapped += 1
      }
    }

    srcExport.setCompressedTrackToSkeletonMapTable(remapped)
    srcExport.setRawTrackTable(remapped)
    result.applied = srcExport.encodeCompressedData(tracks, bakeFormat, null, eliminate, Integer.valueOf(tableOffset))
    result
  }

  /**
   * Merge-retarget: keep the target character's OWN animation ([baseExport]) and overwrite only
   * the tracks whose bones the source ([srcExport]) also drives. The output frame count is the
   * SOURCE's; base (kept) tracks are resampled up. [native] writes onto the source asset (native
   * instanced notify objects survive); otherwise onto the base asset.
   */
  def mergeRetarget(
    srcExport: AnimSequenceExport,
    baseExport: AnimSequenceExport,
    sourceRef: FReferenceSkeleton,
    targetRef: FReferenceSkeleton,
    mode: RetargetMode,
    bakeFormat: AnimationCompressionFormat = AnimationCompressionFormat.ACF_Float96NoW,
    retimeFrames: Int = 0,
    eliminate: Boolean = true,
    keepBones: Set[String] = Set.empty,
    overrides: Map[String, BoneOverride] = Map.empty,
    native: Boolean = false,
  ): Result = {
    val result = new Result
    val sourceTable = srcExport.getCompressedTrackToSkeletonMapTable
    if (sourceTable == null) return result
    val sourceTracks = srcExport.decodeCompressedData()
    if (sourceTracks == null) return result
    val baseTable = baseExport.getCompressedTrackToSkeletonMapTable
    if (baseTable == null) return result
    val baseTracks = baseExport.decodeCompressedData()
    if (baseTracks == null) return result
    if (baseTracks.size != baseTable.size) return result
    val sourceInfo = sourceRef.FinalRefBoneInfo
    val targetInfo = targetRef.FinalRefBoneInfo
    val targetNameToIdx = targetRef.FinalNameToIndexMap
    val targetFramesO = srcExport.compressedNumberOfFrames()
    val targetFrames = if (targetFramesO == null) 0 else targetFramesO.intValue

    // Map target bone name -> retargeted source track (after transforming to target space).
    val nameToRetargeted = scala.collection.mutable.HashMap.empty[String, DecodedTrack]
    val nameToFrozen = scala.collection.mutable.HashSet.empty[String]
    for (i <- 0 until sourceTable.size) {
      val srcIdx = sourceTable.get(i).intValue
      val name = if (srcIdx >= 0 && srcIdx < sourceInfo.size) sourceInfo.get(srcIdx).Name else null
      val dstIdx = if (name != null) targetNameToIdx.get(name) else null
      if (name != null && dstIdx != null && sourceTracks.get(i).getHasKeys) {
        val track = sourceTracks.get(i)
        val animated = track.KeyPos.size > 1 || track.KeyQuat.size > 1
        val o = overrides.getOrElse(name, null)
        val forceKeep = keepBones.contains(name) || (o != null && o.freeze)
        if (o != null && o.freeze) nameToFrozen.add(name)
        val srcRest = if (srcIdx >= 0 && srcIdx < sourceRef.FinalRefBonePose.size) RetargetMath.fromTransform(sourceRef.FinalRefBonePose.get(srcIdx)) else null
        val tgtRest = if (dstIdx >= 0 && dstIdx < targetRef.FinalRefBonePose.size) RetargetMath.fromTransform(targetRef.FinalRefBonePose.get(dstIdx)) else null
        val useMode = if (o == null || o.mode == null) mode else o.mode
        val injectStatic = useMode == RetargetMode.COPY
        if ((animated || injectStatic) && srcRest != null && tgtRest != null && !forceKeep) {
          if ((name == "Root" || name == "Bip001") && useMode != RetargetMode.COPY) {
            // Root-motion bones: preserve horizontal translation exactly, only re-aim rotation.
            retargetRootRotation(track, srcRest, tgtRest)
          } else if (useMode != RetargetMode.COPY) {
            useMode match {
              case RetargetMode.DELTA => retargetDelta(track, srcRest, tgtRest)
              case RetargetMode.SCALE => retargetScale(track, srcRest, tgtRest)
              case RetargetMode.COPY => {}
            }
          }
          nameToRetargeted.put(name, track)
          result.shared += 1
        }
      }
    }

    // Keep the base's exact track table: overwrite base tracks whose bone the source really
    // animates (delta/scale retargeted), keep the rest.
    val merged: java.util.ArrayList[DecodedTrack] = new java.util.ArrayList[DecodedTrack]()
    for (i <- 0 until baseTable.size) {
      val baseIdx = baseTable.get(i).intValue
      val name = if (baseIdx >= 0 && baseIdx < targetInfo.size) targetInfo.get(baseIdx).Name else null
      val injected = if (name != null) nameToRetargeted.get(name) else None
      if (injected.isDefined) {
        merged.add(injected.get)
      } else if (name != null && keepBones.contains(name)) {
        // Explicit per-bone keep: hold at IDENTITY (zero rotation/offset).
        val held = new DecodedTrack
        held.KeyQuat = new java.util.ArrayList[FQuat](java.util.Collections.singletonList(new FQuat(0.0, 0.0, 0.0, 1.0)))
        held.KeyPos = new java.util.ArrayList[FVector](java.util.Collections.singletonList(new FVector(0.0, 0.0, 0.0)))
        held.KeyScale = new java.util.ArrayList[FVector](java.util.Collections.singletonList(new FVector(1.0, 1.0, 1.0)))
        merged.add(held)
        result.uncovered += name
      } else {
        // Bones the source does not drive: hold at the target's BIND REST POSE.
        val restT = if (baseIdx >= 0 && baseIdx < targetRef.FinalRefBonePose.size) RetargetMath.fromTransform(targetRef.FinalRefBonePose.get(baseIdx)) else null
        val held = new DecodedTrack
        if (restT != null) {
          held.KeyQuat = new java.util.ArrayList[FQuat](java.util.Collections.singletonList(new FQuat(restT.r.getX, restT.r.getY, restT.r.getZ, restT.r.getW)))
          held.KeyPos = new java.util.ArrayList[FVector](java.util.Collections.singletonList(new FVector(restT.t.getX, restT.t.getY, restT.t.getZ)))
          held.KeyScale = new java.util.ArrayList[FVector](java.util.Collections.singletonList(new FVector(restT.s.getX, restT.s.getY, restT.s.getZ)))
        } else {
          val kept = baseTracks.get(i)
          if (!kept.KeyQuat.isEmpty) held.KeyQuat = new java.util.ArrayList[FQuat](java.util.Collections.singletonList(kept.KeyQuat.get(0)))
          if (!kept.KeyPos.isEmpty) held.KeyPos = new java.util.ArrayList[FVector](java.util.Collections.singletonList(kept.KeyPos.get(0)))
          if (!kept.KeyScale.isEmpty) held.KeyScale = new java.util.ArrayList[FVector](java.util.Collections.singletonList(kept.KeyScale.get(0)))
        }
        merged.add(held)
        if (name != null) result.uncovered += name
      }
    }

    // Expand the base's frame count to the source's (or the retime target) and update props.
    val sourceLength = srcExport.sequenceLength()
    val sourceRate = if (targetFrames > 0 && sourceLength != null && sourceLength.floatValue > 0f) sourceLength.floatValue / targetFrames else 1f / 30f
    val outFrames = if (retimeFrames > 1) retimeFrames else targetFrames
    val injectedNames = nameToRetargeted.keySet
    for (i <- 0 until merged.size) {
      val bi = baseTable.get(i).intValue
      val isInjected = bi >= 0 && bi < targetInfo.size && injectedNames.contains(targetInfo.get(bi).Name)
      if (outFrames > 1 && !isInjected) resampleToFrames(merged.get(i), outFrames)
    }
    // Write onto the SOURCE asset (native, keeps native notify objects) or the BASE asset.
    val outExport = if (native) srcExport else baseExport
    val srcTableOffset = if (native) srcExport.getCompressedTrackToSkeletonMapTableOffset else -1
    outExport.setCompressedNumberOfFrames(outFrames)
    outExport.updateFrameCountProperties(outFrames, sourceRate * outFrames)
    outExport.setCompressedTrackToSkeletonMapTable(new java.util.ArrayList[Integer](baseTable))
    outExport.setRawTrackTable(new java.util.ArrayList[Integer](baseTable))
    outExport.mirrorForceRootLock(srcExport)
    result.applied = outExport.encodeCompressedData(merged, bakeFormat, outFrames, eliminate, if (native) Integer.valueOf(srcTableOffset) else null)
    result
  }

  /** Root-motion retarget: preserve horizontal locomotion, ground the vertical to the target. */
  private def retargetRootRotation(track: DecodedTrack, srcRest: RetargetMath.T, tgtRest: RetargetMath.T): Unit = {
    val quats = track.KeyQuat.asScala
    val srcRestInv = RetargetMath.quatConjugate(srcRest.r)
    for (k <- 0 until quats.size) {
      // relative rotation = sourceRest^-1 * sourceLocal; then targetRest * relative
      val relR = RetargetMath.quatMultiply(srcRestInv, quats(k))
      quats(k) = RetargetMath.quatMultiply(relR, tgtRest.r)
    }
    val pos = track.KeyPos.asScala
    if (pos.nonEmpty) {
      val tgtBaseY = tgtRest.t.getY
      for (k <- 0 until pos.size) {
        val p = pos(k)
        pos(k) = new FVector(p.getX, tgtBaseY, p.getZ)
      }
    }
  }

  private def retargetDelta(track: DecodedTrack, srcRest: RetargetMath.T, tgtRest: RetargetMath.T): Unit = {
    val quats = track.KeyQuat.asScala
    val pos = track.KeyPos.asScala
    val scale = track.KeyScale.asScala
    val n = math.max(quats.size, pos.size)
    for (k <- 0 until n) {
      val srcLocal = RetargetMath.T(
        if (k < quats.size) quats(k) else new FQuat(0.0, 0.0, 0.0, 1.0),
        if (k < pos.size) pos(k) else new FVector(0.0, 0.0, 0.0),
        if (k < scale.size) scale(k) else new FVector(1.0, 1.0, 1.0))
      val targetLocal = RetargetMath.retargetDelta(srcRest, tgtRest, srcLocal)
      if (k < quats.size) quats(k) = targetLocal.r
      if (k < pos.size) pos(k) = targetLocal.t
      if (k < scale.size) scale(k) = targetLocal.s
    }
  }

  private def retargetScale(track: DecodedTrack, srcRest: RetargetMath.T, tgtRest: RetargetMath.T): Unit = {
    val srcLen = RetargetMath.length(srcRest.t)
    val tgtLen = RetargetMath.length(tgtRest.t)
    if (srcLen <= 1e-6) return
    val ratio = tgtLen / srcLen
    val pos = track.KeyPos.asScala
    for (k <- 0 until pos.size) pos(k) = new FVector(pos(k).getX * ratio, pos(k).getY * ratio, pos(k).getZ * ratio)
    // Compose rotations relative to rest so the pose lands on the target's rest frame.
    val quats = track.KeyQuat.asScala
    for (k <- 0 until quats.size) {
      val srcLocalRot = quats(k)
      // relativeRot = sourceRest.r⁻¹ · sourceLocal.r ; targetRot = targetRest.r · relativeRot
      val rel = RetargetMath.quatMultiply(RetargetMath.quatConjugate(srcRest.r), srcLocalRot)
      quats(k) = RetargetMath.quatMultiply(tgtRest.r, rel)
    }
  }

  /** Linear resample of each key array onto [frames] keys (map [0, frames) onto the source key span). */
  private def resampleToFrames(track: DecodedTrack, frames: Int): Unit = {
    def resampleVec(keys: java.util.List[FVector]): java.util.List[FVector] = {
      if (keys.size <= 1 || keys.size == frames) return keys
      val out = new java.util.ArrayList[FVector](frames)
      val last = keys.size - 1
      for (f <- 0 until frames) {
        val t = f.toDouble / (frames - 1) * last
        val i0 = math.min(math.floor(t).toInt, last - 1)
        val i1 = i0 + 1
        val frac = (t - i0).toFloat
        val a = keys.get(i0); val b = keys.get(i1)
        out.add(new FVector(a.getX + (b.getX - a.getX) * frac, a.getY + (b.getY - a.getY) * frac, a.getZ + (b.getZ - a.getZ) * frac))
      }
      out
    }
    def resampleQuat(keys: java.util.List[FQuat]): java.util.List[FQuat] = {
      if (keys.size <= 1 || keys.size == frames) return keys
      val out = new java.util.ArrayList[FQuat](frames)
      val last = keys.size - 1
      for (f <- 0 until frames) {
        val t = f.toDouble / (frames - 1) * last
        val i0 = math.min(math.floor(t).toInt, last - 1)
        val i1 = i0 + 1
        val frac = (t - i0).toFloat
        val a = keys.get(i0); val b = keys.get(i1)
        // nlerp (normalized) — fine for per-track resampling
        var x = a.getX + (b.getX - a.getX) * frac
        var y = a.getY + (b.getY - a.getY) * frac
        var z = a.getZ + (b.getZ - a.getZ) * frac
        var w = a.getW + (b.getW - a.getW) * frac
        val inv = 1.0 / math.sqrt(x * x + y * y + z * z + w * w)
        x *= inv; y *= inv; z *= inv; w *= inv
        out.add(new FQuat(x, y, z, w))
      }
      out
    }
    track.KeyPos = resampleVec(track.KeyPos)
    track.KeyQuat = resampleQuat(track.KeyQuat)
    track.KeyScale = resampleVec(track.KeyScale)
  }
}

/**
 * Automated check that a (merged) retargeted animation "moves like the source". See uasset4j
 * `RetargetValidator`: per-bone world-space movement correlation + missing-motion detection.
 */
object RetargetValidator {

  /** World-space transforms of every bone at every frame. */
  def worldPositions(
    ref: FReferenceSkeleton,
    tracks: java.util.List[DecodedTrack],
    table: java.util.List[Integer],
    numFrames: Int,
  ): Array[Array[RetargetMath.T]] = {
    val info = ref.FinalRefBoneInfo
    val nameToTrack = scala.collection.mutable.HashMap.empty[String, Int]
    for (i <- 0 until table.size) {
      val idx = table.get(i).intValue
      if (idx >= 0 && idx < info.size) nameToTrack.put(info.get(idx).Name, i)
    }
    val frames = scala.collection.mutable.ArrayBuffer.empty[Array[RetargetMath.T]]
    for (f <- 0 until numFrames) {
      val world = new Array[RetargetMath.T](info.size)
      def walk(i: Int): RetargetMath.T = {
        val cached = world(i)
        if (cached != null) return cached
        val parent = info.get(i).ParentIndex
        val local = trackLocal(tracks, nameToTrack.get(info.get(i).Name), f).getOrElse(RetargetMath.fromTransform(ref.FinalRefBonePose.get(i)))
        val w = if (parent >= 0) RetargetMath.mult(walk(parent), local) else local
        world(i) = w
        w
      }
      for (i <- 0 until info.size) walk(i)
      frames += world
    }
    frames.toArray
  }

  private def trackLocal(tracks: java.util.List[DecodedTrack], trackIdx: Option[Int], f: Int): Option[RetargetMath.T] = {
    trackIdx match {
      case None => None
      case Some(ti) =>
        val tr = tracks.get(ti)
        if (tr.KeyPos.isEmpty) return None
        val k = math.min(f, tr.KeyPos.size - 1)
        Some(RetargetMath.T(
          if (!tr.KeyQuat.isEmpty) tr.KeyQuat.get(math.min(f, tr.KeyQuat.size - 1)) else new FQuat(0.0, 0.0, 0.0, 1.0),
          tr.KeyPos.get(k),
          if (!tr.KeyScale.isEmpty) tr.KeyScale.get(math.min(f, tr.KeyScale.size - 1)) else new FVector(1.0, 1.0, 1.0)))
    }
  }

  /** Per-bone movement magnitude per frame (world translation deltas). */
  def movement(frames: Array[Array[RetargetMath.T]], numBones: Int): Array[Array[Double]] = {
    val out = Array.ofDim[Double](numBones, frames.length)
    for (f <- 1 until frames.length) {
      for (b <- 0 until numBones) {
        val a = frames(f - 1)(b); val c = frames(f)(b)
        if (a != null && c != null) {
          out(b)(f) = math.sqrt((c.t.getX - a.t.getX) * (c.t.getX - a.t.getX) + (c.t.getY - a.t.getY) * (c.t.getY - a.t.getY) + (c.t.getZ - a.t.getZ) * (c.t.getZ - a.t.getZ))
        }
      }
    }
    out
  }

  /** Correlation coefficient of two per-frame signals (source vs merged movement). */
  def correlation(a: Array[Double], b: Array[Double]): Double = {
    val n = math.min(a.length, b.length)
    if (n < 2) return 1.0
    var sa = 0.0; var sb = 0.0
    for (i <- 0 until n) { sa += a(i); sb += b(i) }
    val ma = sa / n; val mb = sb / n
    var num = 0.0; var da = 0.0; var db = 0.0
    for (i <- 0 until n) {
      val x = a(i) - ma; val y = b(i) - mb
      num += x * y; da += x * x; db += y * y
    }
    if (da <= 1e-12 && db <= 1e-12) return 1.0
    if (da <= 1e-12 || db <= 1e-12) return 0.0
    num / (math.sqrt(da) * math.sqrt(db))
  }

  case class BoneResult(name: String, ravenMoves: Double, mergedMoves: Double, corr: Double)

  /** Cosmetics (hair, body detail, drones) that don't drive the visible skeleton — ignore for fidelity. */
  private def isCosmetic(name: String): Boolean =
    name.startsWith("HBN") || name.startsWith("HBn") || name.startsWith("Ab_") || name.startsWith("Dm_") ||
      name.startsWith("Ab-") || name.startsWith("Dm-") || name.contains("Breast") || name.contains("Hair") || name.contains("Drone")

  class Report {
    var numFrames: Int = 0
    var sharedBones: Int = 0
    var bones: scala.collection.mutable.ListBuffer[BoneResult] = scala.collection.mutable.ListBuffer.empty
    /** Bones the source animates but the merged output leaves static (missing-motion symptom). */
    var missing: scala.collection.mutable.ListBuffer[String] = scala.collection.mutable.ListBuffer.empty
    /** Bones whose motion correlates poorly (retarget corrupted). */
    var poorCorrelation: scala.collection.mutable.ListBuffer[String] = scala.collection.mutable.ListBuffer.empty
  }

  def validate(
    srcRef: FReferenceSkeleton,
    srcTracks: java.util.List[DecodedTrack],
    srcTable: java.util.List[Integer],
    srcFrames: Int,
    mergedRef: FReferenceSkeleton,
    mergedTracks: java.util.List[DecodedTrack],
    mergedTable: java.util.List[Integer],
    mergedFrames: Int,
  ): Report = {
    val report = new Report
    report.numFrames = mergedFrames
    val srcInfo = srcRef.FinalRefBoneInfo
    val mrgInfo = mergedRef.FinalRefBoneInfo
    val mrgNameToIdx = mergedRef.FinalNameToIndexMap

    val srcPos = worldPositions(srcRef, srcTracks, srcTable, srcFrames)
    val mrgPos = worldPositions(mergedRef, mergedTracks, mergedTable, mergedFrames)
    val srcMov = movement(srcPos, srcInfo.size)
    val mrgMov = movement(mrgPos, mrgInfo.size)

    // Bones the source actually animates (driven by a track with keys).
    val srcDriven: Set[String] = (for (i <- 0 until srcTable.size) yield {
      val it = srcTable.get(i).intValue
      if (it >= 0 && it < srcInfo.size) Some(srcInfo.get(it).Name) else None
    }).flatten.toSet
    report.sharedBones = srcDriven.size

    for (name <- srcDriven if !isCosmetic(name)) {
      val mi = mrgNameToIdx.get(name)
      if (mi == null || mi.intValue < 0 || mi.intValue >= mrgInfo.size) {
        // Source animates this bone but the merged output has no track for it at all.
        report.missing += name
      } else {
        val si = srcRef.FinalNameToIndexMap.get(name)
        if (si != null) {
          val srcTotal = srcMov(si.intValue).sum
          val mrgTotal = mrgMov(mi.intValue).sum
          val corr = correlation(srcMov(si.intValue), mrgMov(mi.intValue))
          report.bones += BoneResult(name, srcTotal, mrgTotal, corr)
          if (srcTotal > 1.0 && mrgTotal < 0.5) report.missing += name
          else if (srcTotal > 1.0 && corr < 0.3) report.poorCorrelation += name
        }
      }
    }
    report
  }
}
