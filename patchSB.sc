import sbmod._
import scala.collection.immutable.TreeMap

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

def patches: PatchMap = {
  var r: PatchMap = TreeMap(
    // add more/change to uasset patching of interest here, e.g.,
    //"TargetFilterTable" -> patchTargetFilter _,
    "EffectTable" -> patchEffect _  // comment in this line to disable modding via code
  )
  return r
}