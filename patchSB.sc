import sbmod._
import scala.collection.immutable.TreeMap

def patchEffect(obj: UAssetApi.Struct): Unit = {
  val name = obj.name
  name match {

    // based on https://www.nexusmods.com/stellarblade/mods/802
    case "N_Drone_Scan" => 
      obj("LifeTime") = 30d

    // based on https://www.nexusmods.com/stellarblade/mods/897
    case _ if name.startsWith("P_Eve_SkillTree_Just") && (name.contains("BetaGauge") || name.contains("BurstGauge")) =>
      val cmv = name match {
        case "P_Eve_SkillTree_JustParry_BetaGauge1" => 8d
        case "P_Eve_SkillTree_JustParry_BetaGauge2" => 6d
        case "P_Eve_SkillTree_JustEvade_BurstGauge1" => 4d
        case "P_Eve_SkillTree_JustEvade_BurstGauge2" => 3d
      }
      obj("CalculationMultipleValue") = cmv
      obj("OverlapCount") = 1
      obj("LifeType") = "EffectLifeType_IndependentTime"
      obj("LifeTime") = 11d
      obj("StartDelayTime") = 1d
      obj("LoopIntervalTime") = 1d
      obj("ActiveTargetFilterAlias") = "Self"
      obj("LoopTargetFilterAlias") = "Self"

    // ... add more cases for other EffectTable properties of interest here, e.g.,
    //case _ if name.startsWith("Gear_") && name.contains("_3_MK2") && !name.endsWith("_HitDmgUp_Melee") =>
    //  obj("CalculationValue") = obj[Double]("CalculationValue") * 2

    case _ =>
  }
}

def patchTargetFilter(obj: UAssetApi.Struct): Unit = {
  val name = obj.name
  name match {

    // based on https://www.nexusmods.com/stellarblade/mods/802
    case _ if name.startsWith("N_Drone_Normal_Scan1_1_Target_") =>
      obj("FarDistance") = 30000d
      obj("TargetCheckValue1") = 3000d

    case _ =>
  }
}

def patches: DataTableCodePatches = {
  var r: DataTableCodePatches = TreeMap(
    // add more/change to uasset patching of interest here, e.g.,
    //"TargetFilterTable" -> patchTargetFilter _,
    "EffectTable" -> patchEffect _  // comment in this line to disable modding via code
  )
  return r
}