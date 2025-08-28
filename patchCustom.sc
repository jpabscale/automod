import com.fasterxml.jackson.databind.JsonNode
import scala.collection.immutable.{TreeMap, TreeSet}

lazy val uassetNames: TreeSet[String] = TreeSet(
  // insert the uasset name that you want to handle in the patch method here (without file extension)
) 

// This method will be called for any uasset in the uassetNames, as well as for uasset not handled by the script code
// The passed ast's JSON schema is UAssetAPI's; see the UAssetApi.new... methods in UAssetApi.sc for a quick glimpse of it 
def patch(uassetName: String, ast: JsonNode, patchTreeOpt: Option[sbmod.UAssetPropertyChanges]): Boolean = {

  // insert your custom patching code here

  return false // return true if you can modify the ast successfully
}