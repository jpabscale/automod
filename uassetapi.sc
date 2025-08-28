import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.{JsonNodeFactory, ArrayNode, BooleanNode, DoubleNode, IntNode, NullNode, ObjectNode, TextNode}
import scala.collection.immutable.TreeMap
import scala.collection.mutable.HashMap
import scala.jdk.CollectionConverters._
import scala.reflect.runtime.universe._

object Constants {
  val typeKey = ".type" 
  val valueKey = ".value"
  val nameKey = ".name"
  val uint32Key = ".uint32"
}

import Constants._

def toValue[T](node: JsonNode): Option[T] = {
  def toT(o: Any): T = o.asInstanceOf[T]

  node match {
    case node: BooleanNode => Some(toT(node.booleanValue))
    case node: IntNode => Some(toT(node.doubleValue))
    case node: DoubleNode => Some(toT(node.doubleValue))
    case node: ArrayNode => Some(toT((for (i <- 0 until node.size) yield toValue[Any](node.get(i)).get).toSeq))
    case node: ObjectNode => 
      var r = Map[String, Any]()
      for (property <- node.fieldNames.asScala) {
        r = r + (property -> toValue[Any](node.get(property)).get)
      }
      Some(toT(r))
    case node: TextNode => 
      val text = node.textValue
      text match {
        case "+0.0" | "-0.0" | "+0" | "-0" => Some(toT(0d))
        case _ => Some(toT(text))
      }
    case null | _: NullNode => None
    case _ => automod.exit(-1, s"Unsupported value (class: ${node.getClass}): '${node.toPrettyString}'")
  }
}

def fromValue(v: Any): JsonNode = {
  v match {
    case v: Boolean => BooleanNode.valueOf(v)
    case v: Int => IntNode.valueOf(v)
    case v: Double => DoubleNode.valueOf(v)
    case v: String => TextNode.valueOf(v)
    case v: Seq[_] => 
      val r = JsonNodeFactory.instance.arrayNode
      for (o <- v) {
        r.add(fromValue(o))
      }
      r
    case v: Map[_, _] =>
      val r = JsonNodeFactory.instance.objectNode
      for ((k, v) <- v) r.set[JsonNode](k.toString, fromValue(v))
      r
    case null => NullNode.instance
    case _ => automod.exit(-1, s"Unsupported value (class: ${v.getClass}): '$v'")
  }
}

case class Struct(uassetName: String, value: JsonNode, addToFilePatches: Boolean) {
  var objectMap: HashMap[String, ObjectNode] = {
    var r = HashMap.empty[String, ObjectNode]
    val values = value.get("Value").asInstanceOf[ArrayNode]
    for (j <- 0 until values.size) {
      val element = values.get(j).asInstanceOf[ObjectNode]
      val name = element.get("Name").asText
      r.put(name, element)
    }
    r
  }

  def obj(objName: String): ObjectNode = {
    objectMap.get(objName) match {
      case Some(obj) => return obj
      case _ => automod.exit(-1, s"Could not find $objName in $uassetName")
    }
  }

  def setJson(property: String, value: JsonNode): Option[JsonNode] = {
    def toEnumPrettyString(enumType: TextNode)(node: JsonNode): JsonNode = TextNode.valueOf(s"${enumType.asText}::${node.asText}")
    val (rOpt, valueOpt) = value match {
      case value: TextNode if value.asText.contains("::") => 
        var text = value.asText
        val enumType = TextNode.valueOf(text.substring(0, text.indexOf("::")))
        val oldEnumOpt = Option(obj(property).replace("EnumType", enumType)).map(_.toPrettyString)
        val newValue = TextNode.valueOf(text.substring(text.indexOf("::") + 2))
        val oldValueOpt = Option(obj(property).replace("Value", newValue))
        (oldValueOpt, Some(toEnumPrettyString(enumType)(newValue)))
      case _ =>
        val oldValueOpt = Option(obj(property).replace("Value", value))  
        (oldValueOpt, Option(value))
    }
    automod.logPatch(uassetName, s"* $name/$property: ${automod.toJsonPrettyString(rOpt)} => ${automod.toJsonPrettyString(valueOpt)}", console = false)
    if (addToFilePatches) automod.updatePatch(uassetName, name, property, automod.ValuePair(valueOpt, rOpt))
    rOpt
  }
  
  def set(name: String, value: Boolean): Boolean = setJson(name, BooleanNode.valueOf(value)).map(_.asBoolean).getOrElse(false)
  def set(name: String, value: Int): Int = setJson(name, IntNode.valueOf(value)).map(_.asInt).getOrElse(0)
  def set(name: String, value: Double): Double = setJson(name, DoubleNode.valueOf(value)).map(_.asDouble).getOrElse(0d)
  def set(name: String, value: String): String = setJson(name, TextNode.valueOf(value)).map(_.asText).orNull

  def update(name: String, value: Boolean): Boolean = set(name, value)
  def update(name: String, value: Int): Int = set(name, value)
  def update(name: String, value: Double): Double = set(name, value)
  def update(name: String, value: String): String = set(name, value)
  def update(name: String, value: JsonNode): Option[JsonNode] = setJson(name, value)

  def name: String = value.get("Name").asText
  def getJson(name: String): JsonNode = obj(name).get("Value")
  
  def getBoolean(name: String): Boolean = getJson(name).asBoolean
  def getInt(name: String): Int = getJson(name).asInt
  def getDouble(name: String): Double = getJson(name).asDouble
  def getString(name: String): String = getJson(name).asText

  def apply[T: TypeTag](name: String): T = typeOf[T] match {
    case t if t =:= typeOf[Boolean] => getBoolean(name).asInstanceOf[T]
    case t if t =:= typeOf[Int] => getInt(name).asInstanceOf[T]
    case t if t =:= typeOf[Double] => getDouble(name).asInstanceOf[T]
    case t if t =:= typeOf[String] => getString(name).asInstanceOf[T]
  }
}
