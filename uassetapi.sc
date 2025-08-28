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

/*
 * To create UAssetAPI's object data:
 * - BoolPropertyData, value should be an instance of BooleanNode 
 * - IntPropertyData, value should be an instance of IntNode 
 * - FloatPropertyData, value should be an instance of DoubleNode 
 * - StrPropertyData, value should be an instance of TextNode 
 * - ArrayPropertyData, value should be an instance of ArrayNode
 * - UInt32PropertyData, value should be an ObjectNode: { ".type": ".uint32", ".value": <unsigned-32-bit-int-value> }
 * - NamePropertyData, value should be an ObjectNode: { ".type": ".name", ".value": "<value>" }
 * - StructPropertyData, value should be an ObjectNode: { ".type": "<struct-type-name>", "<field-name-1>" : <field-value-1>, ... }
 */
def newData(path: Vector[String], property: String, value: JsonNode): ObjectNode = {
  value match {
    case value: BooleanNode => newBoolean(property, value)
    case value: IntNode => newUAssetApiIntPropertyData(property, value)
    case value: DoubleNode => newUAssetApiFloatPropertyData(property, value)
    case value: TextNode =>
      val text = value.asText
      val colonColonIndex = text.indexOf("::")
      if (colonColonIndex > 0) {
        val enumType = text.substring(0, colonColonIndex)
        val jsonValue = TextNode.valueOf(text.substring(colonColonIndex + 2))
        newEnum(property, enumType, jsonValue)
      } else {
        newString(property, value)
      }
    case value: ArrayNode =>
      val r = newUAssetApiArrayPropertyData(property)
      val elements = r.get("Value").asInstanceOf[ArrayNode]
      for (i <- 0 until value.size) {
        val elementName = i.toString
        elements.add(newData(path :+ elementName, elementName, value.get(i)))
      }
      r
    case value: ObjectNode =>
      value.get(typeKey) match {
        case v: TextNode => 
          v.textValue match {
            case `nameKey` =>
              Option(value.get(valueKey)) match {
                case Some(v: TextNode) => newName(property, v)
                case _ => sbmod.exit(-1, s"""Expecting a string $valueKey field for ${path.mkString("/")}""")
              }
            case `uint32Key` =>
              Option(value.get(valueKey)) match {
                case Some(v: IntNode) => newUInt32(property, v)
                case _ => sbmod.exit(-1, s"""Expecting a string $valueKey field for ${path.mkString("/")}""")
              }
            case structType =>
              val r = newStruct(property, structType)
              val values = r.get("Value").asInstanceOf[ArrayNode]
              import scala.jdk.CollectionConverters._
              for (fieldName <- value.fieldNames.asScala if fieldName != typeKey) {
                values.add(newData(path :+ fieldName, fieldName, value.get(fieldName)))
              }
              r
          }
        case _ => sbmod.exit(-1, 
          s"""Expecting a $typeKey field for ${path.mkString("/")} (whose value either "$nameKey", "$uint32Key", or the struct type name)""")
      }
    case null => null
  }
}

def newStruct(name: String, structType: String): ObjectNode = sbmod.toJsonNode(
  s"""{
     |  "$$type": "UAssetAPI.PropertyTypes.Structs.StructPropertyData, UAssetAPI",
     |  "StructType": "$structType",
     |  "SerializeNone": true,
     |  "StructGUID": "{00000000-0000-0000-0000-000000000000}",
     |  "SerializationControl": "NoExtension",
     |  "Operation": "None",
     |  "Name": "$name",
     |  "ArrayIndex": 0,
     |  "IsZero": false,
     |  "PropertyTagFlags": "None",
     |  "PropertyTagExtensions": "NoExtension",
     |  "Value": []
     |}""".stripMargin
).asInstanceOf[ObjectNode]

def newEnum(name: String, enumType: String, jsonValue: TextNode): ObjectNode = sbmod.toJsonNode(
  s"""{
     |  "$$type": "UAssetAPI.PropertyTypes.Objects.EnumPropertyData, UAssetAPI",
     |  "EnumType": "$enumType",
     |  "InnerType": "ByteProperty",
     |  "Name": "$name",
     |  "ArrayIndex": 0,
     |  "IsZero": false,
     |  "PropertyTagFlags": "None",
     |  "PropertyTagExtensions": "NoExtension",
     |  "Value": ${jsonValue.toPrettyString}
     |}""".stripMargin
).asInstanceOf[ObjectNode]

def newString(name: String, jsonValue: TextNode): ObjectNode = sbmod.toJsonNode(
  s"""{
     |  "$$type": "UAssetAPI.PropertyTypes.Objects.StrPropertyData, UAssetAPI",
     |  "Name": "$name",
     |  "ArrayIndex": 0,
     |  "IsZero": false,
     |  "PropertyTagFlags": "None",
     |  "PropertyTagExtensions": "NoExtension",
     |  "Value": ${if (jsonValue == null) "null" else jsonValue.toPrettyString}
     |}""".stripMargin
).asInstanceOf[ObjectNode]

def newBoolean(name: String, jsonValue: BooleanNode): ObjectNode = sbmod.toJsonNode(
  s"""{
     |  "$$type": "UAssetAPI.PropertyTypes.Objects.BoolPropertyData, UAssetAPI",
     |  "Name": "$name",
     |  "ArrayIndex": 0,
     |  "IsZero": false,
     |  "PropertyTagFlags": "None",
     |  "PropertyTagExtensions": "NoExtension",
     |  "Value": ${jsonValue.toPrettyString}
     |}""".stripMargin
).asInstanceOf[ObjectNode]

def newName(name: String, jsonValue: TextNode): ObjectNode = sbmod.toJsonNode(
  s"""{
     |  "$$type": "UAssetAPI.PropertyTypes.Objects.NamePropertyData, UAssetAPI",
     |  "Name": "$name",
     |  "ArrayIndex": 0,
     |  "IsZero": false,
     |  "PropertyTagFlags": "None",
     |  "PropertyTagExtensions": "NoExtension",
     |  "Value": ${if (jsonValue == null) "null" else jsonValue.toPrettyString}
     |}""".stripMargin
).asInstanceOf[ObjectNode]

def newUAssetApiFloatPropertyData(name: String, jsonValue: DoubleNode): ObjectNode = sbmod.toJsonNode(
  s"""{
     |  "$$type": "UAssetAPI.PropertyTypes.Objects.FloatPropertyData, UAssetAPI",
     |  "Value": "+0",
     |  "Name": "$name",
     |  "ArrayIndex": 0,
     |  "IsZero": false,
     |  "PropertyTagFlags": "None",
     |  "PropertyTagExtensions": "NoExtension",
     |  "Value": ${jsonValue.toPrettyString}
     |}""".stripMargin
).asInstanceOf[ObjectNode]

def newUAssetApiArrayPropertyData(name: String): ObjectNode = sbmod.toJsonNode(
  s"""{
     |  "$$type": "UAssetAPI.PropertyTypes.Objects.ArrayPropertyData, UAssetAPI",
     |  "ArrayType": null,
     |  "DummyStruct": null,
     |  "Name": "$name",
     |  "ArrayIndex": 0,
     |  "IsZero": false,
     |  "PropertyTagFlags": "None",
     |  "PropertyTagExtensions": "NoExtension",
     |  "Value": []
     |}""".stripMargin
).asInstanceOf[ObjectNode]

def newUAssetApiIntPropertyData(name: String, jsonValue: IntNode): ObjectNode = sbmod.toJsonNode(
  s"""{
      |  "$$type": "UAssetAPI.PropertyTypes.Objects.IntPropertyData, UAssetAPI",
      |  "Name": "$name",
      |  "ArrayIndex": 0,
      |  "IsZero": false,
      |  "PropertyTagFlags": "None",
      |  "PropertyTagExtensions": "NoExtension",
      |  "Value": ${jsonValue.toPrettyString}
      |}""".stripMargin
).asInstanceOf[ObjectNode]

def newUInt32(name: String, jsonValue: IntNode): ObjectNode = sbmod.toJsonNode(
  s"""{
     |  "$$type": "UAssetAPI.PropertyTypes.Objects.UInt32PropertyData, UAssetAPI",
     |  "Name": "$name",
     |  "ArrayIndex": 0,
     |  "IsZero": false,
     |  "PropertyTagFlags": "None",
     |  "PropertyTagExtensions": "NoExtension",
     |  "Value": ${jsonValue.toPrettyString}
     |}""".stripMargin
).asInstanceOf[ObjectNode]

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
    case _ => sbmod.exit(-1, s"Unsupported value: '${node.toPrettyString}'")
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
    case _ => sbmod.exit(-1, s"Unsupported value: '$v'")
  }
}

case class Struct(uassetName: String, value: JsonNode, addToDataTableFilePatches: Boolean) {
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
      case _ => sbmod.exit(-1, s"Could not find $objName in $uassetName")
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
    println(s"* $name/$property: ${sbmod.toJsonPrettyString(rOpt)} => ${sbmod.toJsonPrettyString(valueOpt)}")
    if (addToDataTableFilePatches) {
      val key = sbmod.OrderedString(uassetName, "", 0)
      var map = sbmod._patches.getOrElse(key, TreeMap.empty: sbmod.UAssetPropertyChanges)
      var m = map.getOrElse(name, TreeMap.empty: sbmod.PropertyChanges)
      m = m + (property -> sbmod.ValuePair(valueOpt, rOpt))
      map = map + (name -> m)
      sbmod._patches = sbmod._patches + (key -> map)
    }
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
