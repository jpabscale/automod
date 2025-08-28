import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.{ArrayNode, BooleanNode, DoubleNode, IntNode, ObjectNode, TextNode}
import scala.collection.immutable.TreeMap
import scala.collection.mutable.HashMap
import scala.reflect.runtime.universe._
import sbmod._

val typeKey = ".type" 
val valueKey = ".value"
val nameKey = ".name"
val uint32Key = ".uint32"

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
                case _ => exit(-1, s"""Expecting a string $valueKey field for ${path.mkString("/")}""")
              }
            case `uint32Key` =>
              Option(value.get(valueKey)) match {
                case Some(v: IntNode) => newUInt32(property, v)
                case _ => exit(-1, s"""Expecting a string $valueKey field for ${path.mkString("/")}""")
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
        case _ => exit(-1, 
          s"""Expecting a $typeKey field for ${path.mkString("/")} (whose value either "$nameKey", "$uint32Key", or the struct type name)""")
      }
    case null => null
  }
}

def newStruct(name: String, structType: String): ObjectNode = toJsonNode(
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

def newEnum(name: String, enumType: String, jsonValue: TextNode): ObjectNode = toJsonNode(
  s"""{
     |  "$$type": "UAssetAPI.PropertyTypes.Objects.EnumPropertyData, UAssetAPI",
     |  "EnumType": "$enumType",
     |  "InnerType": "ByteProperty",
     |  "Name": "$name",
     |  "ArrayIndex": 0,
     |  "IsZero": true,
     |  "PropertyTagFlags": "None",
     |  "PropertyTagExtensions": "NoExtension",
     |  "Value": ${jsonValue.toPrettyString}
     |}""".stripMargin
).asInstanceOf[ObjectNode]

def newString(name: String, jsonValue: TextNode): ObjectNode = toJsonNode(
  s"""{
     |  "$$type": "UAssetAPI.PropertyTypes.Objects.StrPropertyData, UAssetAPI",
     |  "Name": "$name",
     |  "ArrayIndex": 0,
     |  "IsZero": true,
     |  "PropertyTagFlags": "None",
     |  "PropertyTagExtensions": "NoExtension",
     |  "Value": ${if (jsonValue == null) "null" else jsonValue.toPrettyString}
     |}""".stripMargin
).asInstanceOf[ObjectNode]

def newBoolean(name: String, jsonValue: BooleanNode): ObjectNode = toJsonNode(
  s"""{
     |  "$$type": "UAssetAPI.PropertyTypes.Objects.BoolPropertyData, UAssetAPI",
     |  "Name": "$name",
     |  "ArrayIndex": 0,
     |  "IsZero": true,
     |  "PropertyTagFlags": "None",
     |  "PropertyTagExtensions": "NoExtension",
     |  "Value": ${jsonValue.toPrettyString}
     |}""".stripMargin
).asInstanceOf[ObjectNode]

def newName(name: String, jsonValue: TextNode): ObjectNode = toJsonNode(
  s"""{
     |  "$$type": "UAssetAPI.PropertyTypes.Objects.NamePropertyData, UAssetAPI",
     |  "Name": "$name",
     |  "ArrayIndex": 0,
     |  "IsZero": true,
     |  "PropertyTagFlags": "None",
     |  "PropertyTagExtensions": "NoExtension",
     |  "Value": ${if (jsonValue == null) "null" else jsonValue.toPrettyString}
     |}""".stripMargin
).asInstanceOf[ObjectNode]

def newUAssetApiFloatPropertyData(name: String, jsonValue: DoubleNode): ObjectNode = toJsonNode(
  s"""{
     |  "$$type": "UAssetAPI.PropertyTypes.Objects.FloatPropertyData, UAssetAPI",
     |  "Value": "+0",
     |  "Name": "$name",
     |  "ArrayIndex": 0,
     |  "IsZero": true,
     |  "PropertyTagFlags": "None",
     |  "PropertyTagExtensions": "NoExtension",
     |  "Value": ${jsonValue.toPrettyString}
     |}""".stripMargin
).asInstanceOf[ObjectNode]

def newUAssetApiArrayPropertyData(name: String): ObjectNode = toJsonNode(
  s"""{
     |  "$$type": "UAssetAPI.PropertyTypes.Objects.ArrayPropertyData, UAssetAPI",
     |  "ArrayType": null,
     |  "DummyStruct": null,
     |  "Name": "$name",
     |  "ArrayIndex": 0,
     |  "IsZero": true,
     |  "PropertyTagFlags": "None",
     |  "PropertyTagExtensions": "NoExtension",
     |  "Value": []
     |}""".stripMargin
).asInstanceOf[ObjectNode]

def newUAssetApiIntPropertyData(name: String, jsonValue: IntNode): ObjectNode = toJsonNode(
  s"""{
      |  "$$type": "UAssetAPI.PropertyTypes.Objects.IntPropertyData, UAssetAPI",
      |  "Name": "$name",
      |  "ArrayIndex": 0,
      |  "IsZero": true,
      |  "PropertyTagFlags": "None",
      |  "PropertyTagExtensions": "NoExtension",
      |  "Value": ${jsonValue.toPrettyString}
      |}""".stripMargin
).asInstanceOf[ObjectNode]

def newUInt32(name: String, jsonValue: IntNode): ObjectNode = toJsonNode(
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


case class DataObject(uassetName: String, value: JsonNode, addToDataTableFilePatches: Boolean) {
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
      case _ => exit(-1, s"Could not find $objName in $uassetName")
    }
  }

  def setJson(property: String, value: JsonNode): Option[JsonNode] = {
    if (value != null && ((value.isArray && value.asInstanceOf[ArrayNode].size != 0) || 
      value.isObject)) exit(-1, s"Unsupported patch value for $name/$property: ${value.toPrettyString}")
    def toEnumPrettyString(enumType: TextNode)(node: JsonNode): JsonNode = TextNode.valueOf(s"${enumType.asText}::${node.asText}")
    val (rOpt, valueOpt) = value match {
      case value: TextNode if value.asText.contains("::") => 
        var text = value.asText
        val enumType = TextNode.valueOf(text.substring(0, text.indexOf("::")))
        val oldEnumOpt = Option(obj(property).replace("EnumType", enumType)).map(_.toPrettyString)
        val newValue = TextNode.valueOf(text.substring(text.indexOf("::") + 2))
        val oldValueOpt = Option(obj(property).replace("Value", value))
        (oldValueOpt, Some(toEnumPrettyString(enumType)(newValue)))
      case _ =>
        val oldValueOpt = Option(obj(property).replace("Value", value))  
        (oldValueOpt, Option(value))
    }
    println(s"* $name/$property: ${toJsonPrettyString(rOpt)} => ${toJsonPrettyString(valueOpt)}")
    if (addToDataTableFilePatches) {
      var map = _patches.getOrElse(uassetName, TreeMap.empty: UAssetPropertyChanges)
      var m = map.getOrElse(name, TreeMap.empty: PropertyChanges)
      m = m + (property -> ValuePair(valueOpt, rOpt))
      map = map + (name -> m)
      _patches = _patches + (uassetName -> map)
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
