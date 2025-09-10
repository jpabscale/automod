import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.{JsonNodeFactory, ArrayNode, BooleanNode, DoubleNode, IntNode, NullNode, ObjectNode, TextNode}
import org.graalvm.polyglot.{Context, Value}
import org.graalvm.polyglot.proxy.{Proxy, ProxyArray, ProxyObject}
import org.luaj.vm2.{LuaValue, LuaBoolean, LuaDouble, LuaInteger, LuaNil, LuaString, LuaTable}
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

object PolyArray {
  def apply(context: Context, node: ArrayNode): PolyArray = new PolyArray(context, node.deepCopy)
}

class PolyArray(context: Context, val node: ArrayNode) extends ProxyArray {
  override def get(index: Long): Object = toPolyValue(context, node.get(index.toInt))
  override def set(index: Long, value: Value): Unit = node.set(index.toInt, fromPolyValue(value))
  override def getSize(): Long = node.size()
}

object PolyObject {
  def apply(context: Context, node: ObjectNode): PolyObject = new PolyObject(context, node.deepCopy())
}

class PolyObject(context: Context, val node: ObjectNode) extends java.util.Map[String, Value] {
  override def size(): Int = node.size
  override def isEmpty(): Boolean = node.isEmpty
  override def containsKey(key: Object): Boolean = node.has(key.toString)
  override def containsValue(value: Object): Boolean = {
    for (v <- node.values.asScala if toPolyValue(context, v) == value) return true
    false
  }
  override def get(key: Object): Value = toPolyValue(context, node.get(key.toString))
  override def put(key: String, value: Value): Value = toPolyValue(context, node.replace(key, fromPolyValue(value)))
  override def remove(key: Object): Value = toPolyValue(context, node.remove(key.toString))
  override def putAll(m: java.util.Map[_ <: String, _ <: Value]): Unit = for (entry <- m.entrySet.asScala) put(entry.getKey, entry.getValue)
  override def clear(): Unit = node.removeAll
  override def keySet(): java.util.Set[String] = {
    val r = new java.util.TreeSet[String]
    for (f <- node.fieldNames.asScala) r.add(f)
    r
  }
  override def values(): java.util.Collection[Value] = {
    val r = new java.util.ArrayList[Value]
    for (f <- keySet.asScala) r.add(get(f))
    r
  }
  override def entrySet(): java.util.Set[java.util.Map.Entry[String, Value]] = {
    val r = new java.util.TreeSet[java.util.Map.Entry[String, Value]]
    for (f <- keySet.asScala) r.add(java.util.Map.entry(f, get(f)))
    r
  }
}

def toPolyValue(context: Context, node: JsonNode): Value = {
  node match {
    case node: BooleanNode => context.asValue(node.booleanValue)
    case node: IntNode => context.asValue(node.intValue)
    case node: DoubleNode => context.asValue(node.doubleValue)
    case node: ArrayNode => context.asValue(new PolyArray(context, node))
    case node: ObjectNode => context.asValue(new PolyObject(context, node))
    case node: TextNode => 
      val text = node.textValue
      text match {
        case "+0.0" | "-0.0" | "+0" | "-0" => context.asValue(0d)
        case _ => context.asValue(text)
      }
    case null | _: NullNode => context.asValue(null)
    case _ => automod.exit(-1, s"Unsupported value (${node.getClass}): '${node.toPrettyString}'")
  }
}

def fromPolyValue(v: Value): JsonNode = {
  if (v.isHostObject) v.asHostObject[Object] match {
    case proxy: PolyArray => return proxy.node
    case proxy: PolyObject => return proxy.node
    case _ =>
  }
  if (v.isProxyObject) v.asProxyObject[Proxy] match {
    case proxy: PolyArray => return proxy.node
    case _ =>
  }
  if (v.isNull) NullNode.instance
  else if (v.isBoolean) BooleanNode.valueOf(v.asBoolean)
  else if (v.isNumber) if (v.fitsInInt) IntNode.valueOf(v.asInt) else DoubleNode.valueOf(v.asDouble)
  else if (v.isString) TextNode.valueOf(v.asString)
  else if (v.hasArrayElements) {
    val r = JsonNodeFactory.instance.arrayNode
    for (i <- 0L until v.getArraySize) r.add(fromPolyValue(v.getArrayElement(i)))
    r
  } else if (v.isInstanceOf[java.util.Map[_, _]]) {
    val r = JsonNodeFactory.instance.objectNode
    for ((name, value) <- v.asInstanceOf[java.util.Map[String, Value]].asScala) r.replace(name, fromPolyValue(value))
    r
  } else {
    automod.exit(-1, s"fromPolyValue: Unsupported value (${v.getClass}): '$v'")
  }
}

def fromLuaValue(v: LuaValue): JsonNode = {
  if (v.isnil) NullNode.instance
  else if (v.isboolean) BooleanNode.valueOf(v.toboolean)
  else if (v.isint) IntNode.valueOf(v.toint)
  else if (v.isnumber) DoubleNode.valueOf(v.todouble)
  else if (v.isstring) TextNode.valueOf(v.tojstring)
  else if (v.istable && !v.get(LuaValue.valueOf(1)).isnil) {
    val r = JsonNodeFactory.instance.arrayNode
    var k = LuaValue.NIL;
    var stop = false
    while (!stop) {
      val n = v.next(k)
      k = n.arg1
      if (k.isnil) stop = true
      else r.add(fromLuaValue(n.arg(2)))
    }
    r
  } else if (v.istable) {
    val r = JsonNodeFactory.instance.objectNode
    var k = LuaValue.NIL;
    var stop = false
    while (!stop) {
      val n = v.next(k)
      k = n.arg1
      if (k.isnil) stop = true
      else r.replace(fromLuaValue(k).textValue, fromLuaValue(n.arg(2)))
    }
    r
  } else automod.exit(-1, s"Unsupported value (${v.getClass}): '${v.tojstring}'")
}

def toLuaValue(node: JsonNode): LuaValue = {
  node match {
    case node: BooleanNode => LuaValue.valueOf(node.booleanValue)
    case node: IntNode => LuaValue.valueOf(node.intValue)
    case node: DoubleNode => LuaValue.valueOf(node.doubleValue)
    case node: ArrayNode => 
      var seq = Vector[LuaValue]()
      for (i <- 0 until node.size) {
        seq = seq :+ toLuaValue(node.get(i))
      }
      LuaValue.listOf(seq.toArray)
    case node: ObjectNode => 
      var seq = Vector[LuaValue]()
      for (property <- node.fieldNames.asScala) {
        seq = seq :+ LuaValue.valueOf(property)
        seq = seq :+ toLuaValue(node.get(property))
      }
      LuaValue.tableOf(seq.toArray)
    case node: TextNode => 
      val text = node.textValue
      text match {
        case "+0.0" | "-0.0" | "+0" | "-0" => LuaValue.valueOf(0d)
        case _ => LuaValue.valueOf(text)
      }
    case null | _: NullNode => LuaValue.NIL
    case _ => automod.exit(-1, s"Unsupported value (${node.getClass}): '${node.toPrettyString}'")
  }
}

def toValue[T](node: JsonNode): Option[T] = {
  def toT(o: Any): T = o.asInstanceOf[T]

  node match {
    case node: BooleanNode => Some(toT(node.booleanValue))
    case node: IntNode => Some(toT(node.doubleValue))
    case node: DoubleNode => Some(toT(node.doubleValue))
    case node: ArrayNode => 
      var builder = Vector.newBuilder[Any]
      for (i <- 0 until node.size) builder += toValue[Any](node.get(i)).get
      Some(toT(builder.result()))
    case node: ObjectNode => 
      var r = Map[String, Any]()
      for (property <- node.fieldNames.asScala) {
        r = r + (property -> toValue[Any](node.get(property)).getOrElse(null))
      }
      Some(toT(r))
    case node: TextNode => 
      val text = node.textValue
      text match {
        case "+0.0" | "-0.0" | "+0" | "-0" => Some(toT(0d))
        case _ => Some(toT(text))
      }
    case null | _: NullNode => None
    case _ => automod.exit(-1, s"Unsupported value (${node.getClass}): '${node.toPrettyString}'")
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
    case v: JsonNode => v
    case _ => automod.exit(-1, s"Unsupported value (${v.getClass}): '$v'")
  }
}

def objSetJson(addToFilePatches: Boolean, uassetName: String, name: String, o: ObjectNode, property: String, value: JsonNode): Option[JsonNode] = {
  assert(!value.isMissingNode)
  def toEnumPrettyString(enumType: TextNode)(node: JsonNode): JsonNode = TextNode.valueOf(s"${enumType.asText}::${node.asText}")
  val (rOpt, valueOpt) = value match {
    case value: TextNode if value.asText.contains("::") => 
      var text = value.asText
      val enumType = TextNode.valueOf(text.substring(0, text.indexOf("::")))
      val oldEnumOpt = Option(o.replace("EnumType", enumType)).map(_.toPrettyString)
      val newValue = TextNode.valueOf(text.substring(text.indexOf("::") + 2))
      val oldValueOpt = Option(o.replace("Value", newValue))
      (oldValueOpt, Some(toEnumPrettyString(enumType)(newValue)))
    case _ =>
      val oldValueOpt = Option(o.replace("Value", value))  
      (oldValueOpt, Option(value))
  }
  automod.logPatch(uassetName, s"* $name/$property: ${automod.toJsonPrettyString(rOpt)} => ${automod.toJsonPrettyString(valueOpt)}", console = false)
  if (addToFilePatches) automod.updatePatch(uassetName, name, property, automod.ValuePair(valueOpt, rOpt))
  rOpt
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

  def setJson(property: String, value: JsonNode): Option[JsonNode] =
    objSetJson(addToFilePatches, uassetName, name, obj(property), property, value)
  
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

def isStruct(node: JsonNode): Boolean = node.get("Value").isInstanceOf[ArrayNode] && 
  Option(node.get("$type")).map(_.asText.contains("UAssetAPI.PropertyTypes.Structs")).getOrElse(false)
