import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.{JsonNodeFactory, ArrayNode, BooleanNode, DoubleNode, FloatNode, IntNode, LongNode, NullNode, ObjectNode, TextNode}
import org.graalvm.polyglot.{Context, Value}
import org.graalvm.polyglot.proxy.{Proxy, ProxyArray, ProxyObject}
import org.luaj.vm2.{LuaValue, LuaBoolean, LuaDouble, LuaInteger, LuaNil, LuaString, LuaTable}
import scala.collection.immutable.TreeMap
import scala.collection.mutable.HashMap
import scala.jdk.CollectionConverters._
import scala.reflect.runtime.universe._

// Reuse uassetapi's polyglot proxy wrappers (they are engine-agnostic).
import uassetapi.{PolyArray, PolyObject}

// Unity mirror of uassetapi.sc (plan §2.4 / M6). uassetapi.sc targets UAssetAPI's
// JSON shape (rows of {Name, Value} property wrappers); asset4j emits a flat Unity
// field tree per object ({PathId, ClassId, ..., Data:{field: value}}). The patch
// language (TOML/.patch/.@/expressions) is engine-agnostic; only the Struct/objSetJson
// addressing layer differs, and it is mirrored here.

object UnityConstants {
  val typeKey = "$type"
}

import UnityConstants._

def unityToPolyValue(context: Context, node: JsonNode): Value = {
  node match {
    case node: BooleanNode => context.asValue(node.booleanValue)
    case node: IntNode => context.asValue(node.intValue)
    case node: DoubleNode => context.asValue(node.doubleValue)
    case node: com.fasterxml.jackson.databind.node.FloatNode => context.asValue(node.doubleValue)
    case node: com.fasterxml.jackson.databind.node.LongNode => context.asValue(node.longValue)
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

def unityFromPolyValue(v: Value): JsonNode = {
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
    for (i <- 0L until v.getArraySize) r.add(unityFromPolyValue(v.getArrayElement(i)))
    r
  } else if (v.isInstanceOf[java.util.Map[_, _]]) {
    val r = JsonNodeFactory.instance.objectNode
    for ((name, value) <- v.asInstanceOf[java.util.Map[String, Value]].asScala) r.replace(name, unityFromPolyValue(value))
    r
  } else {
    automod.exit(-1, s"unityFromPolyValue: Unsupported value (${v.getClass}): '$v'")
  }
}

def unityFromLuaValue(v: LuaValue): JsonNode = {
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
      else r.add(unityFromLuaValue(n.arg(2)))
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
      else r.replace(unityFromLuaValue(k).textValue, unityFromLuaValue(n.arg(2)))
    }
    r
  } else automod.exit(-1, s"Unsupported value (${v.getClass}): '${v.tojstring}'")
}

def unityToLuaValue(node: JsonNode): LuaValue = {
  node match {
    case node: BooleanNode => LuaValue.valueOf(node.booleanValue)
    case node: IntNode => LuaValue.valueOf(node.intValue)
    case node: DoubleNode => LuaValue.valueOf(node.doubleValue)
    case node: ArrayNode => 
      var seq = Vector[LuaValue]()
      for (i <- 0 until node.size) {
        seq = seq :+ unityToLuaValue(node.get(i))
      }
      LuaValue.listOf(seq.toArray)
    case node: ObjectNode => 
      var seq = Vector[LuaValue]()
      for (property <- node.fieldNames.asScala) {
        seq = seq :+ LuaValue.valueOf(property)
        seq = seq :+ unityToLuaValue(node.get(property))
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

def unityToValue[T](node: JsonNode): Option[T] = {
  def toT(o: Any): T = o.asInstanceOf[T]

  node match {
    case node: BooleanNode => Some(toT(node.booleanValue))
    case node: IntNode => Some(toT(node.doubleValue))
    case node: DoubleNode => Some(toT(node.doubleValue))
    case node: com.fasterxml.jackson.databind.node.FloatNode => Some(toT(node.doubleValue))
    case node: com.fasterxml.jackson.databind.node.LongNode => Some(toT(node.doubleValue))
    case node: ArrayNode => 
      var builder = Vector.newBuilder[Any]
      for (i <- 0 until node.size) builder += unityToValue[Any](node.get(i)).get
      Some(toT(builder.result()))
    case node: ObjectNode => 
      var r = Map[String, Any]()
      for (property <- node.fieldNames.asScala) {
        r = r + (property -> unityToValue[Any](node.get(property)).getOrElse(null))
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

def unityFromValue(v: Any): JsonNode = {
  v match {
    case v: Boolean => BooleanNode.valueOf(v)
    case v: Int => IntNode.valueOf(v)
    case v: Double => DoubleNode.valueOf(v)
    case v: String => TextNode.valueOf(v)
    case v: Seq[_] => 
      val r = JsonNodeFactory.instance.arrayNode
      for (o <- v) {
        r.add(unityFromValue(o))
      }
      r
    case v: Map[_, _] =>
      val r = JsonNodeFactory.instance.objectNode
      for ((k, v) <- v) r.set[JsonNode](k.toString, unityFromValue(v))
      r
    case null => NullNode.instance
    case v: JsonNode => v
    case _ => automod.exit(-1, s"Unsupported value (${v.getClass}): '$v'")
  }
}

// Unity's field trees are flat: Data.{field: value} with no {Name, Value} wrapper.
// objSetJson replaces the field directly. Nested paths (PPtr sub-fields) use '.'
// separators.
def unityObjSetJson(addToFilePatches: Boolean, uassetName: String, name: String,
                    data: ObjectNode, parts: List[String], value: JsonNode): Option[JsonNode] = {
  assert(!value.isMissingNode)
  def walk(node: ObjectNode, remaining: List[String]): ObjectNode = remaining match {
    case head :: tail if tail.nonEmpty =>
      node.get(head) match {
        case child: ObjectNode => walk(child, tail)
        case _ => automod.exit(-1, s"Cannot replace non-existing nested property: $name/${parts.mkString(".")}")
      }
    case _ => node
  }
  val target = walk(data, parts)
  val property = parts.last
  if (target.get(property) == null) automod.exit(-1, s"Cannot replace a non-existing property: $name/${parts.mkString(".")}")
  val oldValueOpt = Option(target.replace(property, value))
  automod.logPatch(uassetName, s"* $name/${parts.mkString(".")}: ${automod.toJsonPrettyString(oldValueOpt)} => ${automod.toJsonPrettyString(Some(value))}", console = false)
  if (addToFilePatches) automod.updatePatch(uassetName, name, parts.mkString("."), automod.ValuePair(Some(value), oldValueOpt))
  oldValueOpt
}

// Unity Struct wraps a Unity object's Data node (the flat field tree). value may be
// either the full object node {PathId, ..., Data} or a raw Data object from a .@ path.
case class UnityStruct(uassetName: String, value: JsonNode, addToFilePatches: Boolean) extends patchlet.StructLike {
  private def data: ObjectNode = value match {
    case o: ObjectNode if o.has("Data") => o.get("Data").asInstanceOf[ObjectNode]
    case o: ObjectNode => o
    case _ => automod.exit(-1, s"Not a Unity object: ${value.toPrettyString}")
  }

  // Unity has no uniform row name; fall back to PathId when m_Name is absent/empty.
  def name: String = {
    val n = Option(data.get("m_Name")).filter(_.isTextual).map(_.asText).getOrElse("")
    if (n.nonEmpty) n else Option(value.get("PathId")).map(_.asText).getOrElse("")
  }

  def getJson(name: String): JsonNode = Option(data.get(name)).getOrElse(JsonNodeFactory.instance.missingNode)
  def getBoolean(name: String): Boolean = getJson(name).asBoolean
  def getInt(name: String): Int = getJson(name).asInt
  def getDouble(name: String): Double = getJson(name).asDouble
  def getString(name: String): String = getJson(name).asText

  def setJson(property: String, value: JsonNode): Option[JsonNode] = {
    val parts = property.split("\\.").toList
    unityObjSetJson(addToFilePatches, uassetName, name, data, parts, value)
  }
  def set(name: String, value: Boolean): Boolean = setJson(name, BooleanNode.valueOf(value)).map(_.asBoolean).getOrElse(false)
  def set(name: String, value: Int): Int = setJson(name, IntNode.valueOf(value)).map(_.asInt).getOrElse(0)
  def set(name: String, value: Double): Double = setJson(name, DoubleNode.valueOf(value)).map(_.asDouble).getOrElse(0d)
  def set(name: String, value: String): String = setJson(name, TextNode.valueOf(value)).map(_.asText).orNull
  def update(name: String, value: JsonNode): Option[JsonNode] = setJson(name, value)
  def apply[T: TypeTag](name: String): T = typeOf[T] match {
    case t if t =:= typeOf[Boolean] => getBoolean(name).asInstanceOf[T]
    case t if t =:= typeOf[Int] => getInt(name).asInstanceOf[T]
    case t if t =:= typeOf[Double] => getDouble(name).asInstanceOf[T]
    case t if t =:= typeOf[String] => getString(name).asInstanceOf[T]
  }
}
