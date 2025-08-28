import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.{ArrayNode, ObjectNode, TextNode}
import scala.reflect.runtime.universe.runtimeMirror
import scala.tools.reflect.ToolBox


object Constants {
  val codePrefix: String = "=>"
  val atPrefix = ".@:"
  val addPrefix = ".@+:"
  val javaRegexPrefix = ".*:"
  val addValueKey = "value"
}

import Constants._

def checkPatches(uassetName: String, map: sbmod.UAssetPropertyChanges): sbmod.UAssetPropertyChanges = {
  for ((objName, properties) <- map) {
    def checkAddProperties: Boolean = {
      def checkObject(o: JsonNode): Boolean = if (o.isObject) {
        o.get(UAssetApi.Constants.typeKey) match {
          case _: TextNode => true
          case _ => false
        }
      } else !o.isArray
      if (properties.size != 1) return false
      properties.get(addValueKey).flatMap(_.newValueOpt) match {
        case Some(node: ObjectNode) => checkObject(node)
        case Some(node: ArrayNode) =>
          for (i <- 0 until node.size) {
            if (!checkObject(node.get(i))) return false
          }
          true
        case _ => false
      }
    }
    isKeyPrefix(uassetName, objName)
    getKeyPrefix(objName) match {
      case Some(`addPrefix`) if !checkAddProperties => sbmod.exit(-1, s"Expecting a single inline table value with a '${UAssetApi.Constants.typeKey}' String property for $uassetName: $objName")
      case _ =>
    }
    for ((property, value) <- properties) {
      value.newValueOpt match {
        case Some(node: TextNode) => getKeyPrefix(node.textValue) match {
          case Some(`codePrefix`) =>
          case Some(prefix) => sbmod.exit(-1, s"Unrecognized value prefix for $uassetName/$objName/$property: $prefix")
          case _ =>
        }
        case _ =>
      }
    }
  }
  map
}

def eval[T](exp: String): T = {
  val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()
  tb.eval(tb.parse(exp)).asInstanceOf[T]
}

def getKeyPrefix(key: String): Option[String] = {
  if (key.isEmpty) return None
  val i = key.indexOf(':')
  if (key.head == '.' && i > 0) return Some(key.substring(0, i + 1))
  if (key.startsWith("=>")) return Some(key.substring(0, 2)) 
  return None
}

def isKeyPrefix(title: String, key: String): Boolean = getKeyPrefix(key) match {
  case Some(prefix) => prefix match {
    case `codePrefix` => true
    case `javaRegexPrefix` => true
    case `atPrefix` => true
    case _ => sbmod.exit(-1, s"Unrecognized prefix for $title: '$prefix'")
  }
  case _ => false
}

type CodeContext = {
  def objName: String
  def orig[T]: T
  def current[T]: Option[T]
  def valueOf[T](objName: String, property: String): Option[T]
}

sealed trait FilteredChanges {
  def uassetName: String
  def orig: () => JsonNode
  def dataMap: () => collection.mutable.Map[String, ObjectNode]
  def changes: sbmod.PropertyChanges 
  def codeContext(_path: String, _objName: String, _current: JsonNode, _orig: JsonNode): CodeContext = (new {
    def objName: String = _objName
    def orig[T]: T = UAssetApi.toValue[T](_orig).get
    def current[T]: Option[T] = UAssetApi.toValue[T](_current)
    def valueOf[T](objName: String, property: String): Option[T] = {
      dataMap().get(objName) match {
        case Some(node) =>
          val obj = UAssetApi.Struct(uassetName, node, addToDataTableFilePatches = false)
          UAssetApi.toValue[T](obj.getJson(property))
        case _ => None
      }
    }
  }: CodeContext)
  def applyChanges(path: String, node: JsonNode): Unit = {
    val obj = UAssetApi.Struct(uassetName, node, addToDataTableFilePatches = false)
    for ((property, valueOldValuePair) <- changes) {
      var value = valueOldValuePair.newValueOpt.get
      value match {
        case v: TextNode => getKeyPrefix(v.textValue) match {
          case Some(`codePrefix`) =>
            val code = v.textValue.substring(codePrefix.length)
            val propertyF = eval[CodeContext => Any](
              s"""{(v: {
                  |    def objName: String
                  |    def orig[T]: T
                  |    def currentOpt[T]: Option[T]
                  |    def valueOf[T](objName: String, property: String): Option[T]
                  |  }) => 
                  |  def calc() = {
                  |    $code
                  |  }
                  |  calc() 
                  |}""".stripMargin)
            val currentValue = obj.getJson(property)
            val origValue = UAssetApi.Struct(uassetName, orig().at(path), addToDataTableFilePatches = false).getJson(property)
            val ctx = codeContext(path, obj.name, currentValue, origValue)
            try {
              value = UAssetApi.fromValue(propertyF(ctx))
            } catch {
              case _: Throwable => sbmod.exit(-1, s"Evaluation failed for ${obj.name}/$property with value ${Option(origValue).map(_.toPrettyString).getOrElse("")} using $code")
            }
          case _ =>
        }
        case _ =>
      }
      obj.setJson(property, value)
    }
  }
}

case class AtFilteredChanges(uassetName: String,
                             isAdd: Boolean,
                             orig: () => JsonNode,
                             dataMap: () => collection.mutable.Map[String, ObjectNode],
                             path: String, 
                             changes: sbmod.PropertyChanges) extends FilteredChanges {
  def applyChanges(ast: JsonNode): Unit = {
    val node = ast.at(path)
    if (node.isMissingNode) sbmod.exit(-1, s"Could not find $uassetName's path: $path")
    if (isAdd) {
      node match {
        case node: ArrayNode => 
          var first = true
          def add(o: ObjectNode): Unit = {
            if (first) {
              first = false
              println(s"* @$path")
            }
            val lines = o.toPrettyString.linesIterator.map(line => s"    $line").toSeq
            println(s"  * Added: ${lines.head.trim}")
            lines.drop(1).foreach(println)
            node.add(o)
          }
          val value = changes.get(addValueKey).get.newValueOpt.get
          value match {
            case value: ArrayNode =>
              val offset = node.size
              val p = path.split('/').toVector
              for (i <- 0 until value.size) {
                add(UAssetApi.newData(p, (offset + i).toString, value.get(i)))
              }
            case _ => add(UAssetApi.newData(path.split('/').toVector, node.size.toString, value))
          }          
        case _ => sbmod.exit(1, s"Expecting an array for $uassetName at: $path")
      }
    } else {
      if (!node.get("Value").isInstanceOf[ArrayNode]) sbmod.exit(-1, s"$uassetName @$path is not a UAssetAPI's struct")
      applyChanges(path, node)
    }
  }
}

case class KeyFilteredChanges(uassetName: String,
                              orig: () => JsonNode,
                              dataMap: () => collection.mutable.Map[String, ObjectNode],
                              f: String => Boolean, 
                              changes: sbmod.PropertyChanges) extends FilteredChanges {
  def apply(key: String): Boolean = f(key)
}

def kfcMap(uassetName: String, ast: JsonNode, data: ArrayNode, t: sbmod.UAssetPropertyChanges): (collection.mutable.TreeMap[String, KeyFilteredChanges], collection.mutable.TreeMap[String, AtFilteredChanges], sbmod.UAssetPropertyChanges) = {
  var r1 = collection.mutable.TreeMap.empty[String, KeyFilteredChanges]
  var r2 = collection.mutable.TreeMap.empty[String, AtFilteredChanges]
  var rt: sbmod.UAssetPropertyChanges = collection.immutable.TreeMap.empty 
  lazy val _dataMap: collection.mutable.Map[String, ObjectNode] = {
    val r = collection.mutable.HashMap.empty[String, ObjectNode]
    for (i <- 0 until data.size) {
      val o = data.get(i).asInstanceOf[ObjectNode]
      r.put(o.get("Name").asText, o)
    }
    r
  }
  lazy val _orig = ast.deepCopy[JsonNode]
  val dataMap = () => _dataMap
  val orig = () => _orig
  for ((key, properties) <- t) {
    r1.get(key) match {
      case Some(_) => sbmod.exit(-1, s"Redefined key for $uassetName: $key")
      case _ =>
    }
    patchlet.getKeyPrefix(key) match {
      case Some(prefix) => prefix match {
        case `codePrefix` =>
          val (code, props) = properties.get(codePrefix) match {
            case Some(text) => 
              text.newValueOpt match {
                case Some(codeText: TextNode) => (codeText.asText, properties.removed(codePrefix))
                case _ => sbmod.exit(-1, s"Invalid code for $uassetName: $text")
              }
            case _ => (key.substring(codePrefix.length), properties)
          }
          try {
            val fun = eval[String => Boolean](s"{ (v: String) => def predicate(): Boolean = { $code }; predicate() }".stripMargin)  
            r1.put(key, KeyFilteredChanges(uassetName, orig, dataMap, fun, props))
          } catch {
            case _: Throwable => sbmod.exit(-1, s"Invalid code for $uassetName: $code")
          }
        case `atPrefix` =>
          val path = key.substring(atPrefix.length).trim
          r2.put(key, AtFilteredChanges(uassetName, isAdd = false, orig, dataMap, path, properties))
        case `addPrefix` =>
          val path = key.substring(addPrefix.length).trim
          r2.put(key, AtFilteredChanges(uassetName, isAdd = true, orig, dataMap, path, properties))
        case `javaRegexPrefix` =>
          val regexText = key.substring(javaRegexPrefix.length).trim
          try {
            val regex = regexText.r
            val fun = (s: String) => regex.matches(s)
            r1.put(key, KeyFilteredChanges(uassetName, orig, dataMap, fun, properties))
          } catch {
            case _: Throwable => sbmod.exit(-1, s"Invalid Java regex for $uassetName: $regexText")
          }
        case _ => sbmod.exit(-1)
      }
      case _ => rt = rt + (key -> properties)
    }
  }
  (r1, r2, rt) 
}