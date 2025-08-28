import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.{ArrayNode, DoubleNode, IntNode, NullNode, ObjectNode, TextNode}
import com.jayway.jsonpath
import java.util.{List => JList}
import scala.collection.immutable.TreeMap
import scala.jdk.CollectionConverters._
import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox


object Constants {
  val codePrefix: String = "=>"
  val atPrefix = ".@:"
  val javaRegexPrefix = ".*:"
  val addValueKey = "value"
  lazy val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()

  val dataTableJsonPath = toJsonPath(sbmod.dataTablePath) // "$['Exports'][0]['Table']['Data']"
  def toJsonPath(jsonPtr: String): String = ("$" +: jsonPtr.split('/').drop(1).map(s => s.toIntOption match {
    case Some(n) => s"[$n]"
    case _ => s"['$s']"
  })).mkString
}

import Constants._

def eval[T](exp: String): T = tb.eval(tb.parse(exp)).asInstanceOf[T]

def checkPatches(uassetName: String, map: sbmod.UAssetPropertyChanges): sbmod.UAssetPropertyChanges = {
  for ((objName, properties) <- map) {
    def checkAddProperties: Boolean = {
      def checkObject(o: JsonNode): Boolean = if (o.isObject) {
        o.get(uassetapi.Constants.typeKey) match {
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
  def currentOpt[T]: Option[T]
  def valueOf[T](objName: String, property: String): Option[T]
  def ast: JsonNode
  def origAst: JsonNode
}


def evalProperty(uassetName: String, addToFilePatches: Boolean, dataMap: collection.Map[String, ObjectNode], 
                 code: String, obj: uassetapi.Struct, property: String, orig: JsonNode, _ast: sbmod.JsonAst, _origAst: sbmod.JsonAst): JsonNode = {
  val currentValue = obj.getJson(property)
  val origValue = uassetapi.Struct(uassetName, orig, addToFilePatches = false).getJson(property)
  try {
    val nil = """"null""""
    val propertyF = eval[CodeContext => Any](
    s"""{
        |import com.fasterxml.jackson.databind.JsonNode
        |import com.fasterxml.jackson.databind.node.{JsonNodeFactory, ArrayNode, DoubleNode, IntNode, NullNode, ObjectNode, TextNode}
        |
        |lazy val mapper = new com.fasterxml.jackson.databind.ObjectMapper
        |def toJsonNode(content: String): JsonNode = if (content == null) null else mapper.readTree(content)
        |def toJsonNodeT[T <: JsonNode](content: String): T = (if (content == null) null else mapper.readTree(content)).asInstanceOf[T]
        |def fromJsonNode(node: JsonNode): String = Option(node).map(_.toString).getOrElse($nil)
        |
        |(v: {
        |    def objName: String
        |    def orig[T]: T
        |    def currentOpt[T]: Option[T]
        |    def valueOf[T](objName: String, property: String): Option[T]
        |    def ast: JsonNode
        |    def origAst: JsonNode
        |  }) => 
        |  def calc(): Any = {
        |    $code
        |  }
        |  calc() 
        |}""".stripMargin)              
    val ctx = (new {
      def objName: String = obj.name
      def orig[T]: T = uassetapi.toValue[T](origValue).get
      def currentOpt[T]: Option[T] = uassetapi.toValue[T](currentValue)
      def valueOf[T](objName: String, property: String): Option[T] = {
        dataMap.get(objName) match {
          case Some(node) =>
            val obj = uassetapi.Struct(uassetName, node, addToFilePatches)
            uassetapi.toValue[T](obj.getJson(property))
          case _ => None
        }
      }
      def ast: JsonNode = _ast.json[JsonNode]
      def origAst: JsonNode = _origAst.json[JsonNode]
    }: CodeContext)
    return uassetapi.fromValue(propertyF(ctx))
  } catch {
    case t: Throwable =>
      sbmod.exit(-1, 
        s"""Evaluation failed for ${obj.name}/$property with the game original value of ${origValue} and 
           |the current value ${currentValue} using $code:
           |  ${t.getMessage}""".stripMargin)
  }
}
sealed trait FilteredChanges {
  def addToFilePatches: Boolean
  def uassetName: String
  def ast: sbmod.JsonAst
  def orig: sbmod.JsonAst
  def dataMap: collection.Map[String, ObjectNode]
  def changes: sbmod.PropertyChanges 
  def applyPathChanges(path: String, node: JsonNode, orig: JsonNode): Unit = {
    val obj = uassetapi.Struct(uassetName, node, addToFilePatches)
    for ((property, valueOldValuePair) <- changes) {
      var value = valueOldValuePair.newValueOpt.get
      value match {
        case v: TextNode => getKeyPrefix(v.textValue) match {
          case Some(`codePrefix`) =>
            val code = v.textValue.substring(codePrefix.length)
            value = evalProperty(uassetName, addToFilePatches, dataMap, code, obj, property, orig, this.ast, this.orig)
          case _ =>
        }
        case _ =>
      }
      obj.setJson(property, value)
    }
  }
}

case class AtFilteredChanges(addToFilePatches: Boolean,
                             uassetName: String,
                             ast: sbmod.JsonAst,
                             orig: sbmod.JsonAst,
                             dataMap: collection.Map[String, ObjectNode],
                             path: String, 
                             changes: sbmod.PropertyChanges) extends FilteredChanges {
  def applyChanges(ast: sbmod.JsonAst): Unit = {
    val nodes: Seq[(JsonNode, JsonNode)] = if (path.isEmpty) {
      sbmod.exit(-1, s"The path for $uassetName name cannot be empty")
    } else if (path.head == '/') {
      val node = ast.json[JsonNode].at(path)
      if (node.isMissingNode) sbmod.exit(-1, s"Could not find $uassetName's path: $path")
      Seq((node, orig.json[JsonNode].at(path)))
    } else if (path.head == '$') {
      def checkStruct(o: JsonNode): Boolean = o match {
        case o: ObjectNode => Option(o.get("Value")).map(_.isArray).getOrElse(false)
        case _ => false
      }
      try {
        val array = ast.read[ArrayNode](path)
        val origArray = orig.read[ArrayNode](path)
        for (i <- 0 until array.size if checkStruct(array.get(i))) yield (array.get(i), origArray.get(i))
      } catch {
        case t: Throwable => sbmod.exit(-1, 
          s"""Failed to search $uassetName path: $path
             |  reason: ${t.getMessage}""".stripMargin)
      }
    } else {
      sbmod.exit(-1, s"Unrecognized path for $uassetName: $path")
    }
    if (nodes.isEmpty) sbmod.exit(-1, s"Could not find objects for $uassetName: $path")
    for ((node, orig) <- nodes) {
      if (!node.get("Value").isInstanceOf[ArrayNode]) sbmod.exit(-1, s"$uassetName @$path is not a UAssetAPI's struct")
      applyPathChanges(path, node, orig)
    }
  }
}

case class KeyFilteredChanges(addToFilePatches: Boolean,
                              uassetName: String,
                              ast: sbmod.JsonAst,
                              orig: sbmod.JsonAst,
                              dataMap: collection.Map[String, ObjectNode],
                              f: String => Boolean, 
                              changes: sbmod.PropertyChanges) extends FilteredChanges {
  def apply(key: String): Boolean = f(key)
}

def kfcMap(maxOrder: Int, order: Int, addToFilePatches: Boolean, uassetName: String, ast: sbmod.JsonAst, origAst: sbmod.JsonAst,
           origAstPath: sbmod.JsonAst, t: sbmod.UAssetPropertyChanges): (collection.mutable.TreeMap[String, KeyFilteredChanges], collection.mutable.TreeMap[String, AtFilteredChanges], sbmod.UAssetPropertyChanges) = {
  var r1 = collection.mutable.TreeMap.empty[String, KeyFilteredChanges]
  var r2 = collection.mutable.TreeMap.empty[String, AtFilteredChanges]
  var rt: sbmod.UAssetPropertyChanges = collection.immutable.TreeMap.empty 
  val dataMap: collection.Map[String, ObjectNode] = ast.json[JsonNode].at(sbmod.dataTablePath) match {
    case array: ArrayNode => sbmod.toDataMap(array)
    case _ => Map.empty
  } 
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
            r1.put(key, KeyFilteredChanges(addToFilePatches, uassetName, ast, origAst, dataMap, fun, props))
          } catch {
            case _: Throwable => sbmod.exit(-1, s"Invalid code for $uassetName: $code")
          }
        case `atPrefix` =>
          var path = key.substring(atPrefix.length).trim
          var i = 0
          while (i < path.length && path(i) != '/' && path(i) != '$') i += 1
          if (i >= path.length) sbmod.exit(-1, s"Invalid $atPrefix path: $path")
          path = path.substring(i)
          val isDataTable = if (path.startsWith(sbmod.dataTablePath + "/")) true else {
            val r = origAstPath.read(path).asInstanceOf[ArrayNode]
            val prefix = dataTableJsonPath + "["
            def allWithPrefix: Boolean = {
              for (i <- 0 until r.size if !r.get(i).textValue.startsWith(prefix)) return false
              true
            }
            allWithPrefix
          }
          r2.put(key, AtFilteredChanges(addToFilePatches = isDataTable, uassetName, ast, origAst, dataMap, path, properties))
          if (addToFilePatches && !isDataTable) {
            val digits = (maxOrder + 1).toString.length
            val name = s"$atPrefix #${(for (i <- 0 until digits - order.toString.length) yield "0").mkString}$order $path"
            sbmod.updatePatches(uassetName, name, properties)
          }
        case `javaRegexPrefix` =>
          val regexText = key.substring(javaRegexPrefix.length).trim
          try {
            val regex = regexText.r
            val fun = (s: String) => regex.matches(s)
            r1.put(key, KeyFilteredChanges(addToFilePatches, uassetName, ast, origAst, dataMap, fun, properties))
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