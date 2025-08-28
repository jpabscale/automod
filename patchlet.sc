import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.{ArrayNode, NullNode, ObjectNode, TextNode}
import com.jayway.jsonpath
import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox


object Constants {
  val codePrefix: String = "=>"
  val atPrefix = ".@:"
  val addPrefix = ".@+:"
  val javaRegexPrefix = ".*:"
  val addValueKey = "value"
  lazy val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()
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
    getKeyPrefix(objName) match {
      case Some(`addPrefix`) if !checkAddProperties => sbmod.exit(-1, s"Expecting a single inline table value with a '${uassetapi.Constants.typeKey}' String property for $uassetName: $objName")
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
  def addToDataTableFilePatches: Boolean
  def uassetName: String
  def orig: sbmod.JsonAst
  def dataMap: () => collection.mutable.Map[String, ObjectNode]
  def changes: sbmod.PropertyChanges 
  def codeContext(_path: String, _objName: String, _current: JsonNode, _orig: JsonNode): CodeContext = (new {
    def objName: String = _objName
    def orig[T]: T = uassetapi.toValue[T](_orig).get
    def current[T]: Option[T] = uassetapi.toValue[T](_current)
    def valueOf[T](objName: String, property: String): Option[T] = {
      dataMap().get(objName) match {
        case Some(node) =>
          val obj = uassetapi.Struct(uassetName, node, addToDataTableFilePatches)
          uassetapi.toValue[T](obj.getJson(property))
        case _ => None
      }
    }
  }: CodeContext)
  def applyPathChanges(path: String, node: JsonNode, orig: JsonNode): Unit = {
    val obj = uassetapi.Struct(uassetName, node, addToDataTableFilePatches)
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
            val origValue = uassetapi.Struct(uassetName, orig, addToDataTableFilePatches = false).getJson(property)
            val ctx = codeContext(path, obj.name, currentValue, origValue)
            try {
              value = uassetapi.fromValue(propertyF(ctx))
            } catch {
              case t: Throwable =>
                t.printStackTrace 
                sbmod.exit(-1, 
              s"""Evaluation failed for ${obj.name}/$property with value ${Option(origValue).map(_.toPrettyString).getOrElse("")} using $code:
                 |  ${t.getMessage}""".stripMargin)
            }
          case _ =>
        }
        case _ =>
      }
      obj.setJson(property, value)
    }
  }
}

case class AtFilteredChanges(addToDataTableFilePatches: Boolean,
                             uassetName: String,
                             isAdd: Boolean,
                             orig: sbmod.JsonAst,
                             dataMap: () => collection.mutable.Map[String, ObjectNode],
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
    if (isAdd) {
      if (nodes.size != 1) sbmod.exit(-1, s"The path evaluates to multiple objects: $path")
      val (node, _) = nodes.head
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
                add(uassetapi.newData(p, (offset + i).toString, value.get(i)))
              }
            case _ => add(uassetapi.newData(path.split('/').toVector, node.size.toString, value))
          }          
        case _ => sbmod.exit(1, s"Expecting an array for $uassetName at: $path")
      }
    } else {
      for ((node, orig) <- nodes) {
        if (!node.get("Value").isInstanceOf[ArrayNode]) sbmod.exit(-1, s"$uassetName @$path is not a UAssetAPI's struct")
        applyPathChanges(path, node, orig)
      }
    }
  }
}

case class KeyFilteredChanges(addToDataTableFilePatches: Boolean,
                              uassetName: String,
                              orig: sbmod.JsonAst,
                              dataMap: () => collection.mutable.Map[String, ObjectNode],
                              f: String => Boolean, 
                              changes: sbmod.PropertyChanges) extends FilteredChanges {
  def apply(key: String): Boolean = f(key)
}

def kfcMap(addToDataTableFilePatches: Boolean, uassetName: String, ast: sbmod.JsonAst, origAst: sbmod.JsonAst, 
           data: ArrayNode, t: sbmod.UAssetPropertyChanges): (collection.mutable.TreeMap[String, KeyFilteredChanges], collection.mutable.TreeMap[String, AtFilteredChanges], sbmod.UAssetPropertyChanges) = {
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
  lazy val _orig = ast.json[JsonNode].deepCopy[JsonNode]
  val dataMap = () => _dataMap
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
            r1.put(key, KeyFilteredChanges(addToDataTableFilePatches, uassetName, origAst, dataMap, fun, props))
          } catch {
            case _: Throwable => sbmod.exit(-1, s"Invalid code for $uassetName: $code")
          }
        case `atPrefix` =>
          val path = key.substring(atPrefix.length).trim
          r2.put(key, AtFilteredChanges(addToDataTableFilePatches, uassetName, isAdd = false, origAst, dataMap, path, properties))
        case `addPrefix` =>
          val path = key.substring(addPrefix.length).trim
          r2.put(key, AtFilteredChanges(addToDataTableFilePatches, uassetName, isAdd = true, origAst, dataMap, path, properties))
        case `javaRegexPrefix` =>
          val regexText = key.substring(javaRegexPrefix.length).trim
          try {
            val regex = regexText.r
            val fun = (s: String) => regex.matches(s)
            r1.put(key, KeyFilteredChanges(addToDataTableFilePatches, uassetName, origAst, dataMap, fun, properties))
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