import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.fasterxml.jackson.databind.node.{ArrayNode, BooleanNode, DoubleNode, IntNode, JsonNodeFactory, NullNode, ObjectNode, TextNode}
import com.jayway.jsonpath
import java.util.{List => JList}
import org.luaj.vm2.{LuaValue, LuaFunction}
import org.graalvm.polyglot.{Context, HostAccess, Value}
import scala.collection.immutable.TreeMap
import scala.jdk.CollectionConverters._
import scala.collection.parallel.CollectionConverters._
import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox


object Constants {
  val codePrefixScala: String = "=>"
  val codePrefixTypescript: String = "=ts>"
  val codePrefixJavascript: String = "=js>"
  val codePrefixPython: String = "=py>"
  val codePrefixLua: String = "=lua>"
  val atPrefix = ".@:"
  val javaRegexPrefix = ".*:"
  val addValueKey = "value"
  lazy val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()

  val dataTableJsonPath = toJsonPath(automod.dataTablePath) // "$['Exports'][0]['Table']['Data']"
  def toJsonPath(jsonPtr: String): String = ("$" +: jsonPtr.split('/').drop(1).map(s => s.toIntOption match {
    case Some(n) => s"[$n]"
    case _ => s"['$s']"
  })).mkString

  val luaJson = {
    val parse = new LuaFunction {
      override def call(arg: LuaValue): LuaValue = uassetapi.toLuaValue(new ObjectMapper().readTree(arg.tojstring))
    }
    val stringify = new LuaFunction {
      override def call(arg: LuaValue): LuaValue = LuaValue.valueOf(uassetapi.fromLuaValue(arg).toString)
    }
    LuaValue.tableOf(Array(LuaValue.valueOf("parse"), parse, LuaValue.valueOf("stringify"), stringify))
  }
}

import Constants._

sealed trait Lang
object Lang {
  case object Scala extends Lang
  case object Typescript extends Lang
  case object Js extends Lang
  case object Python extends Lang
  case object Lua extends Lang
}

def graalContext(id: String): Context =
  Context.newBuilder(id).
    allowHostAccess(HostAccess.newBuilder(HostAccess.ALL).build).
    allowHostClassLookup(_ => true).
    build

def evalPoly(context: Context, lang: String, code: String): Value = try context.eval(lang, code) catch {
  case t: Throwable =>
    automod.exit(-1, 
      s"""Failed to evaluate $lang: ${t.getMessage}
         |$code""".stripMargin)
}
def evalScala[T](exp: String): T = tb.eval(tb.parse(exp)).asInstanceOf[T]
def evalJs(v: Context => Value, code: String): (Context, Value) = {
  val context = graalContext("js")
  context.getBindings("js").putMember("v", v(context))
  (context, evalPoly(context, "js", code))
}

lazy val tsCache = new java.util.concurrent.ConcurrentHashMap[String, String]

def evalTypescript(preamble: String)(v: Context => Value, exp: String): (Context, Value) = {
  val code = s"$preamble${util.Properties.lineSeparator}$exp"
  val js = Option(tsCache.get(code)) match {
    case Some(c) => c
    case _ =>
      val d = os.temp.dir()
      val input = d / "f.ts"
      val output = d / "f.js"
      os.write.over(input, code)
      if (automod.osKind.isWin) os.proc("cmd", "/d", "/c", "tsc", "--outFile", output, input).call()
      else os.proc("tsc", "--outFile", output, input).call()
      val c = os.read(output)
      tsCache.put(code, c)
      c
  }
  evalJs(v, js)
}

def evalPython(v: Context => Value, code: String): (Context, Value) = {
  val context = graalContext("python")
  context.getBindings("python").putMember("v", v(context))
  (context, evalPoly(context, "python", code.trim))
}

def evalLua(v: LuaValue, code: String, err: String => String): JsonNode = {
  try {
    val g = org.luaj.vm2.lib.jse.JsePlatform.standardGlobals
    org.luaj.vm2.luajc.LuaJC.install(g)
    g.set(LuaValue.valueOf("JSON"), luaJson)
    val chunk = g.load(
      s"""function __f(v)
         |  $code
         |end
         |
         |_f = __f""".stripMargin)
    chunk.call()
    assert(!chunk.isclosure)
    val f = g.get("_f").asInstanceOf[LuaValue]
    uassetapi.fromLuaValue(f.call(v))
  } catch {
    case t: Throwable => automod.exit(-1, err(t.getMessage))
  }
}

def evalObjectNamePredicate(lang: Lang, code: String): String => Boolean = {
  def graal(f: (Context => Value, String) => (Context, Value)): String => Boolean = { (v: String) => 
    val (context, r) = f((c: Context) => uassetapi.toPolyValue(c, TextNode.valueOf(v)), code)
    try uassetapi.toValue[Boolean](uassetapi.fromPolyValue(r)).get finally context.close
  }
  lang match {
    case Lang.Scala => evalScala[String => Boolean](s"{ (v: String) => def predicate(): Boolean = { $code }; predicate() }")
    case Lang.Typescript => graal(evalTypescript("declare var v: string"))
    case Lang.Js => graal(evalJs)
    case Lang.Python => graal(evalPython)
    case Lang.Lua => (s: String) => evalLua(LuaValue.valueOf(s), code, 
        msg => s"""Failed to evaluate Lua code with v as $s: $msg
                  |$code""".stripMargin) match {
      case node: BooleanNode => node.booleanValue
      case _ => 
        automod.exit(-1, 
          s"""The Lua code does not return a boolean value: $code""")
    }
  }
}

def checkPatches(uassetName: String, map: automod.UAssetPropertyChanges): automod.UAssetPropertyChanges = {
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
          case Some(`codePrefixScala`) =>
          case Some(`codePrefixTypescript`) =>
          case Some(`codePrefixJavascript`) =>
          case Some(`codePrefixPython`) =>
          case Some(`codePrefixLua`) =>
          case Some(prefix) => automod.exit(-1, s"Unrecognized value prefix for $uassetName/$objName/$property: $prefix")
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
  if (key.startsWith(codePrefixScala)) return Some(key.substring(0, codePrefixScala.length)) 
  if (key.startsWith(codePrefixTypescript)) return Some(key.substring(0, codePrefixTypescript.length)) 
  if (key.startsWith(codePrefixJavascript)) return Some(key.substring(0, codePrefixJavascript.length)) 
  if (key.startsWith(codePrefixPython)) return Some(key.substring(0, codePrefixPython.length)) 
  if (key.startsWith(codePrefixLua)) return Some(key.substring(0, codePrefixLua.length)) 
  return None
}

def isKeyPrefix(title: String, key: String): Boolean = getKeyPrefix(key) match {
  case Some(prefix) => prefix match {
    case `codePrefixScala` => true
    case `codePrefixTypescript` => true
    case `codePrefixJavascript` => true
    case `codePrefixPython` => true
    case `codePrefixLua` => true
    case `javaRegexPrefix` => true
    case `atPrefix` => true
    case _ => automod.exit(-1, s"Unrecognized prefix for $title: '$prefix'")
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

class PolyCodeContext(context: Context,
                      uassetName: String,
                      addToFilePatches: Boolean,
                      dataMap: collection.Map[String, ObjectNode],
                      @HostAccess.Export val objName: Value,
                      @HostAccess.Export val orig: Value,
                      @HostAccess.Export val current: Value,
                      @HostAccess.Export val ast: Value,
                      @HostAccess.Export val origAst: Value) {
  def valueOf(objName: Value, property: Value): Value = {
    dataMap.get(uassetapi.fromPolyValue(objName).asInstanceOf[String]) match {
      case Some(node) =>
        val obj = uassetapi.Struct(uassetName, node, addToFilePatches)
        uassetapi.toPolyValue(context, obj.getJson(uassetapi.fromPolyValue(property).asInstanceOf[String]))
      case _ => uassetapi.toPolyValue(context, NullNode.instance)
    }
  }
}

def evalStructProperty(lang: Lang, uassetName: String, addToFilePatches: Boolean, dataMap: collection.Map[String, ObjectNode], 
                       code: String, obj: uassetapi.Struct, property: String, orig: JsonNode, _ast: automod.JsonAst, 
                       _origAst: automod.JsonAst): JsonNode = {
  val currentValue = obj.getJson(property)
  val origValue = uassetapi.Struct(uassetName, orig, addToFilePatches = false).getJson(property)
  evalProperty(lang, uassetName, addToFilePatches, dataMap, code, obj.name, currentValue, origValue, property, _ast, _origAst)
}

def evalProperty(lang: Lang, uassetName: String, addToFilePatches: Boolean, dataMap: collection.Map[String, ObjectNode], 
                 code: String, name: String, currentValue: JsonNode, origValue: JsonNode, property: String, _ast: automod.JsonAst, 
                 _origAst: automod.JsonAst): JsonNode = {
  lang match {
    case Lang.Scala =>
      try {
        val nil = """"null""""
        val propertyF = evalScala[CodeContext => Any](
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
          def objName: String = name
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
          automod.exit(-1, 
            s"""Evaluation failed for $name/$property with the game original value of ${origValue} and 
               |the current value ${currentValue}: ${t.getMessage}
               |$code""".stripMargin)
      }
    case Lang.Lua =>
      val v = LuaValue.tableOf(Array[LuaValue](
        LuaValue.valueOf("objName"), LuaValue.valueOf(name),
        LuaValue.valueOf("orig"), uassetapi.toLuaValue(origValue),
        LuaValue.valueOf("current"), uassetapi.toLuaValue(currentValue),
        LuaValue.valueOf("ast"), uassetapi.toLuaValue(_ast.json[JsonNode]),
        LuaValue.valueOf("origAst"), uassetapi.toLuaValue(_origAst.json[JsonNode]),
        LuaValue.valueOf("valueOf"), new LuaFunction {
          override def call(arg1: LuaValue, arg2: LuaValue): LuaValue = {
            val objName = arg1.tojstring
            val property = arg2.tojstring
            dataMap.get(objName) match {
              case Some(node) =>
                val obj = uassetapi.Struct(uassetName, node, addToFilePatches)
                uassetapi.toLuaValue(obj.getJson(property))
              case _ => LuaValue.NIL
            }
          }
        }))
      evalLua(v, code, msg => 
            s"""Evaluation failed for $name/$property with the game original value of ${origValue} and 
               |the current value ${currentValue}: $msg
               |$code""".stripMargin)
    case _ =>
      val v = (c: Context) => c.asValue(
          new PolyCodeContext(c, uassetName, addToFilePatches, dataMap,
                              uassetapi.toPolyValue(c, TextNode.valueOf(name)), 
                              uassetapi.toPolyValue(c, origValue),
                              uassetapi.toPolyValue(c, currentValue),
                              uassetapi.toPolyValue(c, _ast.json[JsonNode]),
                              uassetapi.toPolyValue(c, _origAst.json[JsonNode])))     
      val (context, r) = (lang: @unchecked) match {
        case Lang.Typescript => evalTypescript(
          s"""type JsonNode = { [key: string]: any }
             |
             |interface PolyCodeContext {
             |  objName(): string
             |  orig(): any
             |  current(): any | undefined
             |  ast(): JsonNode
             |  origAst(): JsonNode
             |  valueOf(objName: string, property: string): any | undefined
             |}
             |
             |declare var v: PolyCodeContext""".stripMargin
        )(v, code)
        case Lang.Js => evalJs(v, code)
        case Lang.Python => evalPython(v, code)
      }                        
      return try uassetapi.fromPolyValue(r) finally context.close
  }
}

sealed trait FilteredChanges {
  def addToFilePatches: Boolean
  def uassetName: String
  def ast: automod.JsonAst
  def orig: automod.JsonAst
  def dataMap: collection.Map[String, ObjectNode]
  def changes: automod.PropertyChanges
  def applyAtChange(name: String, o: ObjectNode, property: String, v: JsonNode, vOrig: JsonNode): Unit = {
    var value = v
    value match {
      case v: TextNode => 
        def code(codePrefix: String, lang: Lang): Unit =
          value = evalProperty(lang, uassetName, addToFilePatches, dataMap, v.textValue.substring(codePrefix.length), 
                               name, o.get(property), vOrig, property, this.ast, this.orig)
        getKeyPrefix(v.textValue) match {
          case Some(`codePrefixScala`) => code(codePrefixScala, Lang.Scala)
          case Some(`codePrefixTypescript`) => code(codePrefixTypescript, Lang.Typescript)
          case Some(`codePrefixJavascript`) => code(codePrefixJavascript, Lang.Js)
          case Some(`codePrefixPython`) => code(codePrefixPython, Lang.Python)
          case Some(`codePrefixLua`) => code(codePrefixLua, Lang.Lua)
          case _ =>
        }
      case _ =>
    }
    uassetapi.objSetJson(addToFilePatches, uassetName, name, o, property, value)
  }
  def applyStructChanges(path: String, node: JsonNode, orig: JsonNode): Unit = {
    val obj = uassetapi.Struct(uassetName, node, addToFilePatches)
    for ((property, valueOldValuePair) <- changes) {
      var value = valueOldValuePair.newValueOpt.get
      value match {
        case v: TextNode => 
          def code(codePrefix: String, lang: Lang): Unit =
            value = evalStructProperty(lang, uassetName, addToFilePatches, dataMap, v.textValue.substring(codePrefix.length), 
                                       obj, property, orig, this.ast, this.orig)
          getKeyPrefix(v.textValue) match {
            case Some(`codePrefixScala`) => code(codePrefixScala, Lang.Scala)
            case Some(`codePrefixTypescript`) => code(codePrefixTypescript, Lang.Typescript)
            case Some(`codePrefixJavascript`) => code(codePrefixJavascript, Lang.Js)
            case Some(`codePrefixPython`) => code(codePrefixPython, Lang.Python)
            case Some(`codePrefixLua`) => code(codePrefixLua, Lang.Lua)
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
                             ast: automod.JsonAst,
                             orig: automod.JsonAst,
                             dataMap: collection.Map[String, ObjectNode],
                             path: String, 
                             changes: automod.PropertyChanges) extends FilteredChanges {
  def applyChanges(ast: automod.JsonAst): Unit = {
    val nodes: Seq[(JsonNode, JsonNode)] = if (path.isEmpty) {
      automod.exit(-1, s"The path for $uassetName name cannot be empty")
    } else if (path.head == '/') {
      val node = ast.json[JsonNode].at(path)
      if (node.isMissingNode) automod.exit(-1, s"Could not find $uassetName's path: $path")
      Seq((node, orig.json[JsonNode].at(path)))
    } else if (path.head == '$') {
      try {
        val array = ast.read[ArrayNode](path)
        val origArray = orig.read[ArrayNode](path)
        for (i <- 0 until array.size if array.get(i).isInstanceOf[ObjectNode]) yield (array.get(i), origArray.get(i))
      } catch {
        case t: Throwable => automod.exit(-1, 
          s"""Failed to search $uassetName path: $path
             |  reason: ${t.getMessage}""".stripMargin)
      }
    } else {
      automod.exit(-1, s"Unrecognized path for $uassetName: $path")
    }
    if (nodes.isEmpty) automod.exit(-1, s"Could not find objects for $uassetName: $path")
    for ((node, orig) <- nodes.par) {
      if (uassetapi.isStruct(node)) applyStructChanges(path, node, orig)
      else node match {
        case node: ObjectNode =>
          for ((property, valuePair) <- changes) Option(node.get(property)) match {
            case Some(old) => 
              applyAtChange(path, node, property, valuePair.newValueOpt.get, if (orig == null) null else orig.get(property))
            case _ => automod.exit(-1, s"$uassetName @$path does not have the property: $property")
          }
        case _ => automod.exit(-1, s"$uassetName @$path is neither a UAssetAPI's struct nor a JSON object node")
      }
    }
  }
}

case class KeyFilteredChanges(addToFilePatches: Boolean,
                              uassetName: String,
                              ast: automod.JsonAst,
                              orig: automod.JsonAst,
                              dataMap: collection.Map[String, ObjectNode],
                              f: String => Boolean, 
                              changes: automod.PropertyChanges) extends FilteredChanges {
  def apply(key: String): Boolean = f(key)
}

def kfcMap(maxOrder: Int, order: Int, addToFilePatches: Boolean, uassetName: String, ast: automod.JsonAst, origAst: automod.JsonAst,
           origAstPath: automod.JsonAst, t: automod.UAssetPropertyChanges): (collection.mutable.TreeMap[String, KeyFilteredChanges], collection.mutable.TreeMap[String, AtFilteredChanges], automod.UAssetPropertyChanges) = {
  var r1 = collection.mutable.TreeMap.empty[String, KeyFilteredChanges]
  var r2 = collection.mutable.TreeMap.empty[String, AtFilteredChanges]
  var rt = automod.emptyUAssetPropertyChanges 
  val dataMap: collection.Map[String, ObjectNode] = ast.json[JsonNode].at(automod.dataTablePath) match {
    case array: ArrayNode => automod.toDataMap(array)
    case _ => Map.empty
  } 
  for ((key, properties) <- t) {
    r1.get(key) match {
      case Some(_) => automod.exit(-1, s"Redefined key for $uassetName: $key")
      case _ =>
    }
    def code(codePrefix: String, lang: Lang, key: String): Unit = {
      val (code, props) = properties.get(codePrefix) match {
        case Some(text) => 
          text.newValueOpt match {
            case Some(codeText: TextNode) => (codeText.asText, properties.removed(codePrefix))
            case _ => automod.exit(-1, s"Invalid code for $uassetName: $text")
          }
        case _ => (key.substring(codePrefix.length), properties)
      }
      try {
        val fun = evalObjectNamePredicate(lang, code)  
        r1.put(key, KeyFilteredChanges(addToFilePatches, uassetName, ast, origAst, dataMap, fun, props))
      } catch {
        case _: Throwable => automod.exit(-1, s"Invalid code for $uassetName: $code")
      }
    }
    patchlet.getKeyPrefix(key) match {
      case Some(prefix) => prefix match {
        case `codePrefixScala` => code(codePrefixScala, Lang.Scala, key)
        case `codePrefixTypescript` => code(codePrefixTypescript, Lang.Typescript, key)
        case `codePrefixJavascript` => code(codePrefixJavascript, Lang.Js, key)
        case `codePrefixPython` => code(codePrefixPython, Lang.Python, key)
        case `codePrefixLua` => code(codePrefixLua, Lang.Lua, key)
        case `atPrefix` =>
          var path = key.substring(atPrefix.length).trim
          var i = 0
          while (i < path.length && path(i) != '/' && path(i) != '$') i += 1
          if (i >= path.length) automod.exit(-1, s"Invalid $atPrefix path: $path")
          path = path.substring(i)
          val isDataTable = if (path.startsWith(automod.dataTablePath + "/")) true else {
            val r = (if (path.head == '/') {
              val array = JsonNodeFactory.instance.arrayNode
              Option(origAstPath.json[JsonNode].at(path)).foreach(_ => array.add(toJsonPath(path)))
              array
            } else origAstPath.read(path)).asInstanceOf[ArrayNode]
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
            automod.updatePatches(uassetName, name, properties)
          }
        case `javaRegexPrefix` =>
          val regexText = key.substring(javaRegexPrefix.length).trim
          try {
            val regex = regexText.r
            val fun = (s: String) => regex.matches(s)
            r1.put(key, KeyFilteredChanges(addToFilePatches, uassetName, ast, origAst, dataMap, fun, properties))
          } catch {
            case _: Throwable => automod.exit(-1, s"Invalid Java regex for $uassetName: $regexText")
          }
        case _ => automod.exit(-1)
      }
      case _ => rt = rt + (key -> properties)
    }
  }
  (r1, r2, rt) 
}