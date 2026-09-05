import java.io.PrintStream
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import os._

// Kotlin patchlet evaluator (plan §M6): compiles the patch body as a Kotlin source file at
// runtime with the embeddable K2JVMCompiler, loads it in a fresh URLClassLoader, and invokes
// `eval(v)` — where `v` is the RawScriptContext map (or an inline-property context). Unlike
// Scala's ToolBox (`tb.parse`), Kotlin surfaces real line/column compile diagnostics.
//
// The patch body is wrapped as `fun eval(v: MutableMap<String, Any>): Any = run { BODY }`
// so the last expression of BODY is the return value. Kotlin's own syntax rules apply (no
// Scala ToolBox quirks: `val` in loops, property access, etc.).
object KotlinApi {

  /**
   * Kotlin-friendly `valueOf(objName, property)` — mirrors the Scala CodeContext.valueOf used
   * by inline `=sc>` property patches: returns the JSON value of [property] on the row named
   * [objName] in the patch data table, or null when the row is absent.
   */
  class ValueOfFn(dataMap: collection.Map[String, com.fasterxml.jackson.databind.node.ObjectNode],
                  uassetName: String,
                  addToFilePatches: Boolean) {
    def valueOf(objName: String, property: String): com.fasterxml.jackson.databind.JsonNode = {
      dataMap.get(objName) match {
        case Some(node) =>
          val obj = uassetapi.Struct(uassetName, node, addToFilePatches)
          obj.getJson(property)
        case _ => com.fasterxml.jackson.databind.node.NullNode.instance
      }
    }
  }

  /** Kotlin function form of [ValueOfFn] for `=kt>` property bodies (ValV.valueOf). */
  def valueOfFn(dataMap: collection.Map[String, com.fasterxml.jackson.databind.node.ObjectNode],
                uassetName: String,
                addToFilePatches: Boolean): kotlin.jvm.functions.Function2[String, String, com.fasterxml.jackson.databind.JsonNode] =
    new kotlin.jvm.functions.Function2[String, String, com.fasterxml.jackson.databind.JsonNode] {
      override def invoke(objName: String, property: String): com.fasterxml.jackson.databind.JsonNode =
        new ValueOfFn(dataMap, uassetName, addToFilePatches).valueOf(objName, property)
    }

  private lazy val kotlinCompilerClasspath: String = {
    val cp = System.getProperty("java.class.path")
    // scala-cli gives us the full runtime classpath; the compiler needs kotlin-stdlib too
    // (it's on the classpath already), so we just pass it through.
    cp
  }

  private def compilerJar(): java.nio.file.Path = {
    // locate kotlin-compiler-embeddable in the classpath to pin the exact version dir
    val cp = System.getProperty("java.class.path")
    val entry = cp.split(java.io.File.pathSeparator).iterator.find(p =>
      p.contains("kotlin-compiler-embeddable") && p.endsWith(".jar"))
    entry.map(p => java.nio.file.Path.of(p)).getOrElse(throw new IllegalStateException(
      "kotlin-compiler-embeddable not found on classpath"))
  }

  private lazy val compilerJarPath = compilerJar()

  /**
   * Compiles [body] (a patch body whose last expression is the result) and invokes it with
   * [v]. Returns whatever the body's last expression evaluates to.
   */
  def evalKotlin(v: java.util.Map[String, Any], body: String): Any = {
    val tmp = os.temp.dir()
    val src = tmp / "KtPatch.kt"
    val source =
      s"""package ktp
         |
         |import java.io.File
         |import java.nio.file.Files
         |import java.nio.file.Path
         |import com.fasterxml.jackson.databind.JsonNode
         |import com.fasterxml.jackson.databind.ObjectMapper
         |import com.fasterxml.jackson.databind.node.*
         |import com.github.jpabscale.asset4j.api.AssetService
         |
         |class PatchV(val map: MutableMap<String, Any>) {
         |  val orig: ByteArray get() = map["orig"] as ByteArray
         |  val current: ByteArray get() = map["current"] as ByteArray
         |  val className: String? get() = map["className"]?.toString()
         |  val objects: List<MutableMap<String, Any>> get() = map["objects"] as List<MutableMap<String, Any>>
         |  operator fun get(key: String): Any? = map[key]
         |  fun ttmapOrNull(): String? {
         |    val s = map["ttmap"]?.toString()
         |    return if (s != null && s.isNotEmpty() && Files.isRegularFile(Path.of(s))) s else null
         |  }
         |  fun toJson(): ObjectNode {
         |    val op = map["originalPath"]?.toString()
         |    val opPath = if (op != null && op.isNotEmpty()) Path.of(op)
         |                 else throw RuntimeException("v.toJson() requires a Unity bundle context")
         |    return AssetService.toJsonNodeBytes(current, opPath, ttmapOrNull(), opPath)
         |  }
         |  fun fromJson(node: JsonNode): ByteArray = AssetService.fromJsonNode(node, ttmapOrNull())
         |  fun resource(name: String): ByteArray? {
         |    val dir = map["patchDir"]?.toString() ?: return null
         |    val p = Path.of(dir).resolve(name)
         |    return if (Files.isRegularFile(p)) Files.readAllBytes(p) else null
         |  }
         |}
         |
         |fun eval(v: PatchV): Any = run {
         |$body
         |}
         |""".stripMargin
    Files.write(src.toNIO, source.getBytes(StandardCharsets.UTF_8))
    val outDir = tmp / "out"
    Files.createDirectories(outDir.toNIO)
    val buf = new java.io.ByteArrayOutputStream()
    val out = new PrintStream(buf)
    val args = Array(
      "-classpath", kotlinCompilerClasspath,
      "-no-stdlib", "-no-reflect",
      "-d", outDir.toNIO.toAbsolutePath.toString,
      src.toNIO.toAbsolutePath.toString,
    )
    val compiler = new org.jetbrains.kotlin.cli.jvm.K2JVMCompiler
    val renderer = org.jetbrains.kotlin.cli.common.messages.MessageRenderer.PLAIN_FULL_PATHS
    val code = compiler.exec(out, renderer, args: _*)
    if (code.getCode != 0) {
      val msg = new String(buf.toByteArray, StandardCharsets.UTF_8)
      automod.exit(-1,
        s"""Kotlin compile failed: ${code}
           |$msg
           |$source""".stripMargin)
    }
    val cl = new java.net.URLClassLoader(
      Array(outDir.toNIO.toUri.toURL), Thread.currentThread.getContextClassLoader)
    try {
      val cls = cl.loadClass("ktp.KtPatchKt")
      val patchVCls = cl.loadClass("ktp.PatchV")
      val ctor = patchVCls.getConstructor(classOf[java.util.Map[_, _]])
      val patchV = ctor.newInstance(v)
      val m = cls.getMethod("eval", patchVCls)
      m.invoke(null, patchV)
    } catch {
      case e: java.lang.reflect.InvocationTargetException =>
        automod.exit(-1,
          s"""Kotlin patch threw: ${e.getCause}
             |$source""".stripMargin)
    } finally {
      cl.close()
      os.remove.all(tmp)
    }
  }

  /**
   * Compiles [body] as a `=kt>` property-value expression (evalProperty): `eval(v: ValV)` where
   * `v` exposes objName/orig/current/ast/origAst/valueOf as plain values (not bytes). Mirrors the
   * Kotlin source evalKotlinValue; do NOT reuse evalKotlin (RawScript context) here — that casts
   * orig/current to ByteArray and requires a Unity bundle context for toJson().
   */
  def evalKotlinValue(v: java.util.Map[String, Any], body: String): Any = {
    val tmp = os.temp.dir()
    val src = tmp / "KtPatch.kt"
    val source =
      s"""package ktp
         |
         |import com.fasterxml.jackson.databind.JsonNode
         |import com.fasterxml.jackson.databind.ObjectMapper
         |import com.fasterxml.jackson.databind.node.*
         |
         |class ValV(val map: MutableMap<String, Any?>) {
         |  val objName: String get() = map["objName"] as String
         |  val orig: Any? get() = map["orig"]
         |  val current: Any? get() = map["current"]
         |  val ast: JsonNode get() = map["ast"] as JsonNode
         |  val origAst: JsonNode get() = map["origAst"] as JsonNode
         |  operator fun get(key: String): Any? = map[key]
         |  val valueOf: (String, String) -> JsonNode? get() = map["valueOf"] as (String, String) -> JsonNode?
         |}
         |
         |fun eval(v: ValV): Any? = run {
         |$body
         |}
         |""".stripMargin
    Files.write(src.toNIO, source.getBytes(StandardCharsets.UTF_8))
    val outDir = tmp / "out"
    Files.createDirectories(outDir.toNIO)
    val buf = new java.io.ByteArrayOutputStream()
    val out = new PrintStream(buf)
    val args = Array(
      "-classpath", kotlinCompilerClasspath,
      "-no-stdlib", "-no-reflect",
      "-d", outDir.toNIO.toAbsolutePath.toString,
      src.toNIO.toAbsolutePath.toString,
    )
    val compiler = new org.jetbrains.kotlin.cli.jvm.K2JVMCompiler
    val renderer = org.jetbrains.kotlin.cli.common.messages.MessageRenderer.PLAIN_FULL_PATHS
    val code = compiler.exec(out, renderer, args: _*)
    if (code.getCode != 0) {
      val msg = new String(buf.toByteArray, StandardCharsets.UTF_8)
      automod.exit(-1,
        s"""Kotlin compile failed: ${code}
           |$msg
           |$source""".stripMargin)
    }
    val cl = new java.net.URLClassLoader(
      Array(outDir.toNIO.toUri.toURL), Thread.currentThread.getContextClassLoader)
    try {
      val cls = cl.loadClass("ktp.KtPatchKt")
      val valVCls = cl.loadClass("ktp.ValV")
      val ctor = valVCls.getConstructor(classOf[java.util.Map[_, _]])
      val valV = ctor.newInstance(v)
      val m = cls.getMethod("eval", valVCls)
      m.invoke(null, valV)
    } catch {
      case e: java.lang.reflect.InvocationTargetException =>
        automod.exit(-1,
          s"""Kotlin value threw: ${e.getCause}
             |$source""".stripMargin)
    } finally {
      cl.close()
      os.remove.all(tmp)
    }
  }

  /**
   * Compiles [body] as a Kotlin `(v: String) -> Boolean` predicate (used for `=kt>` object-name
   * filters) and returns a Scala closure wrapping the compiled `eval`.
   */
  def evalKotlinPredicate(body: String): String => Boolean = {
    val v = new java.util.HashMap[String, Any]()
    v.put("v", "")
    // compile once to discover the loaded class; the returned closure recompiles per call is
    // wasteful, so we compile a small indirection: eval(v) -> Boolean, then per call we set v
    // into a fresh map and invoke. Compilation happens once here.
    val holder = compilePredicate(body)
    (s: String) => {
      val arg = new java.util.HashMap[String, Any]()
      arg.put("v", s)
      holder(arg).asInstanceOf[Boolean]
    }
  }

  private def compilePredicate(body: String): (java.util.Map[String, Any]) => Any = {
    val tmp = os.temp.dir()
    val src = tmp / "KtPred.kt"
    val source =
      s"""package ktp
         |
         |fun eval(v: MutableMap<String, Any>): Any = run {
         |  val v = v["v"] as String
         |$body
         |}
         |""".stripMargin
    Files.write(src.toNIO, source.getBytes(StandardCharsets.UTF_8))
    val outDir = tmp / "out"
    Files.createDirectories(outDir.toNIO)
    val buf = new java.io.ByteArrayOutputStream()
    val out = new PrintStream(buf)
    val args = Array(
      "-classpath", kotlinCompilerClasspath,
      "-no-stdlib", "-no-reflect",
      "-d", outDir.toNIO.toAbsolutePath.toString,
      src.toNIO.toAbsolutePath.toString,
    )
    val compiler = new org.jetbrains.kotlin.cli.jvm.K2JVMCompiler
    val renderer = org.jetbrains.kotlin.cli.common.messages.MessageRenderer.PLAIN_FULL_PATHS
    val code = compiler.exec(out, renderer, args: _*)
    if (code.getCode != 0) {
      val msg = new String(buf.toByteArray, StandardCharsets.UTF_8)
      automod.exit(-1,
        s"""Kotlin predicate compile failed: ${code}
           |$msg
           |$source""".stripMargin)
    }
    val cl = new java.net.URLClassLoader(
      Array(outDir.toNIO.toUri.toURL), Thread.currentThread.getContextClassLoader)
    val cls = cl.loadClass("ktp.KtPredKt")
    os.remove.all(tmp)
    val m = cls.getMethod("eval", classOf[java.util.Map[_, _]])
    (arg: java.util.Map[String, Any]) => m.invoke(null, arg)
  }
}
