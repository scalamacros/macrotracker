import testcompiler.{DefaultSettings, TestCompiler}
import testutils.{SymbolsExtractor, TemporaryDirSuite}

/**
 * This test suite tests that all symbols retrieved during a macro expansion
 * through the TypeApi are correctly registered.
 *
 * The defaultProvider uses weakTypeOf[] to get a Type. weakTypeOf is implemented
 * as a macro, which causes exactly 4 symbols to be added. To get rid of them, we
 * drop 4 symbols.
 */
class TypesSuite extends TemporaryDirSuite {

  test("Test Type.members") { f =>
    val compiler = new TestCompiler(DefaultSettings(f.dir))
    val srcA = defaultProvider("tpe.members")
    val srcB = defaultClient

    compiler.compile(List(srcA))
    compiler.compile(List(srcB))

    val allTouchedSymbolsAsStrings = SymbolsExtractor(compiler).flatten.map(_.toString).drop(4)

    if(allTouchedSymbolsAsStrings.isEmpty) fail("No symbols collected")
    else if(!allTouchedSymbolsAsStrings.contains("value foo")) fail("`val foo` has been touched but not registered")
    else if(!allTouchedSymbolsAsStrings.contains("method bar")) fail("`def bar` has been touched but not registered")
    else if(!allTouchedSymbolsAsStrings.contains("method baz")) fail("`def baz` has been touched but not registered")
  }

  test("Test Type.baseClasses") { f =>
    val compiler = new TestCompiler(DefaultSettings(f.dir))
    val srcA = defaultProvider("tpe.baseClasses")
    val srcB = defaultClient

    compiler.compile(List(srcA))
    compiler.compile(List(srcB))

    val allTouchedSymbolsAsStrings = SymbolsExtractor(compiler).flatten.map(_.toString).drop(4)

    if(allTouchedSymbolsAsStrings.isEmpty) fail("No symbols collected")
    else if(!allTouchedSymbolsAsStrings.contains("class SuperClass")) fail("`class SuperClass` has been touched but not registered")
  }

  test("Test Type.decl") { f =>
    val compiler = new TestCompiler(DefaultSettings(f.dir))
    val srcA = defaultProvider("tpe.decl(newTermName(\"bar\"))")
    val srcB = defaultClient

    compiler.compile(List(srcA))
    compiler.compile(List(srcB))

    val allTouchedSymbolsAsStrings = SymbolsExtractor(compiler).flatten.map(_.toString).drop(4)

    if(allTouchedSymbolsAsStrings.isEmpty) fail("No symbols collected")
    else if(!allTouchedSymbolsAsStrings.contains("method bar")) fail("`def bar` has been touched but not registered")
  }

  test("Test Type.member") { f =>
    val compiler = new TestCompiler(DefaultSettings(f.dir))
    val srcA = defaultProvider("tpe.member(newTermName(\"bar\"))")
    val srcB = defaultClient

    compiler.compile(List(srcA))
    compiler.compile(List(srcB))

    val allTouchedSymbolsAsStrings = SymbolsExtractor(compiler).flatten.map(_.toString).drop(4)

    if(allTouchedSymbolsAsStrings.isEmpty) fail("No symbols collected")
    else if(!allTouchedSymbolsAsStrings.contains("method bar")) fail("`def bar` has been touched but not registered")
  }

  // Not sure how to to test this one
  test("Test Type.paramLists") { f =>
  }

  test("Test Type.termSymbol") { f =>
    val compiler = new TestCompiler(DefaultSettings(f.dir))
    val srcA = defaultProvider("tpe.termSymbol")
    val srcB = defaultClient

    compiler.compile(List(srcA))
    compiler.compile(List(srcB))

    val allTouchedSymbolsAsStrings = SymbolsExtractor(compiler).flatten.map(_.toString).drop(4)

    if(allTouchedSymbolsAsStrings.isEmpty) fail("No symbols collected")
    else if(!allTouchedSymbolsAsStrings.contains("object Observed")) fail("`def bar` has been touched but not registered")
  }

  test("Test Type.typeSymbol") { f =>
    val compiler = new TestCompiler(DefaultSettings(f.dir))
    val srcA = defaultProvider("tpe.typeSymbol")
    val srcB = defaultClient

    compiler.compile(List(srcA))
    compiler.compile(List(srcB))

    val allTouchedSymbolsAsStrings = SymbolsExtractor(compiler).flatten.map(_.toString).drop(4)

    if(allTouchedSymbolsAsStrings.isEmpty) fail("No symbols collected")
    else if(!allTouchedSymbolsAsStrings.contains("object Observed")) fail("`def bar` has been touched but not registered")
  }

  val defaultClient = """
    |package test
    |object Client {
    |  Provider.testMacro
    |}""".stripMargin

  def defaultProvider(expr: String) = ("""
    |package test
    |import scala.language.experimental.macros
    |import scala.reflect.macros.whitebox.Context
    |import scala.reflect.macros._
    |class SuperClass {
    |  def baz = 6
    |}
    |object Observed extends SuperClass {
    |  val foo = 4
    |  def bar(parameter1: Int, parameter2: Int) = parameter1 + parameter2
    |}
    |object Provider {
    |  def testMacro: Unit = macro testMacroImpl
    |  def testMacroImpl(c: Context) = {
    |    import c.universe._
    |    val tpe = c.weakTypeOf[Observed.type]
    |    val result = """ + expr + """
    |    q"println(${result.toString})"
    |  }
    |}""").stripMargin

}