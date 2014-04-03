import testcompiler.{DefaultSettings, TestCompiler}
import testutils.{SymbolsExtractor, TemporaryDirSuite}

/**
 * This test suite tests that all symbols retrieved during a macro expansion
 * through the SymbolApi are correctly registered.
 *
 * The defaultProvider uses weakTypeOf[] to get a Type. weakTypeOf is implemented
 * as a macro, which causes exactly 4 symbols to be added. Then we use Type.typeSymbol,
 * which causes 2 more symbols to be added. To get rid of them, we drop 6 symbols.
 */
class SymbolsSuite extends TemporaryDirSuite {

  test("Test Symbol.owner") { f =>
    val compiler = new TestCompiler(DefaultSettings(f.dir))
    val srcA = defaultProvider("sym.owner")
    val srcB = defaultClient

    compiler.compile(List(srcA))
    compiler.compile(List(srcB))

    val allTouchedSymbolsAsStrings = SymbolsExtractor(compiler).flatten.map(_.toString).drop(6)

    if(allTouchedSymbolsAsStrings.isEmpty) fail("No symbols collected")
    else if(!allTouchedSymbolsAsStrings.contains("package test")) fail("`package test` has been touched but not registered")
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
    |  def bar = 7
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
    |    val sym = tpe.typeSymbol
    |    val result = """ + expr + """
    |    q"println(${result.toString})"
    |  }
    |}""").stripMargin

}