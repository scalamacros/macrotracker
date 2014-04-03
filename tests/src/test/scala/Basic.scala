import testcompiler.{DefaultSettings, TestCompiler}
import testutils.{SymbolsExtractor, TemporaryDirSuite}

class BasicSuite extends TemporaryDirSuite {

  test("Test no lookup") { f =>
    val compiler = new TestCompiler(DefaultSettings(f.dir))
    val srcA = """
            |package test
            |import scala.language.experimental.macros
            |import scala.reflect.macros.whitebox.Context
            |import scala.reflect.macros._
            |object Provider {
            |  def testMacro: Unit = macro testMacroImpl
            |  def testMacroImpl(c: Context) = {
            |    import c.universe._
            |    val str = "Hello, world !"
            |    q"println($str)"
            |  }
            |}""".stripMargin

    val srcB = """
            |package test
            |object Client {
            |  Provider.testMacro
            |}""".stripMargin

    compiler.compile(List(srcA))
    compiler.compile(List(srcB))

    val allTouchedSymbolsAsStrings = SymbolsExtractor(compiler).flatten.map(_.toString)

    if(!allTouchedSymbolsAsStrings.isEmpty) fail(s"${allTouchedSymbolsAsStrings.size} symbols collected (expected 0)")
  }

}