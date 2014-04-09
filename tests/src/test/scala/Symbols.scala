import org.scalatest.FunSuite
import testcompiler.TestCompiler
import testutils.SymbolsExtractor

/**
 * This test suite tests that all symbols retrieved during a macro expansion
 * through the SymbolApi are correctly registered.
 *
 * The defaultProvider uses weakTypeOf[] to get a Type. weakTypeOf is implemented
 * as a macro, which causes exactly 4 symbols to be added. Then we use Type.typeSymbol,
 * which causes 2 more symbols to be added. To get rid of them, we drop 6 symbols.
 */

class SymbolsSuite extends FunSuite {

  test("Test Symbol.owner") {
    val compiler = new TestCompiler

    compiler.compile("ProviderForSymbols.testMacro")

    val allTouchedSymbols = SymbolsExtractor(compiler).flatten.drop(6)

    assert(!allTouchedSymbols.isEmpty, "No symbols collected")
    assert(allTouchedSymbols.head.toString == "package observed", "Wrong owner")
  }
}

/**
 * Macro provider
 */
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import scala.reflect.macros._

object ProviderForSymbols {
  def testMacro: Unit = macro testMacroImpl
  def testMacroImpl(c: Context) = {
    import c.universe._
    val tpe = c.weakTypeOf[observed.Observed.type]
    val sym = tpe.typeSymbol
    q"""println(${sym.owner.toString})"""
  }
}