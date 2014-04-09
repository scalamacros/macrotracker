import testcompiler.TestCompiler
import org.scalatest.FunSuite
import testutils.SymbolsExtractor

class BasicSuite extends FunSuite {

  test("Test no lookup") {
    val compiler = new TestCompiler
    val client = "NoLookupProvider.testMacro";

    compiler.compile(client)

    val allTouchedSymbolsAsStrings = SymbolsExtractor(compiler).flatten
    assert(allTouchedSymbolsAsStrings.isEmpty, s"${allTouchedSymbolsAsStrings.size} symbols collected (expected 0)")
  }

}

/**
 * Macro providers
 */

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import scala.reflect.macros._

object NoLookupProvider {
  def testMacro: Unit = macro testMacroImpl
  def testMacroImpl(c: Context) = {
    import c.universe._
    q"""println("...")"""
  }
}
