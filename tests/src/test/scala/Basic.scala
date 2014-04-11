import testcompiler.TestCompiler
import org.scalatest.FunSuite

class BasicSuite extends FunSuite {

  test("Test no lookup") {
    val compiler = new TestCompiler
    val client = "NoLookupProvider.testMacro";

    compiler.compile(client)

    val allTouchedSymbols = compiler.touchedSymbols
    assert(allTouchedSymbols.isEmpty, s"${allTouchedSymbols.size} symbols collected (expected 0)")
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
