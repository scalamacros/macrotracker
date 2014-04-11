import org.scalatest.FunSuite
import testcompiler.TestCompiler
import testutils.SymbolsExtractor

/**
 * This test suite tests that all symbols retrieved during a macro expansion
 * through the SymbolApi are correctly registered.
 */

class SymbolsSuite extends FunSuite {

  test("Test Symbol.alternatives") {
    val compiler = new TestCompiler

    compiler.compile("ProviderForSymbols.testAlternatives")

    val allTouchedSymbols = SymbolsExtractor(compiler).flatten
    val alternatives = allTouchedSymbols.filter(_.toString == "method iHaveAnAlternative")

    assert(alternatives.size == 2, s"Expected to find 2 alternatives, ${alternatives.size} found.")
    assert(alternatives.head != alternatives.tail.head, "Both alternatives are the same symbol")
  }

  test("Test Symbol.overrides") {
    val compiler = new TestCompiler

    compiler.compile("ProviderForSymbols.testOverrides")

    val allTouchedSymbols = SymbolsExtractor(compiler).flatten
    val overriden = allTouchedSymbols.filter(_.toString == "method overrideMe")

    assert(overriden.size == 2, s"Expected to find 2 symbols, ${overriden.size} found.")
    assert(overriden.head != overriden.last, "Both symbols are the same.")
  }

  test("Test Symbol.owner") {
    val compiler = new TestCompiler

    compiler.compile("ProviderForSymbols.testOwner")

    val allTouchedSymbols = SymbolsExtractor(compiler).flatten.map(_.toString)

    assert(allTouchedSymbols.contains("package observed"), "Wrong owner")
  }
}

/**
 * Macro provider
 */
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import scala.reflect.macros._

object ProviderForSymbols {
  def testAlternatives: Unit = macro testAlternativesImpl
  def testAlternativesImpl(c: Context) = {
    import c.universe._
    val tpe = typeOf[observed.Observed.type]
    val alternatives = tpe.decl(newTermName("iHaveAnAlternative")).alternatives
    q"""println(${alternatives.toString})"""
  }

  def testOverrides: Unit = macro testOverridesImpl
  def testOverridesImpl(c: Context) = {
    import c.universe._
    val tpe = typeOf[observed.Observed.type]
    val overriden = tpe.decl(newTermName("overrideMe")).overrides
    q"""println(${overriden.toString})"""
  }

  def testOwner: Unit = macro testOwnerImpl
  def testOwnerImpl(c: Context) = {
    import c.universe._
    val tpe = c.weakTypeOf[observed.Observed.type]
    val sym = tpe.typeSymbol
    q"""println(${sym.owner.toString})"""
  }
}