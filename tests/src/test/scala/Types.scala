import org.scalatest.FunSuite
import testcompiler.TestCompiler
import testutils.SymbolsExtractor

/**
 * This test suite tests that all symbols retrieved during a macro expansion
 * through the TypeApi are correctly registered.
 *
 * The defaultProvider uses weakTypeOf[] to get a Type. weakTypeOf is implemented
 * as a macro, which causes exactly 4 symbols to be added. To get rid of them, we
 * drop 4 symbols.
 */

class TypesSuite extends FunSuite {

  test("Test Type.members") {
    val compiler = new TestCompiler

    compiler.compile("ProviderForTypes.testMembers")

    val allTouchedSymbols = SymbolsExtractor(compiler).flatten.drop(4)

    if(allTouchedSymbols.isEmpty) fail("No symbols collected")
    else if(!allTouchedSymbols.contains("value foo")) fail("`val foo` has been touched but not registered")
    else if(!allTouchedSymbols.contains("method bar")) fail("`def bar` has been touched but not registered")
    else if(!allTouchedSymbols.contains("method baz")) fail("`def baz` has been touched but not registered")
  }

  test("Test Type.baseClasses") {
    val compiler = new TestCompiler

    compiler.compile("ProviderForTypes.testBaseClasses")

    val allTouchedSymbols = SymbolsExtractor(compiler).flatten.drop(4)

    if(allTouchedSymbols.isEmpty) fail("No symbols collected")
    if(allTouchedSymbols.size > 1) fail(s"${allTouchedSymbols.size} symbols collected (expected 1)")
    if(allTouchedSymbols.head.toString != "class SuperClass") fail("`class SuperClass` has been touched but not registered")
  }

/*  test("Test Type.decl") { f =>
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
  }*/
}

/**
 * Macro provider
 */

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import scala.reflect.macros._

object ProviderForTypes {

    def testMembers: Unit = macro testMembersImpl
    def testMembersImpl(c: Context) = {
        import c.universe._
        val tpe = c.weakTypeOf[observed.Observed.type]
        q"println(${tpe.members.toString})"
    }

    def testBaseClasses: Unit = macro testBaseClassesImpl
    def testBaseClassesImpl(c: Context) = {
        import c.universe._
        val tpe = c.weakTypeOf[observed.Observed.type]
        q"println(${tpe.baseClasses.toString})"
    }

}
