import org.scalatest.FunSuite
import testcompiler.TestCompiler

/**
 * This test suite tests that all symbols retrieved during a macro expansion
 * through the TypeApi are correctly registered.
 */

class TypesSuite extends FunSuite {

  test("Test Type.members") {
    val compiler = new TestCompiler

    compiler.compile("ProviderForTypes.testMembers")

    val allTouchedSymbols = compiler.touchedSymbols.map(_.toString)

    assert(!allTouchedSymbols.isEmpty, "No symbols collected")
    assert(allTouchedSymbols.contains("value foo"), "`val foo` has been touched but not registered")
    assert(allTouchedSymbols.contains("value bar"), "`val bar` has been touched but not registered")
    assert(allTouchedSymbols.contains("method baz"), "`def baz` has been touched but not registered")
  }

  test("Test Type.baseClasses") {
    val compiler = new TestCompiler

    compiler.compile("ProviderForTypes.testBaseClasses")

    val allTouchedSymbols = compiler.touchedSymbols.map(_.toString)

    assert(allTouchedSymbols.contains("class SuperClass"), "`class SuperClass` has been touched but not registered")
    assert(allTouchedSymbols.contains("class Object"), "`class Object` has been touched but not registered")
    assert(allTouchedSymbols.contains("class Any"), "`class Any` has been touched but not registered")
  }

  test("Test Type.decl") {
    val compiler = new TestCompiler

    compiler.compile("ProviderForTypes.testDecl")

    val allTouchedSymbols = compiler.touchedSymbols.map(_.toString)

    assert(allTouchedSymbols.contains("value bar"), "`val bar` has been touched but not registered")
  }

  test("Test Type.member") {
    val compiler = new TestCompiler

    compiler.compile("ProviderForTypes.testMember")

    val allTouchedSymbols = compiler.touchedSymbols.map(_.toString)

    assert(allTouchedSymbols.contains("value foo"), "`val foo` has been touched but not registered")
  }

  // Not sure how to to test this one
  test("Test Type.paramLists") {
    val compiler = new TestCompiler

    compiler.compile("ProviderForTypes.testParamLists")

    val allTouchedSymbols = compiler.touchedSymbols.map(_.toString)

    assert(allTouchedSymbols.contains("class Float"), "`class Float` has been touched but not registered")
  }

  test("Test Type.termSymbol") {
    val compiler = new TestCompiler

    compiler.compile("ProviderForTypes.testTermSymbol")

    val allTouchedSymbols = compiler.touchedSymbols.map(_.toString)

    assert(allTouchedSymbols.contains("object Observed"), "`object Observed` has been touched but not registered")
  }

  test("Test Type.typeSymbol") {
    val compiler = new TestCompiler

    compiler.compile("ProviderForTypes.testTypeSymbol")

    val allTouchedSymbols = compiler.touchedSymbols.map(_.toString)

    assert(allTouchedSymbols.contains("object Observed"), "`object Observed` has been touched but not registered")
  }
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
        val tpe = typeOf[observed.Observed.type]
        q"println(${tpe.members.toString})"
    }

    def testBaseClasses: Unit = macro testBaseClassesImpl
    def testBaseClassesImpl(c: Context) = {
        import c.universe._
        val tpe = typeOf[observed.Observed.type]
        q"println(${tpe.baseClasses.toString})"
    }

    def testDecl: Unit = macro testDeclImpl
    def testDeclImpl(c: Context) = {
        import c.universe._
        val tpe = typeOf[observed.Observed.type]
        val sym = tpe.decl(newTermName("bar"))
        q"println(${sym.toString})"
    }

    def testMember: Unit = macro testMemberImpl
    def testMemberImpl(c: Context) = {
        import c.universe._
        val tpe = typeOf[observed.Observed.type]
        val sym = tpe.member(newTermName("foo"))
        q"println(${sym.toString})"
    }

    def testParamLists: Unit = macro testParamListsImpl
    def testParamListsImpl(c: Context) = {
        import c.universe._
        val tpe = typeOf[observed.SuperClass[Float]]
        q"println(${tpe.paramLists.toString})"
    }

    def testTermSymbol: Unit = macro testTermSymbolImpl
    def testTermSymbolImpl(c: Context) = {
        import c.universe._
        val tpe = c.weakTypeOf[observed.Observed.type]
        val sym = tpe.termSymbol
        q"println(${sym.toString})"
    }

    def testTypeSymbol: Unit = macro testTypeSymbolImpl
    def testTypeSymbolImpl(c: Context) = {
        import c.universe._
        val tpe = typeOf[observed.Observed.type]
        val sym = tpe.typeSymbol
        q"println(${sym.toString})"
    }

}
