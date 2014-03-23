package scala.tools.nsc.palladium

import scala.reflect.macros.whitebox.{Context => NscContext}
import scala.tools.nsc.palladium.contexts.{Root => PalladiumContext}

class Context(val c: NscContext) extends NscContext with PalladiumContext {
  import universe._
  val touchedSymbols = scala.collection.mutable.ListBuffer[CompilerSymbol]()
}

