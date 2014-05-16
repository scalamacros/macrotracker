package scala.tools.nsc.macrotracker

import scala.reflect.macros.whitebox.{Context => NscContext}
import scala.tools.nsc.macrotracker.contexts.{Root => MacrotrackerContext}

class Context(val c: NscContext) extends NscContext with MacrotrackerContext {
  import universe._
  lazy val touchedSymbols = scala.collection.mutable.ListBuffer[CompilerSymbol]()
}

