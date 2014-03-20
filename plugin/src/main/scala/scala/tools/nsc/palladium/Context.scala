package scala.tools.nsc.palladium

import scala.reflect.macros.whitebox.{Context => NscContext}
import scala.collection.mutable

class Context(val c: NscContext) extends NscContext {
  val touchedSymbols = mutable.ListBuffer[c.universe.Symbol]()
  // TODO: implement methods of NscContext
}