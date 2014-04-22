package scala.tools.nsc.palladium
import scala.tools.nsc.Global
import Compat._

class Attachments[G <: Global with Singleton](val global: G) {

  var touchedSymbols: List[global.Symbol] = Nil
  def attachment = ExpansionSummaryAttachment(touchedSymbols)

  // Importing the content of global allows us to stay source compatible with versions of scala
  // that don't define ImportableAttachment (< 2.11) and that don't have Importer (< 2.10)
  import global._
  case class ExpansionSummaryAttachment(val touchedSymbols: List[global.Symbol]) extends ImportableAttachment {
    def importAttachment(importer: Importer) = {
      touchedSymbols map (sym => importer.importSymbol(sym.asInstanceOf[importer.from.Symbol]))
      this
    }
  }

}
