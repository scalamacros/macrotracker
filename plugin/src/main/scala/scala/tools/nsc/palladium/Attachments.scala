package scala.tools.nsc.palladium
import scala.tools.nsc.Global

class Attachments[G <: Global with Singleton](val global: G) {

  var touchedSymbols: List[global.Symbol] = Nil
  def attachment = ExpansionSummaryAttachment(touchedSymbols)

  case class ExpansionSummaryAttachment(val touchedSymbols: List[global.Symbol]) extends global.ImportableAttachment {
    def importAttachment(importer: global.Importer) = {
      touchedSymbols map (sym => importer.importSymbol(sym.asInstanceOf[importer.from.Symbol]))
      this
    }
  }

}
