package scala.tools.nsc.palladium

class OurAttachment(val global: scala.tools.nsc.Global) {

  def attachment = new ExpansionSummaryAttachment

  class ExpansionSummaryAttachment extends global.ImportableAttachment {
    def importAttachment(importer: global.Importer) = this
    var touchedSymbols: List[global.Symbol] = Nil
  }

}
