package scala.tools.nsc.palladium

class ExpansionSummaryAttachment(val global: scala.tools.nsc.Global) {
  var touchedSymbols: List[global.Symbol] = Nil
}