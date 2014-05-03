package scala.tools.nsc.palladium
import scala.tools.nsc.Global

class Attachments[G <: Global with Singleton](val global: G) {

  var touchedSymbols: List[global.Symbol] = Nil
  def attachment = OurMap(touchedSymbols)

  // We cannot use a classic Map, because we need to make the attachment importable
  case class OurMap(val syms: Any) extends Map.Map1("touchedSymbols", syms) with global.ImportableAttachment {
    def importAttachment(importer: global.Importer) = {
      this mapValues (sym => importer.importSymbol(sym.asInstanceOf[importer.from.Symbol]))
      this
    }
  }

}
