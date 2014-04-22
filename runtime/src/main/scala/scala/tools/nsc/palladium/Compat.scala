package scala.tools.nsc.palladium

// These classes are only there to provide source compatibility with Scala versions
// that do not have ImportableAttachment nor Importer
private[palladium] object Compat {
  class ImportableAttachment
  class Importer {
    class ImporterCompat {
      class Symbol
    }
    val from = new ImporterCompat
    def importSymbol(a: Any) = 0
  }
}
