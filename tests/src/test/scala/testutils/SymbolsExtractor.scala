package testutils

import testcompiler.TestCompiler

import scala.language.reflectiveCalls

object SymbolsExtractor {

	def apply(compiler: TestCompiler) = compiler.trees.map { t =>
    t.asInstanceOf[scala.reflect.internal.Trees$Tree].attachments.all.collect {
      case esa: { def touchedSymbols: List[_] } => esa.touchedSymbols
    }
  }.toList.flatten

}