package testutils

import testcompiler.TestCompiler

import scala.language.reflectiveCalls

object SymbolsExtractor {

	def apply(compiler: TestCompiler): List[List[compiler.Symbol]] = {
		if(compiler.units.isEmpty) {
			Nil
		} else {
			compiler.units.head.body.collect {
				case t: compiler.Tree if!t.attachments.isEmpty => t.attachments.all.collect {
					case _: compiler.analyzer.MacroExpansionAttachment => Nil
					case expansionSummary: { def touchedSymbols: List[compiler.Symbol] } => expansionSummary.touchedSymbols
				}
	      	}.flatten.filterNot(_.isEmpty)
		}
	}
}