package testutils

import testcompiler.TestCompiler
import scala.reflect.internal._
import scala.reflect.runtime.universe._
import scala.tools.nsc.palladium._

import scala.language.reflectiveCalls

object SymbolsExtractor {

	def apply(compiler: TestCompiler) =
		compiler.trees.map {
			case t: {def attachments: scala.reflect.macros.Attachments } =>
				t.attachments.all.collect {
					case esa: { def touchedSymbols: List[_] } =>
						esa.touchedSymbols
				}.toList.flatten
		}
}