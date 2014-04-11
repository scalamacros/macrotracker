package testutils

import testcompiler.TestCompiler
import scala.reflect.internal._
import scala.reflect.runtime.universe._
import scala.tools.nsc.palladium.ExpansionSummaryAttachment

import scala.language.reflectiveCalls

object SymbolsExtractor {

	def apply(compiler: TestCompiler) = compiler.trees.map { t =>
    t.asInstanceOf[scala.reflect.internal.Trees$Tree].attachments.all.collect {
      case esa: scala.tools.nsc.palladium.ExpansionSummaryAttachment => esa.touchedSymbols
    }
  }.toList.flatten

}