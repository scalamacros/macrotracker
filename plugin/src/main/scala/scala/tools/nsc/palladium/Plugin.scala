package scala.tools.nsc.palladium

import scala.tools.nsc.{Global, Phase, SubComponent}
import scala.tools.nsc.plugins.{Plugin => NscPlugin, PluginComponent => NscPluginComponent}
import scala.tools.nsc.palladium.{Context => PalladiumContext}

class Plugin(val global: Global) extends NscPlugin { self =>
  import global._
  import analyzer.{AnalyzerPlugin => NscAnalyzerPlugin, MacroPlugin => NscMacroPlugin, _}
  import scala.reflect.internal.Mode

  val name = "scalahost"
  val description = """Hosts Project Palladium macros in scalac.
  For more information visit https://github.com/scalareflect/scalahost"""
  val components = List[NscPluginComponent]()
  analyzer.addMacroPlugin(MacroPlugin)

  object MacroPlugin extends NscMacroPlugin {
    case class PalladiumContextAttachment(c1: PalladiumContext)

    override def pluginsMacroArgs(typer: Typer, expandee: Tree): Option[MacroArgs] = {
      val MacroArgs(c, others) = standardMacroArgs(typer, expandee)
      val c1 = new PalladiumContext(c)
      expandee.updateAttachment(PalladiumContextAttachment(c1))
      Some(MacroArgs(c1.asInstanceOf[MacroContext], others))
    }

    override def pluginsMacroExpand(typer: Typer, expandee: Tree, mode: Mode, pt: Type): Option[Tree] = {
      val expander = new DefMacroExpander(typer, expandee, mode, pt) {
        override def onSuccess(expanded: Tree): Tree = {
          val result = super.onSuccess(expanded)
          val c1 = expandee.attachments.get[PalladiumContextAttachment].map(_.c1).get
          val summary = new ExpansionSummaryAttachment(global)
          summary.touchedSymbols = c1.touchedSymbols.toList.asInstanceOf[List[summary.global.Symbol]]
          expandee.updateAttachment(summary)
          result.updateAttachment(summary)
          result
        }
      }
      try Some(expander(expandee))
      finally expandee.removeAttachment[PalladiumContextAttachment]
    }
  }
}
