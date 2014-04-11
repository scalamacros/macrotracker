package scala.tools.nsc.palladium

import scala.tools.nsc.{Global, Phase, SubComponent}
import scala.tools.nsc.plugins.{Plugin => NscPlugin, PluginComponent => NscPluginComponent}
import scala.tools.nsc.palladium.{Context => PalladiumContext}
import scala.reflect.macros.runtime.AbortMacroException
import scala.reflect.runtime.ReflectionUtils
import scala.reflect.macros.blackbox.{Context => BlackboxContext}
import scala.reflect.macros.whitebox.{Context => WhiteboxContext}
import java.lang.reflect.{Constructor => jConstructor}

class Plugin(val global: Global) extends NscPlugin { self =>
  import global._
  import definitions._
  import analyzer.{AnalyzerPlugin => NscAnalyzerPlugin, MacroPlugin => NscMacroPlugin, _}
  import scala.reflect.internal.Mode
  import scala.reflect.internal.Flags._

  val name = "scalahost"
  val description = """Hosts Project Palladium macros in scalac.
  For more information visit https://github.com/scalareflect/scalahost"""
  val components = List[NscPluginComponent]()
  analyzer.addMacroPlugin(MacroPlugin)

  object MacroPlugin extends NscMacroPlugin {
    case class PalladiumContextAttachment(c1: PalladiumContext)
    def saveContext(expandee: Tree, c1: PalladiumContext): Unit = expandee.updateAttachment(PalladiumContextAttachment(c1))
    def loadContext(expandee: Tree): Option[PalladiumContext] = expandee.attachments.get[PalladiumContextAttachment].map(_.c1)
    def eraseContext(expandee: Tree): Unit = expandee.removeAttachment[PalladiumContextAttachment]

    override def pluginsMacroRuntime(expandee: Tree): Option[MacroRuntime] = {
      // Some(args => {
      //   val c1 = new PalladiumContext(args.c)
      //   saveContext(expandee, c1)
      //   standardMacroRuntime(expandee)(MacroArgs(c1, args.other))
      // })
      // TODO: can't just pass c1 to the standard runtime, because MacroArgs requires scala-compiler.jar's context!! FAIL :(
      // therefore I'm essentially forced to copy/paste the standard implementation from scalac and modify it to use c1
      val macroDef = expandee.symbol
      macroLogVerbose(s"looking for macro implementation: $macroDef")
      if (fastTrack contains macroDef) {
        macroLogVerbose("macro expansion is serviced by a fast track")
        Some(fastTrack(macroDef))
      } else {
        val binding = loadMacroImplBinding(macroDef).get
        val isBundle = binding.isBundle
        val className = binding.className
        val methName = binding.methName
        if (className == Predef_???.owner.javaClassName && methName == Predef_???.name.encoded) {
          Some(args => throw new AbortMacroException(args.c.enclosingPosition, "macro implementation is missing"))
        } else {
          try {
            macroLogVerbose(s"resolving macro implementation as $className.$methName (isBundle = $isBundle)")
            macroLogVerbose(s"classloader is: ${ReflectionUtils.show(defaultMacroClassloader)}")
            val implClass = Class.forName(className, true, defaultMacroClassloader)
            val implMeths = implClass.getDeclaredMethods.find(_.getName == methName)
            // relies on the fact that macro impls cannot be overloaded
            // so every methName can resolve to at maximum one method
            val implMeth = implMeths getOrElse { throw new NoSuchMethodException(s"$className.$methName") }
            macroLogVerbose(s"successfully loaded macro impl as ($implClass, $implMeth)")
            Some(args => {
              val c1 = new PalladiumContext(args.c)
              saveContext(args.c.expandee, c1)
              val others1 = args.others.map {
                case arg: Tree => new c1.universe.RichCompilerTree(arg.asInstanceOf[c1.universe.CompilerTree]).wrap
                case arg: WeakTypeTag[_] => new c1.universe.RichCompilerWeakTypeTag(arg.asInstanceOf[c1.universe.CompilerWeakTypeTag[_]]).wrap
              }
              val implObj =
                if (isBundle) {
                  def isMacroContext(clazz: Class[_]) = clazz == classOf[BlackboxContext] || clazz == classOf[WhiteboxContext]
                  def isBundleCtor(ctor: jConstructor[_]) = ctor.getParameterTypes match {
                    case Array(param) if isMacroContext(param) => true
                    case _ => false
                  }
                  val Array(bundleCtor) = implClass.getConstructors.filter(isBundleCtor)
                  bundleCtor.newInstance(c1)
                } else ReflectionUtils.staticSingletonInstance(implClass)
              val implArgs = if (isBundle) others1 else c1 +: others1
              val result1 = implMeth.invoke(implObj, implArgs.asInstanceOf[Seq[AnyRef]]: _*)
              result1 match {
                case arg: c1.Tree => new c1.universe.RichOurTree(arg).unwrap.asInstanceOf[Tree]
              }
            })
          } catch {
            case ex: Exception =>
              macroLogVerbose(s"macro runtime failed to load: ${ex.toString}")
              macroDef setFlag IS_ERROR
              null
          }
        }
      }
    }

    override def pluginsMacroExpand(typer: Typer, expandee: Tree, mode: Mode, pt: Type): Option[Tree] = {
      val expander = new DefMacroExpander(typer, expandee, mode, pt) {
        override def onSuccess(expanded: Tree): Tree = {
          val result = super.onSuccess(expanded)
          loadContext(expandee).foreach(c1 => {
            val attachments = new Attachments[global.type](global)
            attachments.touchedSymbols = c1.touchedSymbols.toList.asInstanceOf[List[attachments.global.Symbol]]
            val summary = attachments.attachment
            expandee.updateAttachment(summary)
            result.updateAttachment(summary)
          })
          result
        }
      }
      try Some(expander(expandee))
      finally eraseContext(expandee)
    }
  }
}
