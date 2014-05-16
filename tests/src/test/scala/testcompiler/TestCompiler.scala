package testcompiler

import scala.tools.reflect.ToolBox
import scala.reflect.runtime.{universe => ru}

class TestCompiler {

	val plugin = "-Xplugin:" + System.getProperty("macrotracker.plugin.jar")
	val classpath = "-cp " + sys.props("sbt.class.directory")

	val tb = scala.reflect.runtime.currentMirror.mkToolBox(options = classpath + " " + plugin)

	var trees: List[ru.Tree] = Nil

	def compile(src: String): Unit = {
		trees ::= tb.typecheck(tb.parse(src))
	}

  def touchedSymbols: List[ru.Symbol] = {
    val ru1 = ru.asInstanceOf[scala.reflect.macros.Universe]
    import ru1.internal._, decorators._

    trees.map { t =>
      t.asInstanceOf[ru1.Tree].attachments.all.collect {
        case syms: Map[String, Any] => syms.get("touchedSymbols").getOrElse(Nil).asInstanceOf[List[ru.Symbol]]
      }
    }.flatten.flatten
  }
}
