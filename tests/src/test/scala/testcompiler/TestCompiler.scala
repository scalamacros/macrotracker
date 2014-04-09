package testcompiler

class TestCompiler {

	val plugin = "-Xplugin:" + System.getProperty("scalahost.plugin.jar")
	val classpath = "-cp " + sys.props("sbt.class.directory")

	import scala.tools.reflect.ToolBox
	val tb = scala.reflect.runtime.currentMirror.mkToolBox(options = classpath + " " + plugin)

	var trees: List[tb.u.Tree] = Nil

	def compile(src: String): Unit = {
		trees ::= tb.typecheck(tb.parse(src))
	}
}
