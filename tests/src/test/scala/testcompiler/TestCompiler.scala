package testcompiler

import scala.tools.nsc.{Global, Settings, CompilationUnits}
import scala.reflect.internal.util.{BatchSourceFile, NoFile}
import scala.reflect.io.PlainFile
import scala.io.Source

class TestCompiler(settings: Settings) extends Global(settings) {

	var units: List[CompilationUnit] = Nil

	def compile(srcs: List[String]): Unit = {
		val compileInput = srcs.map { src =>
			val file = new PlainFile(src)

			if(file.exists) {
				val source = new BatchSourceFile(file, Source.fromFile(src).mkString.toArray)
				new CompilationUnit(source)
			} else {
				val source = new BatchSourceFile(NoFile, src)
				new CompilationUnit(source)
			}
		}

		val run = new Run
		run.compileSources(compileInput.map(_.source))
		units = run.units.toList
	}
}
