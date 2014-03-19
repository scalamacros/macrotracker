package scala.tools.nsc.palladium

import scala.tools.nsc.{ Global, Phase, SubComponent }
import scala.tools.nsc.plugins.{ Plugin => NscPlugin, PluginComponent => NscPluginComponent }

class Plugin(val global: Global) extends NscPlugin {
  import global._

  val name = "Scala host for Project Palladium"
  val description = """Hosts Project Palladium macros in scalac.
  For more information visit https://github.com/scalareflect/palladium-scalac"""
  val components = List[NscPluginComponent]()
}
