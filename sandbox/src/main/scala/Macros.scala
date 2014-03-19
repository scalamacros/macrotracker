import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

object Macros {
  def foo[T]: String = macro impl[T]
  def impl[T](c: Context)(x: c.Tree)(implicit T: c.WeakTypeTag[T]) = {
    T.tpe
  }
}