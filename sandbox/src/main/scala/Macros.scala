import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

object Macros {
  def foo[T](x: T): Unit = macro impl[T]
  def impl[T](c: Context)(x: c.Tree)(implicit T: c.WeakTypeTag[T]) = {
    import c.universe._
    println(x.tpe.typeSymbol)
    println(T.tpe.members.head)
    q"println($x)"
  }
}