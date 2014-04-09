package observed

class SuperClass(val foo: Int)
object Observed extends SuperClass(1) {
  val bar = 2
  def baz(p1: Int, p2: Int) = p1 + p2
}