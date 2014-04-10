package observed

class SuperClass(val foo: Int) {
  def overrideMe = "Hello"
}
object Observed extends SuperClass(1) {
  val bar = 2
  def baz(p1: Int, p2: Int) = p1 + p2
  def iHaveAnAlternative(p: Int) = p + 1
  def iHaveAnAlternative(p: Boolean) = !p
  override def overrideMe = "Hola"
}