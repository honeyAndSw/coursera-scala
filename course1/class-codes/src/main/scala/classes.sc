import week3.Rational
object scratch {
  val rational = new Rational(1, 2)
  def error(message: String) = throw new Error(message)
  error("this is an error message.")
  
  // val x: Int = null
}