package calculator

object Polynomial {
  /**
    * Calcuate discriminant
    *
    * @param a coefficient
    * @param b coefficient
    * @param c coefficient
    * @return b^2 - 4ac, in the form of ax^2 + bx + c
    */
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double])
  : Signal[Double] = {
    Signal{
      (b() * b()) - (4 * a() * c())
    }
  }

  /**
    * Calculate roots for 2nd degree polynomial
    *
    * @param a
    * @param b
    * @param c
    * @param delta
    * @return
    */
  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      if (delta() < 0) {
        Set()
      } else if (delta() == 0) {
        val root: Double = (- b()) / (2 * a())
        Set(root)
      } else {
        val root1 = (- b() + Math.sqrt(delta())) / (2 * a())
        val root2 = (- b() - Math.sqrt(delta())) / (2 * a())
        Set(root1, root2)
      }
    }
  }

}
