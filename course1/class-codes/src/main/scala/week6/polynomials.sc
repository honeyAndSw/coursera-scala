object polynomials {

  /**
    * Polynomial Class
    * @param terms Map(exponent -> coefficient)
    */
  class Poly(val terms: Map[Int, Double]) {

    def + (other: Poly): Poly =
      new Poly(terms ++ (other.terms map adjustIfExists))

    /**
      * Based on terms, adjust coefficient value.
      */
    def adjustIfExists(term: (Int, Double)): (Int, Double) = {
      val(exp, coeff) = term

      terms get exp match {
        case Some(otherCoeff) => exp -> (coeff + otherCoeff) // (exp, coeff + otherCoeff)
        case None => exp -> coeff // (exp, coeff)
      }
    }

    override def toString = {
      val strs = for {
        (exponent, coefficient) <- terms.toList.sorted.reverse
      } yield "(" + coefficient + " * x^" + exponent + ")"
      strs mkString " + "
    }
  }

  val p1 = new Poly(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
  val p2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))
  p1 + p2
}