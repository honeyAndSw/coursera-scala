object polynomials {

  /**
    * Polynomial Class
    * @param terms0 Map(exponent -> coefficient)
    */
  class Poly(terms0: Map[Int, Double]) {
    def this(bindings: (Int, Double)*) = this(bindings.toMap)

    val terms = terms0 withDefaultValue 0.0

    def +(other: Poly): Poly = //new Poly(terms ++ (other.terms map adjustIfExists))
      new Poly((other.terms foldLeft terms)(addTerm))

    def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
      val(exp, coeff) = term
      // + will override existing mapping.
      terms + (exp -> (coeff + terms(exp)))
    }

    /**
      * Based on terms, adjust coefficient value.
      */
    def adjustIfExists(term: (Int, Double)): (Int, Double) = {
      val(exp, coeff) = term
      exp -> (coeff + terms(exp))

      /*terms get exp match {
        case Some(otherCoeff) => exp -> (coeff + otherCoeff)
        case None => exp -> coeff // (exp, coeff)
      }*/
    }

    override def toString = {
      val strs = for {
        (exponent, coefficient) <- terms.toList.sorted.reverse
      } yield "(" + coefficient + " * x^" + exponent + ")"
      strs mkString " + "
    }
  }

  val p1 = new Poly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
  p1.terms(7)
  val p2 = new Poly(0 -> 3.0, 3 -> 7.0)
  p1 + p2
}