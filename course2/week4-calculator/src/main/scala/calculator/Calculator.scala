package calculator

sealed abstract class Expr

/** Set values */
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr

/** Operations */
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions map {
      case (key, expr) => {
        val value = Signal {
          if (isSelfDependent(key, expr(), namedExpressions)) Double.NaN
          else eval(expr(), namedExpressions)
        }
        (key, value)
      }
    }
  }

  /**
    * Return true if there exists self dependency between caller and callee.
    *
    * @param caller
    * @param callee
    * @param references
    * @return
    */
  def isSelfDependent(caller: String, callee: Expr,
      references: Map[String, Signal[Expr]]): Boolean = callee match {
    case Literal(v) => false
    case Ref(name) => {
      if (caller == name) true
      else isSelfDependent(caller, getReferenceExpr(name, references), references)
    }
    case Plus(a, b) => isSelfDependent(caller, a , b, references)
    case Minus(a, b) => isSelfDependent(caller, a , b, references)
    case Times(a, b) => isSelfDependent(caller, a , b, references)
    case Divide(a, b) => isSelfDependent(caller, a , b, references)
  }

  /**
    * Helper function to check self dependency.
    * It's useful when two callees have the same callee.
    */
  def isSelfDependent(caller: String, callee1: Expr, callee2: Expr,
      references: Map[String, Signal[Expr]]): Boolean =
    isSelfDependent(caller, callee1, references) ||
      isSelfDependent(caller, callee2, references)

  /**
    * Helper function for each expression.
    *
    * Hint: Refs to other variables could cause cyclic dependencies
    * e.g., a = b + 1 and b = 2 * a.
    * Such cyclic dependencies are considered as errors
    * (failing to detect this will cause infinite loops).
    */
  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = expr match {
    case Literal(v) => v
    case Ref(name) => {
      eval(getReferenceExpr(name, references), references)
    }
    case Plus(a, b) => {
      eval(a, references) + eval(b, references)
    }
    case Minus(a, b) => {
      eval(a, references) - eval(b, references)
    }
    case Times(a, b) => {
      eval(a, references) * eval(b, references)
    }
    case Divide(a, b) => {
      eval(a, references) / eval(b, references)
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]): Expr = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
