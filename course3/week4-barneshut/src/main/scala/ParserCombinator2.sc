import scala.util.parsing.combinator._


class Expr

case class Number(value: Int) extends Expr {
  override def toString = s"$value"
}

case class Operator(left: Expr, right: Expr, f: (Int, Int) => Int) extends Expr {
  override def toString = s"($f, $left, $right)"
}

class SimpleLanguageParser extends JavaTokenParsers {

  def expr1: Parser[Expr] = (expr ~ opt(("+" | "-") ~ expr)) ^^ {
    case a ~ None => a
    case a ~ Some("+" ~ b) => Operator(a, b, _ + _)
    case a ~ Some("-" ~ b) => Operator(a, b, _ - _)
  }

  def expr: Parser[Expr] = (term ~ opt(("+" | "-") ~ term)) ^^ {
    case a ~ None => a
    case a ~ Some("+" ~ b) => Operator(a, b, _ + _)
    case a ~ Some("-" ~ b) => Operator(a, b, _ - _)
  }

  def term: Parser[Expr] = (factor ~ opt(("*" | "/" ) ~ term)) ^^ {
    case a ~ None => a
    case a ~ Some("*" ~ b) => Operator(a, b, _ * _)
    case a ~ Some("/" ~ b) => Operator(a, b, _ / _)
  }

  def factor: Parser[Expr] = wholeNumber ^^ (n => Number(Integer.parseInt(n))) | "(" ~> expr <~ ")"

}

object Main {
  def main() = {
    val parser = new SimpleLanguageParser
    val result = parser.parse(parser.expr1, "3 - 4 + 5")
    println(result)

    // (<function2>, 3, (<function2>, 4, 5))
    // (<function2>, (<function2>, 3, 4), 5)
  }
}

Main.main()