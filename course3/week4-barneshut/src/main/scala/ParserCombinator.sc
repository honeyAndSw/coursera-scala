import scala.util.parsing.combinator._
import scala.util.parsing.json._

object PC extends JavaTokenParsers {
  val expression: String = "3 - 4 + 5"

  def expr1 = expr ~ opt(("+"|"-") ~ expr)
  def expr: Parser[Any] = term ~ opt(("+"|"-") ~ term) // term +/- expr
  //def expr: Parser[Any] = term ~ opt(("+"|"-") ~ expr)
  def term: Parser[Any] = factor ~ opt(("*"|"/") ~ factor) // factor */ factor
    //term ~ opt(("+"|"-") ~ expr)
  def factor: Parser[Any] = wholeNumber | "(" ~> expr <~ ")"

  def result = parse(expr1, expression)
}
println(PC.result)