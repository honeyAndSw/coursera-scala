import scala.io.Source

object x {
  // val in = Source.fromURL("http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords")
  // val words = in.getLines
  val mnem = Map(
    '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
    '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ"
  )

  /**
    * Invert mnem. 'A' -> '2'
    */
  val charCode: Map[Char, Char] =
    for ((ch, codes) <- mnem; code <- codes) yield code -> ch

  def wordCase(word: String): String = word.toUpperCase map charCode

  /**
    * e.g, "5282" -> List("java", "kata", "lava", ...)
    */
  def wordsForNum = ???
}