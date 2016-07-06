object nqueens {
  val pairs = for {
    i <- 1 until 5
    j <- 1 until 5
  } yield (i, j)

  println(pairs)
}