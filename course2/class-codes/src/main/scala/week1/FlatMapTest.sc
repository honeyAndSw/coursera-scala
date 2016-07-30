import scala.collection.immutable.IndexedSeq

object FlatMapTest {
  val withForExpression = for {
    i <- 1 until 5
    j <- 1 until i
  } yield (i, j)

  private val toFlatMap: (Int) => IndexedSeq[(Int, Int)] =
    i => (1 until i).map(j => (i, j))

  val withFlatMap = (1 until 5)
    .flatMap(toFlatMap)
}