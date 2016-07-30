object Generators {
  trait Generator[+T] {
    self =>

    def generate: T

    def map[S](f: T => S): Generator[S] = new Generator[S] {
      override def generate: S = f(self.generate)
    }

    def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
      override def generate: S = f(self.generate).generate
    }
  }
  /**
    * Int type generator
    */
  val integers = new Generator[Int] {
    val rand = new java.util.Random
    def generate = rand.nextInt()
  }

  /**
    * Boolean type generator
    * Implemented with anonymous object
    */
  val booleansAnonymous = new Generator[Boolean] {
    def generate = integers.generate > 0
  }

  /**
    * Pair of Int generator
    * Implemented with anonymous object
    */
  val pairsAnonymous = new Generator[(Int, Int)] {
    def generate = (integers.generate, integers.generate)
  }

  /**
    * Without for expression support, compiler will say:
    * 'value map is not a member of ...'
    * because internally, code below is identical to:
    * val booleans = integers map (x => x > 0)
    */
  val booleans = for (x <- integers) yield (x > 0)

  /**
    * Without for expression support, compiler will say:
    * 'value flatMap is not a member of ...'
    * because internally, code below is identical to:
    * def pairs[T, U](t: Generator[T], u: Generator[U]) =
    *    t flatMap (x => u map (y => (x, y)))
    */
  def pairs[T, U](t: Generator[T], u: Generator[U]) = for {
    x <- t
    y <- u
  } yield (x, y)
}