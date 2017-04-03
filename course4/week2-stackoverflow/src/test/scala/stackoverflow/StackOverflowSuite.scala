package stackoverflow

import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.rdd.RDD
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FunSuite}

@RunWith(classOf[JUnitRunner])
class StackOverflowSuite extends FunSuite with BeforeAndAfterAll {

  lazy val testObject = new StackOverflow {
    override val langs =
      List(
        "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
        "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")
    override def langSpread = 50000
    override def kmeansKernels = 45
    override def kmeansEta: Double = 20.0D
    override def kmeansMaxIterations = 120
  }

  test("testObject can be instantiated") {
    val instantiatable = try {
      testObject
      true
    } catch {
      case _: Throwable => false
    }
    assert(instantiatable, "Can't instantiate a StackOverflow object")
  }

  val fixture = new {
    val conf = new SparkConf().setMaster("local").setAppName("StackOverflow-test")
    val sc = new SparkContext(conf)
    val lines = sc.textFile("src/main/resources/stackoverflow/stackoverflow.csv")
    val raw: RDD[Posting] = testObject.rawPostings(lines)
  }

  test("scoredPostings should contain expected values") {
    val f = fixture

    val grouped = testObject.groupedPostings(f.raw)
    val scored: RDD[(Posting, Int)] = testObject.scoredPostings(grouped)
    val map = scored.collectAsMap()

    val shouldContain = List(
      ((1,6,None,None,140,Some("CSS")),67),
      ((1,42,None,None,155,Some("PHP")),89),
      ((1,72,None,None,16,Some("Ruby")),3),
      ((1,126,None,None,33,Some("Java")),30),
      ((1,174,None,None,38,Some("C#")),20)
    )

    shouldContain.foreach { elem =>
      val posting = new Posting(
        elem._1._1, elem._1._2, elem._1._3,
        elem._1._4, elem._1._5, elem._1._6)
      assert(map.contains(posting))
      assert(map.get(posting).get == elem._2)
    }
  }

  test("vectorPostings should contain expected values") {
    val f = fixture

    val grouped = testObject.groupedPostings(f.raw)
    val scored = testObject.scoredPostings(grouped)
    val vectors: RDD[(Int, Int)] = testObject.vectorPostings(scored)
    val map = vectors.collect()

    val shouldContain = List(
      (350000,67),
      (100000,89),
      (300000,3),
      (50000,30),
      (200000,20)
    )

    shouldContain.foreach { elem =>
      assert(map.contains(elem))
    }
  }
}
