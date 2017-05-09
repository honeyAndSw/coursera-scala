package observatory

import java.time.LocalDate

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite {

  test("locateTemperatures with test data") {
    val result = Extraction.locateTemperatures(2015, "/test_stations.csv", "/test_2015.csv")

    val expected = Array(
      (LocalDate.of(2015, 8, 11), Location(37.35, -78.433), 27.3),
      (LocalDate.of(2015, 12, 6), Location(37.358, -78.438), 0.0),
      (LocalDate.of(2015, 1, 29), Location(37.358, -78.438), 2.0)
    )

    result.size should be (3)
    result.seq should equal (expected)
  }

  test("locateTemperatures") {
    val result = Extraction.locateTemperatures(1975, "/stations.csv", "/1975.csv")
    result.size should be > (0)
  }

  test("locationYearlyAverageRecords with test data") {
    val result = Extraction.locationYearlyAverageRecords(
      Extraction.locateTemperatures(2015, "/test_stations.csv", "/test_2015.csv"))

    result.size should be (2)
    result.foreach(println)
  }
}