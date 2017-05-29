package observatory

import com.sksamuel.scrimage.Image
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers
import org.scalatest.Matchers._

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers {
  test("distance") {
    val jfk = Location(40.6398010300, -73.7789001500)
    val lhr = Location(51.4706001282, -0.4619410038)
    val dist = Visualization.distance(jfk, lhr)
    println(dist)
  }

  test("predictTemperature: some point closer") {
    val location1 = Location(1,1)
    val temp1 = 10d
    val location2 = Location(-10,-10)
    val temp2 = 50d
    val list = List(
      (location1, temp1),
      (location2, temp2)
    )
    val result = Visualization.predictTemperature(list, Location(0, 0))
    assert(temp1 - result < temp2 - result)
  }

  test("[#2 - Raw data display] (1) predicted temperature at location z should be closer to known temperature at location x than to known temperature at location y, if z is closer (in distance) to x than y, and vice versa") {
    val temps = List(
      (Location(45.0, -90.0), 10.0), (Location(-45.0, 0.0), 20.0)
    )
    // [Observed Error] 12.0 did not equal 10.0 +- 1.0E-4 Incorrect predicted temperature at Location(45.0,-90.0): 12.0. Expected: 10.0
    val result = Visualization.predictTemperature(temps, Location(45.0, -90.0))
    result should equal (10.0)
  }

  test("[#2 - Raw data display] (2) predicted temperature at location z should be closer to known temperature at location x than to known temperature at location y, if z is closer (in distance) to x than y, and vice versa") {
    val temps = List(
      (Location(45.0, -90.0), 10.0), (Location(-45.0, 0.0), 20.0)
    )
    val result = Visualization.predictTemperature(temps, Location(90.0, -180.0))
    assert(Math.abs(10.0 - result) < Math.abs(20.0 - result))
  }

  test("[#2 - Raw data display] (1) color interpolation") {
    Visualization.interpolateColor(
      List((-1.0, Color(255,0,0)), (0.0, Color(0,0,255))),
      -0.75
    ) should equal (Color(191, 0, 64))
  }

  test("[#2 - Raw data display] (2) color interpolation") {
    Visualization.interpolateColor(
      List(
        (-2.147483648E9,Color(255,0,0)), (1.0,Color(0,0,255))
      ),
      -1.61061273575E9
    ) should equal (Color(191, 0, 64))
  }

  test("calculate pixel index") {
    def idx(loc: Location): Int =
      ((-loc.lat + 90) * 360 + (loc.lon + 180)).toInt

    idx(Location(90, -180)) should equal (0)
    idx(Location(90, 179)) should equal (359)
    idx(Location(-89, -180)) should equal (179 * 360)
  }

  test("[#2 - Raw data display] visualize") {
    val temps = List(
      (Location(45.0,-90.0), 100.0), (Location(-45.0,0.0), 1.0)
    )
    val colors = List(
      (100.0, Color(255,0,0)), (1.0, Color(0,0,255))
    )

    Visualization.visualize(temps, colors)
  }
}
