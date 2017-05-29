package observatory

import com.sksamuel.scrimage.{Image, Pixel, Color => ScriImageColor}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  private val MAX_KM_USE_GIVE_TEMP = 1

  /* power parameter on the inverse distance weighting algorithm */
  private val P: Double = 2
  /* radius of Earth */
  private val EARTH_RADIUS: Double = 6371

  /**
    * Inverse distance weighting
    * https://en.wikipedia.org/wiki/Inverse_distance_weighting
    *
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    // from (location of known location, temperature of known location)
    // to (distance of the two, temperature of known location) sorted by distance
    val distTempTuples: Iterable[(Double, Double)] = temperatures.map { case (loc, temp) =>
      val d = distance(loc, location)
      (d, temp)
    }.toSeq.sortBy(_._1)

    val head = distTempTuples.head

    if (head._1 <= MAX_KM_USE_GIVE_TEMP) {
      head._2
    }
    else {
      val dinom = distTempTuples.map{case (dist, _) => 1.0 / Math.pow(dist, P)}.reduce(_ + _)
      val num = distTempTuples.map{case (dist, temp) => temp / Math.pow(dist, P)}.reduce(_ + _)

      (num / dinom)
    }
  }

  /**
    * Great-circle distance
    * https://en.wikipedia.org/wiki/Great-circle_distance
    *
    * http://www.movable-type.co.uk/scripts/latlong.html
    * http://introcs.cs.princeton.edu/java/12types/GreatCircle.java.html
    */
  def distance(l1: Location, l2: Location): Double = {
    def greatCircleDistance = {
      val (lat1, lon1) = (l1.lat.toRadians, l1.lon.toRadians)
      val (lat2, lon2) = (l2.lat.toRadians, l2.lon.toRadians)

      val diffLat: Double = Math.abs(lat1 - lat2)
      val diffLon: Double = Math.abs(lon1 - lon2)

      val angle: Double = Math.acos(
        Math.sin(lat1) * Math.sin(lat2) + Math.cos(lat1) * Math.cos(lat2) * Math.cos(diffLon)
      )

      (angle * EARTH_RADIUS.toRadians).toDegrees
    }

    if (l1.equals(l2)) 0.0 else greatCircleDistance
  }

  /**
    * Linear interpolation
    * https://en.wikipedia.org/wiki/Linear_interpolation
    *
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    // Sorted by temperatures
    val tempSortedPoints = points.toSeq.sortBy(_._1)

    if (tempSortedPoints(0)._1 >= value) tempSortedPoints(0)._2
    else if (tempSortedPoints.last._1 <= value) tempSortedPoints.last._2
    else {
      val x0 = tempSortedPoints(tempSortedPoints.lastIndexWhere{case (temp, _) => temp <= value})
      val x1 = tempSortedPoints(tempSortedPoints.indexWhere{case (temp, _) => temp >= value})

      if (x0._1 == x1._1) x0._2
      else linearInterpolateColor(x0, x1, value)
    }
  }

  private def linearInterpolateColor(p1: (Double, Color), p2: (Double, Color), p: Double): Color =
    Color(
      linearInterpolate((p1._1, p1._2.red),   (p2._1, p2._2.red),   p),
      linearInterpolate((p1._1, p1._2.green), (p2._1, p2._2.green), p),
      linearInterpolate((p1._1, p1._2.blue),  (p2._1, p2._2.blue),  p)
    )

  private def linearInterpolate(p1: (Double, Int), p2: (Double, Int), p: Double): Int = {
    val (x0, y0) = p1
    val (x1, y1) = p2
    val x = p

    Math.round(((y0 * (x1 - x)) + (y1 * (x - x0))) / (x1 - x0)).toInt
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    val (width, height) = (360, 180)
    val pixels: Array[Pixel] = new Array[Pixel](width * height)

    for {
      w <- 0 until width
      h <- 0 until height
    } yield {
      val (lat, lon) = (-h + 90, w - 180)
      val temp: Double = predictTemperature(temperatures, Location(lat, lon))
      val color: Color = interpolateColor(colors, temp)

      val pixel: Pixel = Pixel(ScriImageColor(color.red, color.green, color.blue))
      pixels(h * width + w) = pixel
    }

    Image(width, height, pixels)
  }
}

