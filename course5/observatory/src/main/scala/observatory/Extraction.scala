package observatory

import java.time.LocalDate

import org.apache.spark.sql.{Column, DataFrame}
import org.apache.spark.sql.types._
import org.apache.spark.sql.functions._

/**
  * 1st milestone: data extraction
  */
object Extraction {

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    val stations = SparkHolder.readCsv(getResourcePath(stationsFile), stationsSchema).persist()
    val temps = SparkHolder.readCsv(getResourcePath(temperaturesFile), temperaturesSchema).persist()

//      equalCond("stn")(stations, temps) && equalCond("wban")(stations, temps))
    val joinOnStationId = temps.join(stations,
        ((stations("stn").isNull && temps("stn").isNull) || (stations("stn") === temps("stn"))) ||
        ((stations("wban").isNull && temps("wban").isNull) || (stations("wban") === temps("wban")))
      )
      .filter(stations("latitude").isNotNull && stations("longitude").isNotNull)

    val toTuples: Array[(LocalDate, Location, Double)] = joinOnStationId
      .collect()
      .map { row =>
        val date = LocalDate.of(year, row.getAs[Int]("month"), row.getAs[Int]("day"))
        val location = Location(row.getAs[Double]("latitude"), row.getAs[Double]("longitude"))
        (date, location, toCelsius(row.getAs[Double]("temp")))
      }

    toTuples
  }

  private def equalCond(col: String): (DataFrame, DataFrame) => Column = (d1, d2) => {
    (d1(col).isNull && d2(col).isNull) || (d1(col) === d2(col))
  }

  private def toCelsius(f: Double): Double = ((f + 40) / 1.8) - 40

  private def getResourcePath(resource: String): String = this.getClass.getResource(resource).getPath

  private val stationsSchema: StructType = StructType(Seq(
    StructField("stn", StringType),
    StructField("wban", StringType),
    StructField("latitude", DoubleType),
    StructField("longitude", DoubleType)
  ))

  private val temperaturesSchema: StructType = StructType(Seq(
    StructField("stn", StringType),
    StructField("wban", StringType),
    StructField("month", IntegerType),
    StructField("day", IntegerType),
    StructField("temp", DoubleType)
  ))

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    records.foldLeft(Map[Location, (Double, Int)]()) { case (map, (_, loc, temp)) =>
      if (map.contains(loc)) {
        val prev = map(loc)
        map + ((loc, (prev._1 + temp, prev._2 + 1)))
      } else {
        map + ((loc, (temp, 1)))
      }
    }
    .map { case (loc, (temp, size)) => (loc, temp / size.toDouble) }
  }
}