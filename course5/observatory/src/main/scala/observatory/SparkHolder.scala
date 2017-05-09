package observatory

import org.apache.spark.SparkContext
import org.apache.spark.sql.DataFrame
import org.apache.spark.sql.types.StructType

/**
  * Created by honey.and.sw on 2017. 5. 5.
  */
object SparkHolder {
  import org.apache.spark.sql.SparkSession

  lazy val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Observatory")
      .config("spark.master", "local")
      .getOrCreate()

  lazy val context: SparkContext = spark.sparkContext

  def readCsv(path: String, schema: StructType): DataFrame = spark.read.schema(schema).csv(path)
}
