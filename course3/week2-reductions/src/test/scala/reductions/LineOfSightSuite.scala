package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import java.util.concurrent.ForkJoinPool.ForkJoinWorkerThreadFactory

@RunWith(classOf[JUnitRunner]) 
class LineOfSightSuite extends FunSuite {
  import LineOfSight._
  test("lineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    lineOfSight(Array[Float](0f, 1f, 8f, 9f), output)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("parLineOfSight - threshold 3") {
    val output = new Array[Float](6)
    parLineOfSight(List[Float](0f, 9f, 18f, 27f, 36f, 45f).toArray, output, 3)
    assert(output.toList == List(0.0, 9.0, 9.0, 9.0, 9.0, 9.0))
  }

  test("parLineOfSight - threshold 2") {
    val output = new Array[Float](6)
    parLineOfSight(List(Float.NaN, 9f, 9f, 9f, 9f, 9f).toArray, output, 2)
    assert(output.toList == List(0.0, 9.0, 9.0, 9.0, 9.0, 9.0))
  }

  test("upsweepSequential should correctly handle the chunk 1 until 4 of an array of 4 elements") {
    val res = upsweepSequential(Array[Float](0f, 1f, 8f, 9f), 1, 4)
    assert(res == 4f)
  }

  test("upsweep should run correctly") {
    val res = upsweep(Array[Float](0f, 1f, 8f, 9f), 1, 4, 1)
    // assert(res == 4f)
  }

  test("downsweepSequential should correctly handle a 4 element array when the starting angle is zero") {
    val output = new Array[Float](4)
    downsweepSequential(Array[Float](0f, 1f, 8f, 9f), output, 0f, 1, 4)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

}

