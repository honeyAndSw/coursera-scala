
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer.
    * R(Red), G(Green), B(Blue), A(Alpha, the amount of transparency) */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values.
    * (x, y) position is mapped to 'y * width + x' index of data. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))

    def apply(x: Int, y: Int): RGBA = data(y * width + x)

    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c

    override def toString() = (for {
      i <- 0 until data.length
    } yield {
      if ((i + 1) % width == 0) data(i) + "\n"
      else data(i) + "\t"
    }).mkString
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    val (xFrom, xTo) = (
      clamp(x - radius, 0, src.width - 1),
      clamp(x + radius, 0, src.width - 1))
    val (yFrom, yTo) = (
      clamp(y - radius, 0, src.height - 1),
      clamp(y + radius, 0, src.height - 1))

    var xIdx: Int = xFrom
    var yIdx: Int = yFrom
    var r, g, b, a, cnt = 0;

    while (xIdx <= xTo) {
      while (yIdx <= yTo) {
        val rgba = src(xIdx, yIdx)
        r = r + red(rgba)
        g = g + green(rgba)
        b = b + blue(rgba)
        a = a + alpha(rgba)

        cnt = cnt + 1
        yIdx = yIdx + 1
      }

      yIdx = yFrom
      xIdx = xIdx + 1
    }

    rgba(r/cnt, g/cnt, b/cnt, a/cnt)
  }

  def createBlurRangeUnits(range: Range, max: Int): List[(Int, Int)] = {
    if (range.isEmpty) Nil
    else {
      val head = range.head
      (head, Math.min(head + range.step, max)) :: createBlurRangeUnits(range.tail, max)
    }
  }

}
