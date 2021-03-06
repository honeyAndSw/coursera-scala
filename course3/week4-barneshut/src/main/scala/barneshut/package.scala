import common._
import barneshut.conctrees._

package object barneshut {

  class Boundaries {
    var minX = Float.MaxValue

    var minY = Float.MaxValue

    var maxX = Float.MinValue

    var maxY = Float.MinValue

    def width = maxX - minX

    def height = maxY - minY

    def size = math.max(width, height)

    def centerX = minX + width / 2

    def centerY = minY + height / 2

    override def toString = s"Boundaries($minX, $minY, $maxX, $maxY)"
  }

  sealed abstract class Quad {
    /** the center of mass of the bodies */
    def massX: Float
    def massY: Float

    /** the total mass of bodies */
    def mass: Float

    /** the coordinates of the center of the cell */
    def centerX: Float
    def centerY: Float

    /** the length of the size of the cell */
    def size: Float

    /** the total number of bodies */
    def total: Int

    def insert(b: Body): Quad
  }

  case class Empty(centerX: Float, centerY: Float, size: Float)
  extends Quad {
    def massX: Float = centerX
    def massY: Float = centerY
    def mass: Float = 0
    def total: Int = 0

    /**
      * Insert a body to Empty Quad.
      * It simply creates a new Leaf with single body.
      */
    def insert(b: Body): Quad = new Leaf(centerX, centerY, size, Seq[Body](b))
  }

  case class Fork(nw: Quad, ne: Quad, sw: Quad, se: Quad)
  extends Quad {
    /** center of the Fork = the lower right corner of the nw */
    val centerX: Float = nw.centerX + (nw.size / 2)
    val centerY: Float = nw.centerY + (nw.size / 2)

    val size: Float = nw.size + ne.size + sw.size + se.size

    val total: Int = nw.total + ne.total + sw.total + se.total

    val mass: Float = nw.mass + ne.mass + sw.mass + se.mass

    def foldLeftChildren(op: (Float, Quad) => Float): Float = {
      List(nw, ne, sw, se).foldLeft(0f)(op)
    }

    /**
      * massX = (m_B * x_B + m_C * x_C + m_D * x_D + m_E * x_D) / mass
      * I thought x_B should be centerX of itself, but x_B should be massX to pass test cases.
      */
    val massX: Float = if (total == 0) centerX else {
      foldLeftChildren((f, q) => f + (q.mass * q.massX)) / mass
    }

    /**
      * massX = (m_B * y_B + m_C * y_C + m_D * y_D + m_E * y_D) / mass
      */
    val massY: Float = if (total == 0) centerY else {
      foldLeftChildren((f, q) => f + (q.mass * q.massY)) / mass
    }

    /**
      * Update the respective child and create a new Fork.
      */
    def insert(b: Body): Fork = {
      if (b.x < centerX) { // nw or sw
        if (b.y < centerY) {
          new Fork(nw.insert(b), ne, sw, se) // nw
        } else {
          new Fork(nw, ne, sw.insert(b), se) // sw
        }
      } else { // ne or se
        if (b.y < centerY) {
          new Fork(nw, ne.insert(b), sw, se) // ne
        } else {
          new Fork(nw, ne, sw, se.insert(b)) // se
        }
      }
    }
  }

  case class Leaf(centerX: Float, centerY: Float, size: Float, bodies: Seq[Body])
  extends Quad {
    def foldLeftBody(op: (Float, Body) => Float): Float = {
      bodies.foldLeft(0f)(op)
    }

    val mass: Float = foldLeftBody((f, body) => f + body.mass)
    val massX: Float = foldLeftBody((f, body) => f + body.mass * body.x) / mass
    val massY: Float = foldLeftBody((f, body) => f + body.mass * body.y) / mass

    val total: Int = bodies.length

    /**
      * Only create a Fork when size is greater than minimumSize.
      */
    def insert(b: Body): Quad = if (size > minimumSize) {
      forkQuad(b)
    } else {
      new Leaf(centerX, centerY, size, b +: bodies)
    }

    /**
      * insert helper function
      */
    def forkQuad(b: Body): Quad = {
      val half = size / 2
      val quarter = size / 4

      var fork: Quad = new Fork(
        new Empty(centerX - quarter, centerY - quarter, half), // nw
        new Empty(centerX + quarter, centerY - quarter, half), // ne
        new Empty(centerX - quarter, centerY + quarter, half), // sw
        new Empty(centerX + quarter, centerY + quarter, half)) // se

      for (body <- b +: bodies) yield {
        fork = fork insert body
      }

      fork
    }
  }

  def minimumSize = 0.00001f

  def gee: Float = 100.0f

  def delta: Float = 0.01f

  def theta = 0.5f

  def eliminationThreshold = 0.5f

  def force(m1: Float, m2: Float, dist: Float): Float = gee * m1 * m2 / (dist * dist)

  def distance(x0: Float, y0: Float, x1: Float, y1: Float): Float = {
    math.sqrt((x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0)).toFloat
  }

  class Body(val mass: Float, val x: Float, val y: Float, val xspeed: Float, val yspeed: Float) {

    def updated(quad: Quad): Body = {
      var netforcex = 0.0f
      var netforcey = 0.0f

      def addForce(thatMass: Float, thatMassX: Float, thatMassY: Float): Unit = {
        val dist = distance(thatMassX, thatMassY, x, y)
        /* If the distance is smaller than 1f, we enter the realm of close
         * body interactions. Since we do not model them in this simplistic
         * implementation, bodies at extreme proximities get a huge acceleration,
         * and are catapulted from each other's gravitational pull at extreme
         * velocities (something like this:
         * http://en.wikipedia.org/wiki/Interplanetary_spaceflight#Gravitational_slingshot).
         * To decrease the effect of this gravitational slingshot, as a very
         * simple approximation, we ignore gravity at extreme proximities.
         */
        if (dist > 1f) {
          val dforce = force(mass, thatMass, dist)
          val xn = (thatMassX - x) / dist
          val yn = (thatMassY - y) / dist
          val dforcex = dforce * xn
          val dforcey = dforce * yn
          netforcex += dforcex
          netforcey += dforcey
        }
      }

      def traverse(quad: Quad): Unit = (quad: Quad) match {
        case Empty(_, _, _) =>
          // no force
        case Leaf(_, _, _, bodies) =>
          for (body <- bodies) yield addForce(body.mass, body.x, body.y)
        case Fork(nw, ne, sw, se) => {
          // see if node is far enough from the body, or recursion is needed
          val dist = distance(quad.centerX, quad.centerY, x, y)
          if ((quad.size / dist) < theta) {
            // approximate a cluster of bodies with a single point
            addForce(quad.mass, quad.massX, quad.massY)
          } else {
            for (q <- List(nw, ne, sw, se)) yield {
              traverse(q)
            }
          }
        }
      }

      traverse(quad)

      val nx = x + xspeed * delta
      val ny = y + yspeed * delta
      val nxspeed = xspeed + netforcex / mass * delta
      val nyspeed = yspeed + netforcey / mass * delta

      new Body(mass, nx, ny, nxspeed, nyspeed)
    }

  }

  val SECTOR_PRECISION = 8

  class SectorMatrix(val boundaries: Boundaries, val sectorPrecision: Int) {

    val sectorSize = boundaries.size / sectorPrecision

    val matrix = new Array[ConcBuffer[Body]](sectorPrecision * sectorPrecision)
    /** Each entry contains a ConcBuffer[Body] object. */
    for (i <- 0 until matrix.length) matrix(i) = new ConcBuffer

    def +=(b: Body): SectorMatrix = {
      val xIdx = ((b.x - boundaries.minX) / sectorSize) toInt
      val yIdx = ((b.y - boundaries.minY) / sectorSize) toInt

      if (xIdx < sectorPrecision && yIdx < sectorPrecision) {
        // Body is inside of the Boundaries
        this(xIdx, yIdx) += b
      } else if (xIdx >= sectorPrecision) {
        this(sectorPrecision - 1, yIdx) += b
      } else {
        this(xIdx, sectorPrecision - 1) += b
      }

      this
    }

    def apply(x: Int, y: Int) = matrix(y * sectorPrecision + x)

    def combine(that: SectorMatrix): SectorMatrix = {
      val sector = new SectorMatrix(boundaries, sectorPrecision)

      for (i <- 0 until matrix.length) {
        sector.matrix(i) = this.matrix(i) combine that.matrix(i)
      }

      sector
    }

    def toQuad(parallelism: Int): Quad = {
      def BALANCING_FACTOR = 4
      def quad(x: Int, y: Int, span: Int, achievedParallelism: Int): Quad = {
        if (span == 1) {
          val sectorSize = boundaries.size / sectorPrecision
          val centerX = boundaries.minX + x * sectorSize + sectorSize / 2
          val centerY = boundaries.minY + y * sectorSize + sectorSize / 2
          var emptyQuad: Quad = Empty(centerX, centerY, sectorSize)
          val sectorBodies = this(x, y)
          sectorBodies.foldLeft(emptyQuad)(_ insert _)
        } else {
          val nspan = span / 2
          val nAchievedParallelism = achievedParallelism * 4
          val (nw, ne, sw, se) =
            if (parallelism > 1 && achievedParallelism < parallelism * BALANCING_FACTOR) parallel(
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            ) else (
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            )
          Fork(nw, ne, sw, se)
        }
      }

      quad(0, 0, sectorPrecision, 1)
    }

    override def toString = s"SectorMatrix(#bodies: ${matrix.map(_.size).sum})"
  }

  class TimeStatistics {
    private val timeMap = collection.mutable.Map[String, (Double, Int)]()

    def clear() = timeMap.clear()

    def timed[T](title: String)(body: =>T): T = {
      var res: T = null.asInstanceOf[T]
      val totalTime = /*measure*/ {
        val startTime = System.currentTimeMillis()
        res = body
        (System.currentTimeMillis() - startTime)
      }

      timeMap.get(title) match {
        case Some((total, num)) => timeMap(title) = (total + totalTime, num + 1)
        case None => timeMap(title) = (0.0, 0)
      }

      println(s"$title: ${totalTime} ms; avg: ${timeMap(title)._1 / timeMap(title)._2}")
      res
    }

    override def toString = {
      timeMap map {
        case (k, (total, num)) => k + ": " + (total / num * 100).toInt / 100.0 + " ms"
      } mkString("\n")
    }
  }
}
