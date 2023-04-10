package elodin.geometry

import elodin.opt.{Reference, Operation, OperationGraph, Context}

case class BBox(minX: Double, maxX: Double, minY: Double, maxY: Double):
  val origin = Context.currentGraph

  lazy val center: Point = Point((minX + maxX) / 2, (minY + maxY) / 2)
  inline def inInterval(a: Double, min: Double, max: Double) =
    min <= a && a <= max

  inline def containsPoint(x: Double, y: Double) =
    inInterval(x, minX, maxX) && inInterval(y, minY, maxY)

  inline def intervalsOverlap(minA: Double, maxA: Double, minB: Double, maxB: Double) =
    inInterval(minB, minA, maxA) || inInterval(minA, minB, maxB)

  def intersects(other: BBox) =
    intervalsOverlap(minX, maxX, other.minX, other.maxX) &&
      intervalsOverlap(minY, maxY, other.minY, other.maxY)

  def expand(other: BBox) =
    BBox(
      math.min(minX, other.minX),
      math.max(maxX, other.maxX),
      math.min(minY, other.minY),
      math.max(maxY, other.maxY)
    )

  def width = math.abs(maxX - minX)
  def height = math.abs(maxY - minY)

object BBox:
  // If we have a point set or something
  def computeBox(pts: Seq[Point]) =
    val DMIN: Double = Double.MinValue
    val DMAX: Double = Double.MaxValue
    pts.foldLeft(
      (DMAX, DMIN, DMAX, DMIN)
    ) { case ((a, b, c, d), pt) =>
      val (x, y) = (pt.x, pt.y)
      (math.min(a, x), math.max(b, x), math.min(c, y), math.max(d, y))
    }

  def apply(poly: Polygon): BBox =
    poly.edges.list match
      case Seq() => elodin.opt.Queries.error("Cannot compute bounding box with no edges!")
      case Seq(first, rest*) =>
        rest.foldLeft(first.bounds)((bounds, next) => bounds.expand(next.bounds))

  def apply(edge: Line): BBox =
    val minX = math.min(edge.start.x, edge.end.x)
    val maxX = math.max(edge.start.x, edge.end.x)
    val minY = math.min(edge.start.y, edge.end.y)
    val maxY = math.max(edge.start.y, edge.end.y)
    BBox(minX, maxX, minY, maxY)

  def apply(a: Arc): BBox = // Very rough approximation: give the bbox of the circle.
    val x = a.center.x
    val y = a.center.y
    val r = a.radius
    BBox(x - r, x + r, y - r, y + r)
