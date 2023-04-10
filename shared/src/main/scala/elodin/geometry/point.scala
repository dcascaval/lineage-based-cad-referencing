package elodin.geometry

import elodin.global.api.*
import elodin.opt.{Reference, Operation, OperationGraph, Context}
import Operation.*

object Point:
  def unapply(pt: Point): (Double, Double) = (pt.x, pt.y)
  def fromPolar(r: Double, theta: Double) =
    Point(r * math.cos(theta), r * math.sin(theta))

// Most of the operators are inlined, allowing SROA or similar
// optimizations to happen in intermediate expressions even
// when the point leaves the method.
final class Point(val x: Double, val y: Double, orphan: Boolean = false)
    extends Reference
    with Transformable
    with Operation:

  val origin = if orphan then Context.unitOp(this) else Context.currentGraph

  inline def +(other: Point) = Point(x + other.x, y + other.y)
  inline def -(other: Point) = Point(x - other.x, y - other.y)
  inline def *(other: Double) = Point(x * other, y * other)
  inline def /(other: Double) = Point(x / other, y / other)
  inline def dot(other: Point): Double = x * other.x + y * other.y

  inline def to2 = (x, y)

  inline def dist(other: Point) =
    val dx = other.x - x
    val dy = other.y - y
    (dx * dx) + (dy * dy)

  inline def rotate(theta: Double): Point =
    val sint = math.sin(theta)
    val cost = math.cos(theta)
    Point(x * cost - y * sint, x * sint + y * cost)

  inline def rotate(center: Point, theta: Double): Point =
    val r = (center.x, center.y) + ((x - center.x, y - center.y).rotate(theta))
    Point(r.x, r.y)

  inline def spread(other: Point): Double =
    val vu = this dot other
    val uu = other dot other
    val vv = this dot this
    1.0 - ((vu * vu) / (uu * vv))

  lazy val length = math.sqrt(lengthSquared)
  lazy val lengthSquared = (x * x) + (y * y)

  inline def withLength(newLength: Double) =
    val ratio = newLength / length
    Point(ratio * x, ratio * y)

  inline def scale(origin: Point, factor: Double) =
    val p = (this.x, this.y)
    val o = (origin.x, origin.y)
    val r = ((p - o) * factor) + o
    Point(r.x, r.y)

  inline def scaleNU(origin: Point, factor: Point) =
    val p = (this.x, this.y)
    val o = (origin.x, origin.y)
    val c = p - o
    val r = (c.x * c.x, c.y * factor.y) + o
    Point(r.x, r.y)

  inline def crossZ(other: Point) =
    (x * other.y) - (y * other.x)

  inline def normal() = Point(-y, x)

  lazy val rawSVG = s"$x $y"
  lazy val toSVG = s"${x.toInt} ${y.toInt}"

  /** Copy but keep a lineage pointer (for mutable objects) */
  def copy() =
    val result = Point(x, y)
    directLineage(this -> result)
    result

  /** Copy without tracing lineage */
  def duplicate() = Point(x, y)

  override def toString() =
    inline def fmtNum(inline n: Double) =
      f"${n}%.4f".stripSuffix(".0000")
    f"Point(${fmtNum(x)},${fmtNum(y)})"

  type TransformResult = Point
  def transform(tfx: Transform) =
    transformInternal(tfx, orphan)

  def transformInternal(tfx: Transform, orphan: Boolean = false) =
    val point = this
    import Transform.*
    val resultPoint = tfx match
      case Translate(vec)      => point + vec
      case Rotate(origin, deg) => point.rotate(origin, deg)
      case Scale(origin, f)    => point.scale(origin, f)
      case ScaleNU(origin, f)  => point.scaleNU(origin, f)
      case Mirror(axis)        => axis.mirror(point)
    if !orphan then resultPoint
    else
      val op = new Point(resultPoint.x, resultPoint.y, orphan = true)
      op.arguments(point)
      directLineage(point -> op)
      op
