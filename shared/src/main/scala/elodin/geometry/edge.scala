package elodin.geometry

import collection.mutable.Buffer

import elodin.global.api.*

import elodin.opt.{Reference, Operation, OperationGraph, Context}
import elodin.opt.Operations.*
import elodin.opt.Operation.*
import elodin.opt.Queries.*
import elodin.opt.Kernel.*

/* An edge can be either a line or an arc. */
sealed trait Edge extends Reference with Transformable:
  var start: Point
  var end: Point

  lazy val startTangent: Point
  lazy val endTangent: Point
  lazy val flipped: this.type
  lazy val length: Double
  lazy val bounds: BBox

  /** Evaluate a point at some parameter on this curve. */
  def eval(t: Double): Point

  /** Move this edge in the 2D plane */
  type TransformResult = this.type
  def transform(tfx: Transform): this.type

  /** Find a point offset along this edge by some distance. */
  def offsetPoint(length: Double, reverse: Boolean): Point

  /** Intersect this edge with another. */
  def intersect(other: Edge): Buffer[IntersectionResult]

  /** Create a new edge offset from this one by some distance. */
  def offset(distance: Double): this.type

  /** Offset some point along the normal of the closest point on the curve to it. */
  def offsetNormal(pt: Point, distance: Double): Point

  /** Truncate the edge according to some parameter interval. */
  def trimParameters(startParameter: Double, endParameter: Double): this.type

  /** Truncate this edge at some point. If reverse, keep the back half. */
  def trimSegment(offsetPoint: Point, reverse: Boolean): this.type

  override lazy val containedElements =
    Set(start, end, this)

  def withPoints(start: Point, end: Point) =
    this.start = start; this.end = end; this

  def copy(): this.type

  def copyWithPoints(start: Point, end: Point): this.type =
    val result = this.copy().withPoints(start, end)
    directLineage(this -> result)
    result.asInstanceOf[this.type]

/** A basic line, which depends on two points. If ancestors are passed explicitly, those become the
  * parents, otherwise by default it resolves to the endpoints.
  */
final class Line(var start: Point, var end: Point, orphan: Boolean = false) extends Edge with Operation:

  val origin = if orphan then {
    val g = Context.unitOp(this, start, end)
    lineage(Seq(start, end) -> this)
    start = start.copy()
    end = end.copy()
    g
  } else Context.currentGraph

  // Lines can be evaluated at certain parameters to create points which depend on them.
  def eval(t: Double): Point = start + ((end - start) * t)

  lazy val flipped =
    Line(end, start)
      .asInstanceOf[this.type]

  def transform(tfx: Transform) =
    Line(start.transform(tfx), end.transform(tfx))
      .asInstanceOf[this.type]

  def intersect(other: Edge) =
    if !bounds.intersects(other.bounds) then Buffer()
    else
      other match
        case b: Line => intersectSegments(start, end, other.start, other.end)
        case b: Arc  => intersectSegmentArc(start, end, b.center, b.radius, b.theta0, b.theta1)

  def offsetPoint(offsetLength: Double, reverse: Boolean) =
    if !reverse then start + (end - start).withLength(offsetLength)
    else end + (start - end).withLength(offsetLength)

  def trimParameters(startParameter: Double, endParameter: Double): this.type =
    Line(eval(startParameter), eval(endParameter)).asInstanceOf[this.type]

  def trimSegment(offsetPoint: Point, reverse: Boolean): this.type =
    (if !reverse then Line(start, offsetPoint)
     else Line(offsetPoint, end)).asInstanceOf[this.type]

  lazy val vector = end - start
  lazy val startTangent = vector.withLength(1.0)
  lazy val endTangent = startTangent

  lazy val normal = vector.normal().withLength(1.0)
  lazy val length = vector.length
  lazy val bounds = BBox(this)

  def offset(distance: Double) =
    val offsetVector = normal * distance
    val s = start + offsetVector
    val e = end + offsetVector
    Line(s, e).asInstanceOf[this.type]

  def offsetNormal(pt: Point, d: Double) =
    pt + (normal * d)

  def copy() = Line(start.duplicate(), end.duplicate()).as[this.type]

  override def toString() = s"Line($start,$end)"

final class Arc(
    val center: Point,
    val radius: Double,
    val theta0: Double,
    val theta1: Double
) extends Edge:
  val origin = Context.currentGraph
  // Right now we aren't making any assumptions about how arcs look in practice
  // The interval theta0 <-> theta1 denotes direction. If
  // - theta0 > theta1, the arc goes clockwise.
  // - theta1 > theta0, the arc goes counter-clockwise.

  var start: Point = Point.fromPolar(radius, theta0) + center
  var end: Point = Point.fromPolar(radius, theta1) + center

  lazy val length: Double = radius * math.abs(theta0 - theta1) // Already multiplied by 2pi
  lazy val bounds: BBox = BBox(this)

  lazy val flipped =
    Arc(center, radius, theta1, theta0)
      .asInstanceOf[this.type]

  def eval(t: Double) =
    val angle = theta0 + t * (theta1 - theta0)
    Point.fromPolar(radius, angle) + center

  def offsetPoint(offsetLength: Double, reverse: Boolean) =
    val t = offsetLength / length
    eval(if reverse then 1.0 - t else t)

  private inline def orientTangent(t: Point) =
    if theta0 < theta1 then t else t * -1

  // TODO: Direction from theta0 to theta1 will sometimes dictate this be flipped.
  lazy val startTangent: Point =
    orientTangent((start - center).normal().withLength(1.0))
  lazy val endTangent: Point =
    orientTangent((end - center).normal().withLength(1.0))

  def offset(distance: Double) =
    val d = if theta0 < theta1 then -distance else distance
    Arc(center, radius + d, theta0, theta1).asInstanceOf[this.type]

  /** INVARIANT: arcs should not be more than 1 entire circle large. */
  private def boundThetas(start: Double, end: Double): (Double, Double) =
    if start - end >= tau then (start - tau, end)
    else if (end - start) >= tau then (start, end - tau)
    else (start, end)

  private inline def between(t: Double, a: Double, b: Double): Boolean =
    (a <= t && t <= b) ||
      (b <= t && t <= a)

  def thetaFromParameter(t: Double) =
    theta0 + t * (theta1 - theta0)

  def trimParameters(startParameter: Double, endParameter: Double): this.type =
    val t0 = thetaFromParameter(startParameter)
    val t1 = thetaFromParameter(endParameter)
    Arc(center, radius, t0, t1).asInstanceOf[this.type]

  def trimSegment(offsetPoint: Point, reverse: Boolean): this.type =
    var t = (offsetPoint - center).polarTheta
    if !between(t, theta0, theta1) then //
      if t < theta0 then t += tau else t -= tau

    val result: Arc =
      val (s, e) = if !reverse then boundThetas(theta0, t) else boundThetas(t, theta1)
      Arc(center, radius, s, e)
    result.asInstanceOf[this.type]

  def offsetNormal(pt: Point, distance: Double) =
    val d = if theta0 < theta1 then -distance else distance
    pt + (pt - center).withLength(d)

  def intersect(other: Edge) =
    if !bounds.intersects(other.bounds) then Buffer()
    else
      other match
        case o: Arc =>
          intersectArcArc(center, radius, theta0, theta1, o.center, o.radius, o.theta0, o.theta1)
        case l: Line =>
          intersectSegmentArc(l.start, l.end, center, radius, theta0, theta1).map(_.flip())

  def transform(tfx: Transform) =
    inline def incrementThetas(theta0: Double, theta1: Double, deg: Double) =
      // Preserve full circles
      if (math.abs(math.abs(theta0 - theta1) - tau) <= TOLERANCE) then (theta0, theta1)
      else
        var (r0, r1) = (theta0 + deg, theta1 + deg)
        if r0 > tau || r1 > tau then (r0 - tau, r1 - tau)
        else if r0 < -tau || r1 < -tau then (r0 + tau, r1 + tau)
        else (r0, r1)

    import Transform.*
    val result = tfx match
      case Translate(vec) => Arc(center.transform(tfx), radius, theta0, theta1)
      case Rotate(origin, deg) =>
        val (t0, t1) = incrementThetas(theta0, theta1, deg)
        val result = Arc(center.transform(tfx), radius, t0, t1)
        result
      case Scale(origin, factor) =>
        Arc(center.transform(tfx), radius * factor, theta0, theta1)
      case Mirror(axis) =>
        val cen = center.transform(tfx)
        // Mirror the thetas by mirroring points and re-solving for bounded thetas
        val (ns, ne) = (start.transform(tfx), end.transform(tfx))
        val (nt0, nt1) = ((ns - cen).toPolar.polarTheta, (ne - cen).toPolar.polarTheta)
        val (it0, it1) = incrementThetas(nt0, nt1, 0)
        // BUGGY: negative thetas; backwards arcs
        Arc(cen, radius, it0, it1)
      case s: ScaleNU => error(s"Cannot scale arc non-uniformly")
    result.asInstanceOf[this.type]

  def copy() =
    Arc(center.duplicate(), radius, theta0, theta1).as[this.type]

  override def toString() =
    s"Arc($center, $radius, [$theta0 to $theta1], $start, $end)"

object Arc:
  def thetaFromPoint(center: Point, point: Point): Double =
    (point - center).polarTheta
