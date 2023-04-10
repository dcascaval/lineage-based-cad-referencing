package elodin.geometry

import elodin.global.api.*

import elodin.opt.*
import elodin.opt.Queries.*
import elodin.opt.Operations.*
import elodin.opt.Operation.*
import elodin.opt.Kernel.*

trait Polygon extends Reference with Transformable:
  val points: DynList[Point]
  val edges: DynList[Edge]

  lazy val allGeometry = points.list ++ edges.list :+ this
  // TODO: is there a way we can potentially pre-cache bounding boxes
  //       for elements where we don't want to recompute them, in cases
  //       where we know the bounding box hasn't changed (e.g. a copy)
  lazy val bbox = BBox(this)

  def translate(vector: Point): Polygon =
    TransformPolygon[this.type](this, Transform.Translate(vector))

  def rotate(center: Point, theta: Double): Polygon =
    TransformPolygon[this.type](this, Transform.Rotate(center, theta))

  def rotateDeg(center: Point, deg: Double) =
    rotate(center, deg2rad(deg))

  def scale(origin: Point, factor: Double): Polygon =
    TransformPolygon[this.type](this, Transform.Scale(origin, factor))

  def scale(origin: Point, vector: Point): Polygon =
    TransformPolygon[this.type](this, Transform.ScaleNU(origin, vector))

  def ccw(): Boolean =
    val area2 = points.list.wrappedPairs
      .map((p, q) => (p.x * q.y) - (q.x * p.y))
      .fold(0.0)(_ + _)
    return area2 > 0

  type TransformResult = Polygon
  def transform(tfx: Transform): Polygon =
    TransformPolygon[this.type](this, tfx)

  def asRegion(): PolygonRegion =
    PolygonRegion(this)

  def display(): String =
    s"Polygon $this = \n" +
      s"\t$points\n" + s"\t$edges\n"

  /** Copy a polygon ensuring direct lineage from original to copy with the context of an operation. */
  def copy(): Polygon =
    val pts = this.points.list.map(_.copy())
    val lns = zip3(this.edges.list, pts, pts.rotate(1))
      .map((edge, start, end) => edge.copyWithPoints(start, end))
    val newPoly = BasePolygon(pts, Some(lns))
    directLineage(this -> newPoly)
    newPoly

  override lazy val containedElements =
    Set.from(points.list) ++ edges.list + this

final class RegularPolygon(center: Point, numSides: Double, radius: Double)
    extends Polygon
    with Operation:
  // TODO: polygons are a mix case. In order to meet the indexability rules we need
  // to allow selecting individually each of their points or edges; but the only distinguishing characteristic
  // of these is their index, which varies by numSides.
  // Solution: restrict numSides to a *constant* (cannot depend in ANY way on the program parameters)
  // and allow a subtype of sequence that is "indexable"; created by constant bound. We now do index, but it
  // is not safe since the argument's constancy is not checked. We will have an abstract pass
  // and then assign constant / non-constant tags to each expression, and change the polygon application
  // to a macro that verifies the expression constancy.
  val origin = Context.startOp(this)

  if numSides < 3 then error(s"Cannot make a Polygon with < 3 sides (got: $numSides)")

  val angleInterval = tau / numSides
  val p0 = center + Point(0, -radius)
  val points =
    (0 until Math.floor(numSides).toInt)
      .map(i => p0.rotate(center, angleInterval * i))
      .asDynList(true)
  val edges = points.list.wrappedPairs.map(Line(_, _)).asDynList(true)

  Context.endOp(this)

def Square(x: Double, y: Double, radius: Double) =
  Rectangle(pt(x, y), radius, radius)

object Rectangle:
  def orderRect(root: Point, width: Double, height: Double): Seq[Point] =
    val p0 = root
    val p1 = root + Point(width, 0)
    val p2 = root + Point(width, height)
    val p3 = root + Point(0, height)

    (height < 0, width < 0) match
      case (true, true)   => Seq(p2, p3, p0, p1)
      case (true, false)  => Seq(p3, p2, p1, p0)
      case (false, true)  => Seq(p1, p0, p3, p2)
      case (false, false) => Seq(p0, p1, p2, p3)

final class Rectangle(root: Point, width: Double, height: Double) extends Polygon with Operation:
  val origin = Context.startOp(this)

  val points = Rectangle.orderRect(root.duplicate(), width, height).asDynList(true)
  val Seq(bottomLeft, bottomRight, topRight, topLeft) = points.list

  val left = Line(topLeft, bottomLeft)
  val bottom = Line(bottomLeft, bottomRight)
  val right = Line(bottomRight, topRight)
  val top = Line(topRight, topLeft)
  val edges = Seq(bottom, right, top, left).asDynList(true)

  def center() =
    Point(root.x + width / 2, root.y + height / 2, orphan = true)

  Context.endOp(this)

  // TODO: we may want to put these back at some point; or introduce a version with customizable lineage (?).
  //
  // Situation A: we have a collection of points and rectangles made from them floating around,
  //              and we want to select the rectangles corresponding to some points; so we need this edge.
  //
  // Situation B: we position a rectangle relative to another rectangle's edge point, but we don't
  //              mean to directly "inherit" from it in the sense that we need this for indexing;
  //              instead we already have direct static access to the rectangle.
  //
  // In other words, whether we want the edge depends on what context the primitive is being created in.

  // arguments(root)
  // lineage(root -> allGeometry)

  override def toString() = s"Rectangle@${hashCode}"

def operationSummary(op: Operation with Reference, geo: Seq[Reference]) =
  println(s"Context = ${Context.currentGraph} | Subgraph = ${op.origin}")
  if !(geo.map(_.origin).forall(o => o == op.origin)) then
    println(s"Lost geometries = ${geo.filter(_.origin != op.origin)}")
// assert(false)

object Polygon:
  def apply(pts: Seq[Point]): BasePolygon = BasePolygon(pts)

/** A basic polygon constructed directly from points. Can only represent a closed region, and should not
  * be self-intersecting. Warning: this procedure mutates any passed-in lines to add the appropriate
  * reference back to this polygon. It should not be called with external lines.
  */
final class BasePolygon(pts: Seq[Point], lns: Option[Seq[Edge]] = None) extends Polygon:
  val origin =
    Context.currentGraph // NOT its own operation in this case, we may add an orphan version later
  val points = dynamicList(pts)
  val edges = dynamicList(lns.getOrElse(pts.wrappedPairs.map(Line(_, _))))

  override def toString() = s"BasePoly@${hashCode}"

final class Circle(c: Point, r: Double) extends Polygon with Operation:
  if r <= 0.0 then error(s"Cannot create negative-length circle [radius = $r]")
  val origin = Context.startOp(this)
  given Operation = this

  val curve = Arc(c, r, 0.0, 2.0 * math.Pi)
  val points = dynamicList(Seq(curve.start))
  val edges = dynamicList(Seq(curve))

  Context.endOp(this)

  def center() =
    Point(c.x, c.y, orphan = true)

  // Rationale: see Rectangle
  // arguments(c)
  // lineage(c -> allGeometry)

  override def toString() = s"Circle@${hashCode}"
