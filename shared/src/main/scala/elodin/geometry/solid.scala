package elodin.geometry

import elodin.global.api.*

import elodin.opt.{Reference, Operation, OperationGraph, Context, debug_ref, containedElements}
import elodin.opt.Queries.*
import elodin.opt.Operations.*
import elodin.opt.Operation.*
import elodin.opt.Kernel.*

sealed trait Transform3D
case class Translate(x: Double, y: Double, z: Double) extends Transform3D
case class Rotate(axis: Axis3D, angle: Double) extends Transform3D

case class Axis3D(s: (Double, Double, Double), e: (Double, Double, Double)):
  val v = e - s
  val l2 = v.lengthSquared
  val normalVector = v.withLength(1.0)

  def closestPoint(point: (Double, Double, Double)): (Double, Double, Double) =
    val t = (point - s).dot(v) / l2
    s + (v * t)

object Axis3D:
  val X = Axis3D((0, 0, 0), (1, 0, 0))
  val Y = Axis3D((0, 0, 0), (0, 1, 0))
  val Z = Axis3D((0, 0, 0), (0, 0, 1))

  def ZAt(pt: Point) =
    Axis3D((pt.x, pt.y, 0.0), (pt.x, pt.y, 1.0))

case class Plane3D(origin: Vertex3D, face: Face3D)

object Transform3D:
  def Move(from: Vertex3D, to: Vertex3D): Translate =
    val (x, y, z) = to.physicalPoint - from.physicalPoint
    Translate(x, y, z)

  def Move(from: Point, to: Point): Translate =
    val (x, y) = (to - from).to2
    Translate(x, y, 0)

  def RotateAxis(p1: Vertex3D, p2: Vertex3D, angle: Double): Rotate =
    Rotate(Axis3D(p1.physicalPoint, p2.physicalPoint), angle)

  def RotateAxis(edge: Edge3D, angle: Double): Rotate =
    Rotate(Axis3D(edge.start.physicalPoint, edge.end.physicalPoint), angle)

// Represents an element that can be transformed in 3D. This trait does no
// math, it merely keeps track of the stack of transforms that should then
// be evaluated to return transformation matrices which will be used to position
// the element in 3D during the rendering pass
sealed trait Transformable3D extends Reference {
  val transforms: Seq[Transform3D]
  // Add a transform to the stack
  def withTransform(transform: Transform3D): this.type
}

// Invariant: for all of these, the element must be on the parent (it will be treated as such!)

sealed trait Face3D extends Transformable3D:
  val parent: Solid
  val origin = parent.origin

/** All vertical faces are extruded or revolved from an edge */
final class VerticalFace(val edge: Edge, val parent: Solid, val transforms: Seq[Transform3D] = Seq())
    extends Face3D:
  lineage(edge -> this)
  def withTransform(t: Transform3D) = VerticalFace(edge, parent, transforms :+ t).as[this.type]

  def evalU(t: Double): Edge3D = VerticalEdge(edge.eval(t), parent, transforms)
  def evalV(t: Double): Edge3D = HorizontalEdge(edge, parent, t, transforms)
  def eval(tU: Double, tV: Double): Vertex3D = SolidVertex(edge.eval(tU), parent, tV, transforms)

/** All horizontal faces are either on the base or on the top of the solid. */
final class HorizontalFace(
    val parent: Solid,
    val parameter: Double,
    val transforms: Seq[Transform3D] = Seq()
) extends Face3D:
  def withTransform(t: Transform3D) = HorizontalFace(parent, parameter, transforms :+ t).as[this.type]

sealed trait Edge3D extends Transformable3D:
  val parent: Solid
  val origin = parent.origin

  lazy val start: Vertex3D
  lazy val end: Vertex3D
  def eval(t: Double): Vertex3D

/** All vertical edges are extruded or revolved from a point */
final class VerticalEdge(val point: Point, val parent: Solid, val transforms: Seq[Transform3D] = Seq())
    extends Edge3D:
  lineage(point -> this)

  lazy val start = SolidVertex(point, parent, 0.0, transforms)
  lazy val end = SolidVertex(point, parent, 1.0, transforms)

  def withTransform(t: Transform3D) =
    VerticalEdge(point, parent, transforms :+ t).as[this.type]

  def eval(t: Double) =
    val result = SolidVertex(point, parent, t, transforms)
    directLineage(this -> result)
    result

/** All horizontal edges are either on the original polygon or on the top of the solid. */
final class HorizontalEdge(
    val edge: Edge,
    val parent: Solid,
    val parameter: Double,
    val transforms: Seq[Transform3D] = Seq()
) extends Edge3D:
  directLineage(edge -> this)

  lazy val start = SolidVertex(edge.start, parent, parameter, transforms)
  lazy val end = SolidVertex(edge.end, parent, parameter, transforms)

  def withTransform(t: Transform3D) =
    HorizontalEdge(edge, parent, parameter, transforms :+ t).as[this.type]

  def eval(t: Double) =
    val result = SolidVertex(edge.eval(t), parent, parameter, transforms)
    directLineage(this -> result)
    result

trait Vertex3D extends Transformable3D:
  val parent: Solid
  val origin = parent.origin

  val point: Point
  val parameter: Double
  def inSolid(): (Double, Double, Double)

  lazy val physicalPoint: (Double, Double, Double) =
    var current = inSolid()
    for t <- transforms do
      t match
        case Translate(x, y, z) =>
          current = current + (x, y, z)
        case Rotate(axis, theta) =>
          val sinT = Math.sin(-theta); val cosT = Math.cos(-theta)
          val p = current
          val axisPt = axis.closestPoint(p)
          val revolveVec = (p - axisPt)
          val cosRot = revolveVec * cosT
          val normal = revolveVec.cross(axis.v).withLength(revolveVec.length)
          val sinRot = normal * sinT
          // val (cl, sl) = (cosRot.length, sinRot.length)
          // println(s"$p => ($axisPt on ${axis.s}->${axis.e}) | cos = $cosRot ($cl) | sin = $sinRot ($cl)")
          current = axisPt + cosRot + sinRot
    current

final class SolidVertex(
    val point: Point,
    val parent: Solid,
    val parameter: Double,
    val transforms: Seq[Transform3D] = Seq()
) extends Vertex3D:
  directLineage(point -> this)
  def withTransform(t: Transform3D) =
    SolidVertex(point, parent, parameter, transforms :+ t).as[this.type]
  def inSolid() =
    parent.pointInSolid(parameter)(point.x, point.y)

sealed trait Solid extends Transformable3D:
  val base: Region2D

  lazy val basePoints: DynList[Vertex3D] = base.points.map(SolidVertex(_, this, 0.0, transforms))
  lazy val outerPoints: DynList[Vertex3D] = base.points.map(SolidVertex(_, this, 1.0, transforms))
  lazy val verticalEdges: DynList[Edge3D] = base.points.map(VerticalEdge(_, this, transforms))
  lazy val baseEdges: DynList[Edge3D] = base.edges.map(HorizontalEdge(_, this, 0.0, transforms))
  lazy val outerEdges: DynList[Edge3D] = base.edges.map(HorizontalEdge(_, this, 1.0, transforms))
  lazy val verticalFaces: DynList[Face3D] = base.edges.map(VerticalFace(_, this, transforms))

  lazy val baseFace: Face3D = HorizontalFace(this, 0.0, transforms)
  lazy val outerFace: Face3D = HorizontalFace(this, 1.0, transforms)

  // def sliceAt(parameter: Double): Face3D

  lazy val points = basePoints.union(outerPoints)
  lazy val edges = DynList(baseEdges.list ++ outerEdges.list ++ verticalEdges.list)
  lazy val faces = DynList(verticalFaces.list ++ Seq(baseFace, outerFace))

  /** Maps a point from the base polygon into the 3D volume defined by the solid. This is well-defined
    * given only a single normalized depth parameter. The result of this function is *prior* to the
    * application of any transformations in the stack.
    */
  def pointInSolid(parameter: Double): (Double, Double) => (Double, Double, Double)

final class Extrusion(
    val base: Region2D,
    val length: Double,
    val transforms: Seq[Transform3D] = Seq()
) extends Solid
    with Operation:

  val origin = Context.startOp(this)
  Context.endOp(this)
  def withTransform(t: Transform3D) =
    Extrusion(base, length, transforms :+ t).as[this.type]

  def pointInSolid(t: Double): (Double, Double) => (Double, Double, Double) =
    (px: Double, py: Double) => (px, py, t * length)

final class Revolve(
    val base: Region2D,
    val axis: Axis,
    val angleStart: Double = 0.0,
    val angleEnd: Double = tau,
    val transforms: Seq[Transform3D] = Seq()
) extends Solid
    with Operation:

  val origin = Context.startOp(this)
  Context.endOp(this)

  def withTransform(t: Transform3D) =
    Revolve(base, axis, angleStart, angleEnd, transforms :+ t).as[this.type]

  // This can be used to do rotations around an axis
  def pointInSolid(t: Double): (Double, Double) => (Double, Double, Double) =
    // Could be saved if we knew we were evaluating all at the same parameter.
    val theta = angleStart + ((angleEnd - angleStart) * t)
    val sinT = Math.sin(theta); val cosT = Math.cos(theta)
    (px: Double, py: Double) =>
      val p = (px, py)
      val axisPt = axis.closestPoint(p)
      val revolveVec = (p - axisPt)
      val newXY = axisPt + (revolveVec * cosT)
      val newZ = revolveVec.length * sinT
      (newXY.x, newXY.y, newZ)
