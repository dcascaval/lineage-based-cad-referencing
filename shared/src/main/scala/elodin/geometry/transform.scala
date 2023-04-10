package elodin.geometry

import elodin.global.api.*

enum Transform:
  case Translate(vector: Point)
  case Rotate(center: Point, radians: Double)
  case Scale(origin: Point, factor: Double)
  case ScaleNU(origin: Point, factor: Point)
  case Mirror(axis: Axis)

object Axis:
  val X = Axis(Point(0, 0), Point(1, 0))
  val Y = Axis(Point(0, 0), Point(0, 1))
  def apply(line: Edge) = new Axis(line.start, line.end)
  def apply(start: Point, end: Point) = new Axis(start, end)

case class Axis(start: Point, end: Point) extends Transformable:
  val vec = end - start
  val l2 = vec.lengthSquared
  if l2 < 1e-12 then throw new Exception(s"Degenerate axis: $start -> $end")

  val s = (start.x, start.y)
  val v = (vec.x, vec.y)

  def closestPoint(point: (Double, Double)): (Double, Double) =
    val t = (point - s).dot(v) / l2
    s + (v * t)

  def mirror(point: Point) =
    val p = (point.x, point.y)
    val cp = closestPoint(p)
    val (dx, dy) = ((cp - p) * 2.0)
    Point(point.x + dx, point.y + dy)

  type TransformResult = Axis
  def transform(t: Transform) =
    val transformedSegment = Line(start, end).transform(t)
    Axis(transformedSegment.start, transformedSegment.end)

trait Transformable:
  type TransformResult
  def transform(t: Transform): TransformResult
