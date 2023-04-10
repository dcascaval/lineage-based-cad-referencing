package elodin.geometry

import elodin.global.api.*

import elodin.opt.{Reference, Operation, OperationGraph, Context, debug_ref, containedElements}
import elodin.opt.Queries.*
import elodin.opt.Operations.*
import elodin.opt.Kernel.*

/** Representation of a 2D area with potentially multiple polygons. */
trait Region2D extends Reference with Transformable:
  /** All polygons, positive or negative, in this region. */
  val polygons: DynList[Polygon]

  /** Positive polygons have area. */
  val positivePolygons: DynList[Polygon]

  /** Negative polygons are cutting polygons that remove area. */
  val negativePolygons: DynList[Polygon]

  // private def emptyList[T <: Reference] = DynList.empty[T](this.as[Operation & Reference])
  lazy val edges = DynList(polygons.list.flatMap(_.edges.list))
  lazy val points = DynList(polygons.list.flatMap(_.points.list))

  lazy val allGeometry = edges.list ++ points.list ++ polygons.list ++ Seq(this)

  type TransformResult = Region2D
  def transform(t: Transform): Region2D = transformRegion(this, t)

  override lazy val containedElements =
    polygons.list.containedElements() + this

/** A single polygon. */
class PolygonRegion(area: Polygon) extends Region2D with Operation:
  given Operation = this

  val origin = Context.startOp(this)
  val polygons = dynamicList(Seq(area))
  val positivePolygons = polygons
  val negativePolygons = dynamicList(Seq())

  Context.endOp(this)
  arguments(area)

class MultiPolyRegion(polys: Seq[Polygon]) extends Region2D with Operation:
  given Operation = this

  val origin = Context.startOp(this)
  val polygons = dynamicList(polys)
  val positivePolygons = polygons
  val negativePolygons = dynamicList(Seq())

  Context.endOp(this)
  arguments(polys*)
