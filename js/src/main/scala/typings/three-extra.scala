package typings.three.examples.lines

import scala.scalajs.js
import js.annotation.*
import org.scalajs.dom.*
import org.scalajs.dom.raw.{
  HTMLElement,
  HTMLDocument,
  HTMLMediaElement,
  HTMLVideoElement,
  HTMLCanvasElement,
  HTMLImageElement
}
import org.scalajs.dom.raw.{WebGLShader, WebGLFramebuffer}
import org.scalajs.dom.experimental.gamepad.*
import scalajs.js.typedarray.*

import typings.three.*
import typings.three.helpers.*
import typings.three.textures.*
import typings.three.objects.*
import typings.three.extras.*
import typings.three.extras.core.*
import typings.three.extras.curves.*
import typings.three.extras.objects.*
import typings.three.animation.*
import typings.three.animation.tracks.*
import typings.three.loaders.*
import typings.three.cameras.*
import typings.three.core.*
import typings.three.materials.*
import typings.three.renderers.*
import typings.three.renderers.webxr.*
import typings.three.renderers.shaders.*
import typings.three.renderers.webgl.*
import typings.three.geometries.*
import typings.three.scenes.*
import typings.three.math.*
import typings.three.math.interpolants.*
import typings.three.audio.*
import typings.three.lights.*
import typings.three.*

@js.native
@JSImport("three/examples/jsm/lines/Line2", "Line2")
class Line2[G <: BufferGeometry, M <: Material] extends Mesh[G, M]:
  def this(geometry: G, material: M) = this()
  def computeLineDistances(): this.type = js.native
  override def raycast(raycaster: Raycaster, intersects: scala.scalajs.js.Array[Intersection]): Unit =
    js.native

@js.native
@JSImport("three/examples/jsm/lines/LineGeometry", "LineGeometry")
class LineGeometry() extends InstancedBufferGeometry:
  def setPositions(positions: js.Array[Double]): this.type = js.native
  def setColors(colors: js.Array[Double]): this.type = js.native
  def fromLine(line: Line[?, ?]): this.type = js.native

trait LineMaterialParameters extends js.Object:
  val color: js.UndefOr[Int] = js.undefined
  val linewidth: js.UndefOr[Double] = js.undefined
  val dashed: js.UndefOr[Boolean] = js.undefined
  val dashScale: js.UndefOr[Double] = js.undefined
  var dashSize: js.UndefOr[Double] = js.undefined
  var dashOffset: js.UndefOr[Double] = js.undefined
  var gapSize: js.UndefOr[Double] = js.undefined
  var resolution: Vector2

@js.native
@JSImport("three/examples/jsm/lines/LineMaterial", "LineMaterial")
class LineMaterial extends ShaderMaterial:
  def this(parameters: LineMaterialParameters) = this()

@js.native
@JSImport("three/examples/jsm/utils/BufferGeometryUtils", "BufferGeometryUtils")
object BufferGeometryUtils extends js.Object:
  def mergeBufferGeometries(
      geometries: js.Array[BufferGeometry],
      useGroups: js.UndefOr[Boolean] = js.undefined
  ): BufferGeometry = js.native
