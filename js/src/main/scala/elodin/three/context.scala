package elodin.three

import scalajs.js
import org.scalajs.dom.{document as doc, window as win, Document, KeyboardEvent, console}
import collection.mutable.Buffer

import elodin.global.api.*
import elodin.dom.api.*

import prelude.*

class ThreeContext(
    var scene: Scene,
    var camera: Camera,
    var renderer: WebGLRenderer,
    var controls: Option[OrbitControls]
):
  def background(color: String | Color) =
    val arg = color match
      case c: String => Color(c)
      case c: Color  => c
    scene.background = arg

  def setCamera(position: Vector3, target: Vector3) =
    camera.position.copy(position)
    camera.updateMatrix()
    controls.map(ctl =>
      ctl.target = target
      ctl.update()
    )

object ThreeContext:

  def draw(xs: Object3D*)(using ctx: ThreeContext) =
    ctx.scene.add(xs*)

  def animate(f: Double => Any)(using ctx: ThreeContext) =
    ctx.renderer.setAnimationLoop((t, _) => f(t))

  inline def remap(x: Float, y: Float, z: Float) = (y, z, x)
  inline def remap(x: Double, y: Double, z: Double) = (y.toFloat, z.toFloat, x.toFloat)
  def remap(v: Vector3) = Vector3(v.y.toFloat, v.z.toFloat, v.x.toFloat)

  def Vec(x: Double, y: Double, z: Double) =
    val (rx, ry, rz) = remap(x, y, z)
    Vector3(rx, ry, rz)
