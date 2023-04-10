package elodin.three

object prelude:
  type Side = typings.three.Side
  val FrontSide = typings.three.FrontSide
  val BackSide = typings.three.BackSide
  val DoubleSide = typings.three.DoubleSide

  type Float32Array = scalajs.js.typedarray.Float32Array
  type Uint16Array = scalajs.js.typedarray.Uint16Array

  type Color = typings.three.math.Color
  type Vector2 = typings.three.math.Vector2
  type Vector3 = typings.three.math.Vector3
  type Vector4 = typings.three.math.Vector4
  type Matrix = typings.three.math.Matrix
  type Matrix3 = typings.three.math.Matrix3
  type Matrix4 = typings.three.math.Matrix4
  def Color = new Color(_)
  def Color(r: Double, g: Double, b: Double) = new Color(r, g, b)
  def Vector2 = new Vector2(_, _)
  def Vector3 = new Vector3(_, _, _)
  def Matrix3 = new Matrix3()
  def Matrix4 = new Matrix4()

  type BufferGeometry = typings.three.core.BufferGeometry
  type BufferAttribute = typings.three.core.BufferAttribute
  type InterleavedBufferAttribute = typings.three.core.InterleavedBufferAttribute
  type Object3D = typings.three.core.Object3D
  type Group = typings.three.objects.Group

  type Scene = typings.three.scenes.Scene
  type Camera = typings.three.cameras.Camera
  type PerspectiveCamera = typings.three.cameras.PerspectiveCamera
  type OrthographicCamera = typings.three.cameras.OrthographicCamera

  type OrbitControls = typings.three.examples.controls.OrbitControls
  type AxesHelper = typings.three.helpers.AxesHelper

  type PolyhedronGeometry = typings.three.geometries.PolyhedronGeometry
  type OctahedronGeometry = typings.three.geometries.OctahedronGeometry
  type RingGeometry = typings.three.geometries.RingGeometry
  type TorusGeometry = typings.three.geometries.TorusGeometry
  type IcosahedronGeometry = typings.three.geometries.IcosahedronGeometry
  type SphereGeometry = typings.three.geometries.SphereGeometry
  type ConeGeometry = typings.three.geometries.ConeGeometry
  type DodecahedronGeometry = typings.three.geometries.DodecahedronGeometry
  type ShapeGeometry = typings.three.geometries.ShapeGeometry
  type TextGeometry = typings.three.geometries.TextGeometry
  type TorusKnotGeometry = typings.three.geometries.TorusKnotGeometry
  type WireframeGeometry = typings.three.geometries.WireframeGeometry
  type CircleGeometry = typings.three.geometries.CircleGeometry
  type PlaneGeometry = typings.three.geometries.PlaneGeometry
  type LatheGeometry = typings.three.geometries.LatheGeometry
  type ParametricGeometry = typings.three.geometries.ParametricGeometry
  type EdgesGeometry = typings.three.geometries.EdgesGeometry
  type CylinderGeometry = typings.three.geometries.CylinderGeometry
  type ExtrudeGeometry = typings.three.geometries.ExtrudeGeometry
  type BoxGeometry = typings.three.geometries.BoxGeometry
  type TubeGeometry = typings.three.geometries.TubeGeometry
  type TetrahedronGeometry = typings.three.geometries.TetrahedronGeometry

  type Material = typings.three.materials.Material
  type ShaderMaterial = typings.three.materials.ShaderMaterial
  type PointsMaterial = typings.three.materials.PointsMaterial
  type MeshBasicMaterial = typings.three.materials.MeshBasicMaterial
  type LineBasicMaterial = typings.three.materials.LineBasicMaterial
  type LineBasicMaterialParameters = typings.three.materials.LineBasicMaterialParameters
  type MeshNormalMaterial = typings.three.materials.MeshNormalMaterial
  type ShaderMaterialParameters = typings.three.materials.ShaderMaterialParameters
  type MeshBasicMaterialParameters = typings.three.materials.MeshBasicMaterialParameters
  type MeshNormalMaterialParameters = typings.three.materials.MeshNormalMaterialParameters
  type ShaderUniform = typings.three.materials.AnonObject31

  type Mesh[G <: BufferGeometry, M <: Material] = typings.three.objects.Mesh[G, M]
  type InstancedMesh[G <: BufferGeometry, M <: Material] = typings.three.objects.InstancedMesh[G, M]
  type LineSegments[G <: BufferGeometry, M <: Material] = typings.three.objects.LineSegments[G, M]
  type Points[G <: BufferGeometry, M <: Material] = typings.three.objects.Points[G, M]

  type Texture = typings.three.textures.Texture
  type Renderer = typings.three.renderers.Renderer
  type WebGLRenderer = typings.three.renderers.WebGLRenderer

  val ShaderChunk = typings.three.renderers.shaders.ShaderChunk
  val UniformsLib = typings.three.renderers.shaders.UniformsLib

  type Fog = typings.three.scenes.Fog
