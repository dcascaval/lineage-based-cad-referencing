package elodin.opt

import scala.annotation.targetName

import scalajs.js
import js.JSConverters.*
import js.UndefOr
import org.scalajs.dom.{document as doc, window as win, console, Document, KeyboardEvent}
import org.scalajs.dom.raw.{HTMLElement, MouseEvent}
import collection.mutable.Buffer

import elodin.global.api.*

import elodin.global.js.*
import elodin.dom.api.*
import elodin.three.*
import prelude.*

import Operations.*
import ThreeContext.*
import typings.three.objects
import typings.three.materials
import typings.three.math.{Box3, Line3}
import typings.three.extras.core.{Curve, Shape}
import typings.three.materials.{LineBasicMaterial, LineDashedMaterial, MeshLambertMaterial}
import typings.three.geometries.{ExtrudeGeometry, EdgesGeometry, ExtrudeGeometryOptions}
import typings.three.examples.lines.{Line2, LineGeometry, LineMaterial, LineMaterialParameters}
import typings.three.examples.controls.MapControls
import typings.three.math.Quaternion

// OPTIMIZATIONS TO MAKE IN RENDERER:
//
// - Reuse BufferGeometry objects between render passes (hard for arbitrary shapes with variable triangulations,
//   but could be worthwhile with 3D faces and such that are created by PlaneGeometry.)
// - MERGE BufferGeometries of the same type so that we end up doing many fewer draw calls. This is can be quite
//   high return; we saw about a 50% speedup on the gear example with a few hundred elements (merging does, itself,
//   have some overhead.) Unfortunately; this is hard to do for the edges: the lines use InterleavedBufferAttributes,
//   which THREE does not care to merge for us.

enum Style:
  case Fill(s: String)
  case Opacity(d: Double)
  case Stroke(s: String)
  case Dotted

import elodin.geometry.*

type Renderable = Point | Edge | Polygon | Region2D | Solid | Vertex3D | Edge3D | Face3D

class RenderObject(val objs: Seq[Object3D])

enum View { case View2D; case View3D }
import View.*

extension (ls: Seq[Int])
  def sum =
    ls.reduceLeftOption(_ + _).getOrElse((0))

object RenderObject:
  @targetName("renderObjectSingle")
  def apply(objs: Object3D*) =
    new RenderObject(objs)

  @targetName("renderObjectSeq")
  def apply(objs: Seq[Object3D]) =
    new RenderObject(objs)

  given Ordering[Vector3] with
    def compare(a: Vector3, b: Vector3): Int =
      if a.x != b.x then a.x.compare(b.x)
      else if a.y != b.y then a.y.compare(b.y)
      else a.z.compare(b.z)

  extension (ps: js.Array[Vector3])
    @targetName("show3")
    def show() =
      ps.toSeq.map(p => f"[${p.x}%.2f, ${p.y}%.2f, ${p.z}%.2f]").mkString(" ")
    def flatten(): js.Array[Double] =
      val result = new js.Array[Double](ps.length * 3)
      for i <- 0 until ps.length do
        val p = ps(i); val i3 = 3 * i
        result(i3 + 0) = p.x
        result(i3 + 1) = p.y
        result(i3 + 2) = p.z
      result

  extension [A](ps: js.Array[A])
    def map[B](f: A => B): js.Array[B] =
      val result = new js.Array[B](ps.length)
      for i <- 0 until ps.length do result(i) = f(ps(i))
      result

  extension (ps: js.Array[Vector2])
    @targetName("show2")
    def show() =
      ps.toSeq.map(p => f"[${p.x}%.2f, ${p.y}%.2f]").mkString(" ")
    def flatten3(): js.Array[Double] =
      val result = new js.Array[Double](ps.length * 3)
      for i <- 0 until ps.length do
        val p = ps(i); val i3 = 3 * i
        result(i3 + 0) = p.x
        result(i3 + 1) = p.y
        result(i3 + 2) = 0.0
      result

  extension (s: Seq[Style])
    def color: Color = Color(s.collectFirst { case f: Style.Fill => f.s }.getOrElse("black"))
    def stroke: Color =
      s.collectFirst { case f: Style.Stroke => f.s }
        .map(c => Color(c))
        .getOrElse(color)
    def dashed = s.contains(Style.Dotted)
    def opacity = s.collectFirst { case f: Style.Opacity => f.d }

  val DEFAULT_POINT_SIZE = 6.0
  private var pointSize = DEFAULT_POINT_SIZE
  var cube = new BoxGeometry(pointSize, pointSize, pointSize, 1, 1, 1)

  def makePointShape(pointSize: Double) =
    val r = pointSize / 2
    new ShapeGeometry(
      new Shape().moveTo(-r, -r).lineTo(-r, r).lineTo(r, r).lineTo(r, -r).lineTo(-r, -r)
    )

  var pointShape = makePointShape(pointSize)
  val blu = pointMaterial(Seq(Style.Fill("blue")))

  def setPointSize(n: Double) =
    pointSize = n
    cube.dispose()
    pointShape.dispose()
    cube = new BoxGeometry(pointSize, pointSize, pointSize, 1, 1, 1)
    pointShape = makePointShape(pointSize)

  var allocationTracker = Buffer[Material | BufferGeometry]()

  def deallocate() =
    for obj <- allocationTracker do
      obj match
        case m: Material       => m.dispose()
        case g: BufferGeometry => g.dispose()
    allocationTracker.clear()

  inline def track[M <: Material | BufferGeometry](m: M) =
    allocationTracker += m; m

  def styleMaterial(mat: MeshBasicMaterial, s: Seq[Style]) =
    mat.color = s.color
    if view == View3D then mat.depthWrite = false
    mat.side = DoubleSide
    s.opacity.map(o =>
      mat.depthWrite = false
      mat.transparent = true
      mat.opacity = o
    )
    mat

  def pointMaterial(s: Seq[Style]): Material =
    styleMaterial(new MeshBasicMaterial(), s)

  def lineMaterial(res: Vector2, s: Seq[Style]): LineMaterial =
    val matParams = new LineMaterialParameters {
      var resolution = res
      override val color = s.stroke.getHex().toInt
      override val dashed = s.dashed
      override val dashScale = 0.25
      override val linewidth = if s.dashed then 1.0 else 2.0
      // override val dashSize = 0.5
    }
    new LineMaterial(matParams)

  inline def makeCurve(inline f: Double => (Double, Double, Double)) =
    val crv = new Curve[Vector3] {
      override def getPoint(t: Double, optionalTarget: js.UndefOr[Vector3] = js.undefined) =
        val r = f(t)
        val vr = Vector3(r(0), r(1), r(2))
        if !js.isUndefined(optionalTarget) then //
          optionalTarget.as[Vector3].set(vr.x, vr.y, vr.z)
        vr
    }
    crv.as[js.Dynamic].closed = false
    crv

  def pointInSolid(e: Solid, t: Double): (Double, Double) => Vector3 =
    val f = e.pointInSolid(t)
    (px: Double, py: Double) =>
      val (x, y, z) = f(px, py)
      Vector3(x, y, z)

  def revolveRail(e: Revolve, basePt: (Double, Double)): Curve[Vector3] =
    val axisPt = e.axis.closestPoint(basePt) // The "origin" of the revolve
    val distance = (basePt - axisPt).length

    val v = e.axis.vec.withLength(distance)
    val newX = (-v.y, v.x, 0.0)
    val newY = (0.0, 0.0, distance)

    inline def linear(t: Double) =
      e.angleStart + ((e.angleEnd - e.angleStart) * t)

    makeCurve { t =>
      val y = newY * Math.sin(linear(t))
      val x = newX * Math.cos(linear(t))
      (y + x) + (axisPt.x, axisPt.y, 0.0)
    }

  def extrudeRail(e: Extrusion, p0: (Double, Double)) =
    makeCurve { t => (p0.x, p0.y, t * e.length) }

  def solidRail(e: Solid) =
    e match
      case e: Extrusion => (p: (Double, Double)) => extrudeRail(e, p)
      case e: Revolve   => (p: (Double, Double)) => revolveRail(e, p)

  def samples(theta0: Double, theta1: Double): Int =
    val MIN_ANGLE = 5 // We want at least one sample for every (n) degrees of curvature
    val dt = math.abs(theta1 - theta0)
    // ...and at least a few samples no matter how small the arc is.
    math.max(math.round((dt / tau) * (360 / MIN_ANGLE)).toDouble, 2.0).toInt

  inline def edgeSamples(e: Edge) =
    e match
      case _: Line => 1
      case a: Arc  => samples(a.theta0, a.theta1)

  inline def solidSamples(s: Solid) =
    s match
      case _: Extrusion => 1
      case r: Revolve   => samples(r.angleStart, r.angleEnd)

  def sampleArray(n: Int) = (0 to n.toInt).map(i => i.toDouble / n)

  // class planeGeometryCache()

  def faceGeometry(s: Solid, e: Edge) =
    val samplesX = sampleArray(edgeSamples(e))
    val samplesY = sampleArray(solidSamples(s))

    val surface = track(new PlaneGeometry(0, 0, samplesX.length - 1, samplesY.length - 1))
    val srfVertices = surface.attribute("position").float32Array
    val pointsX = samplesX.map(t => e.eval(t))

    var i = 0; val n = samplesX.length
    for tY <- samplesY do
      var j = 0
      val f = pointInSolid(s, tY)
      for pX <- pointsX do
        val vec = f(pX.x, pX.y)
        val idx = (i * n) + j
        srfVertices(idx * 3 + 0) = vec.x.toFloat
        srfVertices(idx * 3 + 1) = vec.y.toFloat
        srfVertices(idx * 3 + 2) = vec.z.toFloat
        j += 1
      i += 1

    // surface.computeVertexNormals()
    // surface.computeBoundingBox()
    surface

  extension (s: Shape)
    inline def moveToPoint(p: Point) = s.moveTo(p.x, p.y)
    inline def lineToPoint(p: Point) = s.lineTo(p.x, p.y)
    def moveAlongArc(a: Arc): Shape =
      val parameters = sampleArray(samples(a.theta0, a.theta1))
      val finalShape = parameters.foldLeft(s)((shape, t) => //
        shape.lineToPoint(a.eval(t))
      )
      finalShape

  def edgeShape(e: Edge) =
    val shape = new Shape().moveToPoint(e.start)
    e match
      case l: Line => shape.lineToPoint(l.end)
      case a: Arc  => shape.moveAlongArc(a)

  def polygonShape(p: Polygon) =
    p.edges.list match
      case Seq() => new Shape()
      case edges @ Seq(first, rest*) =>
        val init = new Shape().moveToPoint(first.start)
        edges.foldLeft(init)((shape, edge) =>
          edge match
            case l: Line => shape.lineToPoint(l.end)
            case a: Arc  => shape.moveAlongArc(a)
        )

  def polygonShapeSplit(p: Polygon): Seq[Shape] =
    p.edges.list.map(edge =>
      val s = new Shape().moveToPoint(edge.start)
      edge match
        case l: Line => s.lineToPoint(l.end)
        case a: Arc  => s.moveAlongArc(a)
    )

  def generateContainmentMapping(r: Region2D) =
    // For this we actually have to call into the kernel; each negative poly is associated with
    // one positive poly. But let's test the holes.
    val positives = r.positivePolygons.list
    val mapping = positives.map(p => (p, Buffer[Polygon]())).toMap
    val negatives = r.negativePolygons.list

    for neg <- negatives do
      var i = 0; var continue = true
      val negPt = neg.edges.list(0).eval(0.5)
      while i < positives.size && continue do
        val pos = positives(i)
        val contained = Kernel.containsPoint(pos, negPt)
        if contained then
          continue = false
          mapping(pos) += neg
        i += 1
    mapping

  // Generate a shape all in itty pieces.
  // The first sequence is all of the shape faces
  // The second sequence is all of the actual 2d regions with holes in em
  def regionShapesSplit(r: Region2D): (Seq[Shape], Seq[Shape]) =
    val mapping = generateContainmentMapping(r)
    val pieces = Buffer[Shape]()
    val srfs = mapping.toSeq.map((pos, negs) =>
      val shape = polygonShape(pos)
      pieces ++= polygonShapeSplit(pos)
      for neg <- negs do
        val hole = polygonShape(neg)
        pieces ++= polygonShapeSplit(neg)
        shape.holes.push(hole)
      shape
    )
    (pieces.toSeq, srfs)

  def regionShapes(r: Region2D): Seq[Shape] =
    val mapping = generateContainmentMapping(r)
    mapping.toSeq.map((pos, negs) =>
      val shape = polygonShape(pos)
      for neg <- negs do
        val hole = polygonShape(neg)
        shape.holes.push(hole)
      shape
    )

  def makeArea(f: (Double, Double) => Vector3)(s: Shape) =
    val g = track(new ShapeGeometry(s))
    val verts = g.attribute("position").float32Array
    for i <- 0 until (verts.length / 3) do
      val redone = f(verts(3 * i), verts((3 * i) + 1))
      verts(3 * i + 0) = redone.x.toFloat
      verts(3 * i + 1) = redone.y.toFloat
      verts(3 * i + 2) = redone.z.toFloat
    g

  def mkLine3d(lineMat: LineMaterial)(
      s: Shape | Curve[Vector3],
      samples: Option[Double] = None,
      f: Option[Vector2 => Vector3] = None
  ): Seq[Line2[LineGeometry, LineMaterial]] =
    def geoFromPoints(pts: js.Array[Double]) =
      val g = track(new LineGeometry())
      // console.log("making line3d", pts)
      g.setPositions(pts)
      val result = new Line2(g, lineMat)
      result.computeLineDistances()
    s match
      case s: Shape =>
        val seq = (Seq(s) ++ s.holes.toSeq) //
        f match
          case Some(f) => seq.map(c => geoFromPoints(c.getPoints().map(f).flatten()))
          case None    => seq.map(c => geoFromPoints(c.getPoints().flatten3()))
      case s: Curve[?] =>
        val n: js.UndefOr[Double] = samples.getOrElse(js.undefined)
        Seq(geoFromPoints(s.as[Curve[Vector3]].getPoints(n).flatten()))

  def getNumSegsFaces(r: Renderable): (Int, Int) =
    r match
      case p: Point    => (0, 0)
      case e: Edge     => (1, 0)
      case p: Polygon  => (p.edges.size, 1)
      case r: Region2D => (r.polygons.list.map(p => p.edges.size).sum, 1)
      case e: Solid =>
        val baseSegs = e.base.polygons.list.map(p => p.edges.size).sum
        (baseSegs * 3, baseSegs + 2)
      case p3: Vertex3D => (0, 0)
      case e3: Edge3D   => (1, 0)
      case f3: Face3D =>
        f3 match
          case vf: VerticalFace   => (4, 1)
          case hf: HorizontalFace => getNumSegsFaces(hf.parent.base)

  var view: View = View2D
  def fromRenderable(
      renderer: Renderer,
      newView: View,
      width: Double,
      height: Double,
      r: Renderable,
      index: Double,
      s: Seq[Style]
  ): RenderObject =
    view = newView
    val depthStep = (if view == View2D then 0.001 else 0.1)
    var depth = index * depthStep
    val resolution = Vector2(width, height)

    def setMeshDepth[T <: Object3D](m: T) =
      if view == View2D then m.position.z += depth
      m

    def solidMaterials(s: Seq[Style]) =
      val fillMat = pointMaterial(s)
      val lineMat = lineMaterial(resolution, s)
      (fillMat, lineMat)

    def drawPoint(p: Point, s: Seq[Style]) =
      val mesh = new Mesh(pointShape, pointMaterial(s))
      mesh.position.set(p.x, p.y, 0.0)
      setMeshDepth(mesh)

    def drawEdge(e: Edge, s: Seq[Style]) =
      val shape = edgeShape(e)
      val mat = lineMaterial(resolution, s)
      mkLine3d(mat)(shape).map(setMeshDepth)

    def drawPolygonShape(shape: Shape, s: Seq[Style]) =
      val geo = track(new ShapeGeometry(shape))
      val mesh = new Mesh(geo, pointMaterial(s))
      setMeshDepth(mesh)

    def drawPolygon(p: Polygon, s: Seq[Style]) =
      if p.edges.size == 0 then Seq()
      else if !s.dashed then Seq(drawPolygonShape(polygonShape(p), s))
      else p.edges.list.flatMap(e => drawEdge(e, s))

    def drawRegion(r: Region2D, s: Seq[Style]) =
      if !s.dashed then regionShapes(r).map(drawPolygonShape(_, s))
      else r.polygons.list.flatMap(drawPolygon(_, s))

    extension (axis: Axis3D)
      def vector(): Vector3 =
        val (x, y, z) = axis.normalVector
        Vector3(x, y, z)

    def getMatrixFromTransforms(t: Seq[Transform3D]): typings.three.math.Matrix4 =
      var obj = new Object3D()
      val originalObj = obj
      for transform <- t do
        transform match
          case Translate(x, y, z) =>
            val newObj = new Object3D()
            obj.position.add(Vector3(x, y, z))
            newObj.add(obj)
            newObj.updateMatrixWorld(true)
            obj = newObj
          case Rotate(axis, angle) =>
            val newObj = new Object3D() // PIVOT
            val (dx, dy, dz) = axis.s
            obj.position.sub(Vector3(dx, dy, dz)) // ELEMENT
            obj.position.applyAxisAngle(axis.vector(), angle)
            obj.position.add(Vector3(dx, dy, dz))
            obj.rotateOnAxis(axis.vector(), angle)
            newObj.add(obj)
            newObj.updateMatrixWorld(true)
            obj = newObj

      originalObj.updateMatrixWorld(true)
      originalObj.matrixWorld

    def applyTransformsToMesh(mesh: Mesh[?, ?], m: Matrix4): Unit =
      mesh.applyMatrix4(m)

    def drawVertex3(v: Vertex3D, s: Seq[Style]) =
      // val p0 = v.physicalPoint
      // val m1 = new Mesh(cube, blu)
      // m1.position.set(p0.x, p0.y, p0.z)
      // console.log("Setting vertex", m1.position)

      val p1 = v.inSolid()
      val m2 = new Mesh(cube, pointMaterial(s))
      m2.position.set(p1.x, p1.y, p1.z)
      applyTransformsToMesh(m2, getMatrixFromTransforms(v.transforms))
      Seq(m2)

    def drawEdge3(e: Edge3D, s: Seq[Style]) =
      val mat = lineMaterial(resolution, s)
      val meshes = e match
        case h: HorizontalEdge =>
          val f = pointInSolid(h.parent, h.parameter)
          val shape = edgeShape(h.edge)
          mkLine3d(mat)(shape, None, Some(p => f(p.x, p.y)))
        case v: VerticalEdge =>
          val start = (v.start.point.x, v.start.point.y)
          val rail = solidRail(v.parent)(start)
          mkLine3d(mat)(rail, Some(solidSamples(v.parent).toDouble))
      val transformMatrix = getMatrixFromTransforms(e.transforms)
      for mesh <- meshes do applyTransformsToMesh(mesh, transformMatrix)
      meshes

    def drawFace3(f: Face3D, s: Seq[Style]) =
      if s.dashed then Seq()
      else
        val fill = pointMaterial(s)
        val meshes = f match
          case h: HorizontalFace =>
            val shapes = regionShapes(h.parent.base)
            shapes.map(shape =>
              val area = makeArea(pointInSolid(h.parent, h.parameter))(shape)
              new Mesh(area, fill)
            )
          case v: VerticalFace =>
            Seq(new Mesh(faceGeometry(v.parent, v.edge), fill))
        val transformMatrix = getMatrixFromTransforms(f.parent.transforms ++ f.transforms)
        for mesh <- meshes do applyTransformsToMesh(mesh, transformMatrix)
        meshes

    def drawSolid(e: Solid, s: Seq[Style]) =
      val (pMat, lineMat) = solidMaterials(s)
      val dashed = s.dashed
      val getRail = solidRail(e)
      val numSamples = solidSamples(e)
      val areas = regionShapes(e.base)

      val lineMeshes =
        if renderer.renderEdges then
          e.base.edges.list.flatMap { edge =>
            // We do revolutions by constructing first the 3D curve along which to extrude
            // and then making an ExtrudeGeometry Out of it.
            val start = (edge.start.x, edge.start.y)
            val rail = getRail(start)
            val shape = edgeShape(edge)

            val f0 = pointInSolid(e, 0.0)
            val f1 = pointInSolid(e, 1.0)
            val lRail = mkLine3d(lineMat)(rail, Some(numSamples.toDouble))

            // Todo: these are both duplicative with the face meshes; perhaps can be shared
            // (Likely not a huge contribution since it's just like adding two more rows anyway, but may as well)
            // ... In the case of extrudes, they're like 50% more, so. worth
            val lProf1 = mkLine3d(lineMat)(shape, None, Some(p => f0(p.x, p.y)))
            val lProf2 = mkLine3d(lineMat)(shape, None, Some(p => f1(p.x, p.y)))
            lRail ++ lProf1 ++ lProf2
          }
        else Seq()

      val areaMeshes =
        if s.dashed then Seq()
        else
          areas.flatMap { s0 =>
            val g0 = makeArea(pointInSolid(e, 0.0))(s0)
            val g1 = makeArea(pointInSolid(e, 1.0))(s0)
            Seq(new Mesh(g0, pMat), new Mesh(g1, pMat))
          }
      val faceMeshes =
        if s.dashed then Seq()
        else
          e.base.edges.list.flatMap(edge =>
            val g = faceGeometry(e, edge)
            val m1 = new Mesh(g, pMat)
            Seq(m1)
          )
      val originMeshes = faceMeshes ++ areaMeshes ++ lineMeshes
      val transformMatrix = getMatrixFromTransforms(e.transforms)
      for mesh <- originMeshes do applyTransformsToMesh(mesh, transformMatrix)
      originMeshes

    r match
      case p: Point     => RenderObject(drawPoint(p, s))
      case e: Edge      => RenderObject(drawEdge(e, s))
      case p: Polygon   => RenderObject(drawPolygon(p, s))
      case r: Region2D  => RenderObject(drawRegion(r, s))
      case e: Solid     => RenderObject(drawSolid(e, s))
      case p3: Vertex3D => RenderObject(drawVertex3(p3, s))
      case e3: Edge3D   => RenderObject(drawEdge3(e3, s))
      case f3: Face3D   => RenderObject(drawFace3(f3, s))

trait Renderer:
  val willDraw: Boolean
  var renderEdges: Boolean
  def draw(elements: Seq[(Renderable, Seq[Style])]): Unit
  def setPointSize(n: Double) =
    RenderObject.setPointSize(n)
  var num_segments: Int
  var num_faces: Int

class NullRenderer extends Renderer:
  val willDraw = false
  var renderEdges = false
  var num_segments = 0
  var num_faces = 0
  def draw(elements: Seq[(Renderable, Seq[Style])]) = ()

var EDITOR_WIDTH = 668

class MeasuringRenderer extends Renderer:
  val willDraw = true
  var renderEdges = false
  var num_segments = 0
  var num_faces = 0
  def draw(elements: Seq[(Renderable, Seq[Style])]) =
    val (ns, nf) = elements
      .map(e => RenderObject.getNumSegsFaces(e(0)))
      .foldLeft((0, 0))((acc, next) => (acc(0) + next(0), acc(1) + next(1)))
    num_segments = ns
    num_faces = nf

class Renderer3D(container: HTMLElement, view: View) extends Renderer:
  def withView[T](a: => T, b: => T) =
    if view == View2D then a else b

  val willDraw = true
  val document = doc
  val window = win
  var renderEdges = true

  def width = window.innerWidth - EDITOR_WIDTH - 10
  def height = window.innerHeight
  RenderObject.setPointSize(RenderObject.DEFAULT_POINT_SIZE)

  type ResizeFn = (Double, Double) => Unit
  val resizeListeners = Buffer[ResizeFn]()
  def onResize(callback: ResizeFn) =
    resizeListeners += callback

  val currentElements = Buffer[RenderObject]()
  val scene = new Scene()
  val renderer = new WebGLRenderer(new { antialias = true; alpha = true })

  val camera = withView(
    new OrthographicCamera(0, width, height, 0, -1, 1),
    new PerspectiveCamera(75, width / height, 0.01, 100000)
  )
  if view == View3D then camera.up = Vector3(0, 0, 1)

  val controls = new OrbitControls(camera, renderer.domElement)
  if view == View2D then controls.enableRotate = false

  renderer.setPixelRatio(window.devicePixelRatio)
  renderer.setSize(width, height)
  container.appendChild(renderer.domElement)
  controls.update()

  val ctx = new ThreeContext(scene, camera, renderer, Some(controls))
  scene.add(new AxesHelper(20))
  if view == View3D then //
    ctx.setCamera(Vector3(-200, -200, 200), Vector3(0, 0, 0))

  extension (inline v: Vector3)
    inline def anyNaN =
      v.x.isNaN || v.y.isNaN || v.z.isNaN
    inline def anyInfinite =
      v.x.isInfinite() || v.y.isInfinite() || v.z.isInfinite()

  def centerView() =
    val bbox = new Box3()
    for r <- currentElements do
      for obj <- r.objs do
        if !obj.position.anyNaN then
          bbox.expandByObject(obj)
          scene.add(obj)

    val center = new Vector3()
    bbox.getCenter(center)

    if view == View3D then
      val axial = new Vector3().copy(bbox.max).sub(bbox.min).length()
      val cameraVec = new Vector3().copy(camera.position).sub(center)
      val newPos =
        if cameraVec.length() > axial then cameraVec.setLength(axial).add(center)
        else camera.position
      if !newPos.anyInfinite && !newPos.anyNaN then ctx.setCamera(newPos, center)
    else
      center.x -= width / 2
      center.y -= height / 2
      val target = center.jsClone().asInstanceOf[Vector3]
      center.z = camera.position.z
      target.z = 0.0
      ctx.setCamera(center, target)

  var numRenders = 0
  var num_segments = 0
  var num_faces = 0

  def render() =
    // Iterate through all renderObjects
    for r <- currentElements do for obj <- r.objs do scene.add(obj)
    if numRenders == 0 then centerView()

  def drawScene() =
    renderer.render(scene, camera)

  def onWindowResize() =
    if view == View.View2D then
      val c = camera.as[OrthographicCamera]
      c.right = width
      c.top = height
      c.updateProjectionMatrix()
    else
      val c = camera.as[PerspectiveCamera]
      c.aspect = width / height
      c.updateProjectionMatrix()
    renderer.setSize(width, height)
    for listener <- resizeListeners do listener(width, height)
    drawScene()

  render()
  renderer.render(scene, camera)
  window.addEventListener("resize", _ => onWindowResize())
  controls.addEventListener("change", _ => drawScene())
  drawScene()

  val disposeCallbacks = Buffer[() => Unit]()
  def dispose() =
    disposeCallbacks.foreach(_())
    RenderObject.deallocate()

  val TRACK_MOUSE = false
  if TRACK_MOUSE then
    val mouseText = tags.div("mouse-position")
    document.body(mouseText)

    // TRACK MOUSE POINTER FROM SCREEN SPACE. We need to do this
    // for bidirectional dragging.
    val listener: js.Function1[MouseEvent, Any] = (e: MouseEvent) =>
      val x = e.clientX; val y = e.clientY
      val x0 = (x / width) * 2 - 1
      val y0 = (-(y / height)) * 2 + 1
      val wp = Vector3(x0, y0, -1).unproject(camera)
      val (ex, ey) = (e.clientX.toInt, e.clientY.toInt)
      mouseText.innerHTML = s"${wp.x.toInt},${wp.y.toInt}"
      mouseText.attr("style", s"top: ${ey}px; left:${ex + 15}px")

    document.addEventListener("mousemove", listener)
    disposeCallbacks += (() =>
      document.body.removeChild(mouseText)
      document.removeEventListener("mousemove", listener)
    )

  def draw(elements: Seq[(Renderable, Seq[Style])]) =
    val (w, h) = (width, height)
    // remove old elements
    for obj <- currentElements do scene.remove(obj.objs*)
    RenderObject.deallocate()
    currentElements.clear()

    // add new elements
    currentElements ++= elements.zipWithIndex.map { case ((r, s), i) =>
      RenderObject.fromRenderable(this, view, w, h, r, i, s)
    }

    render()
    drawScene()
    numRenders += 1
