package elodin.opt

import scalajs.js
import org.scalajs.dom.{document, console, window, MouseEvent}
import org.scalajs.dom.raw.HTMLElement
import collection.mutable.{Map, Buffer, Stack}

import elodin.global.api.*
import elodin.geometry.*

import UI.ErrorDialog

import api.*
import dsl.*
import Operations.{extrudeCurve, extrudeCurveAngle, userDefinedLineage, subtractAll, subtractAllPolygons}

import scala.util.CommandLineParser.ParseError

object Var:
  def apply(value: Double, name: String): Var =
    val bounds = dsl.defaultBound(value)
    Var(value, name, bounds.min, bounds.max)

  def apply(value: Double, name: String, min: Double, max: Double): Var =
    new Var(value, name, min, max)

class Var(var value: Double, val name: String, val min: Double, val max: Double):
  def withValue(newValue: Double): Var = Var(newValue, name, min, max)

object Heap:
  type Binding = collection.immutable.Map[String, DynObj]
  type Scope = List[Binding]

  def makeObject(kv: (String, DynObj)*) =
    (collection.immutable.Map.from[String, DynObj](kv), ObjectT)

class Heap():
  import Heap.*

  var bindings: Scope = List(collection.immutable.Map[String, DynObj]())
  var stacks = List[Scope]()

  def pushScope(scope: Scope) =
    stacks = bindings :: stacks
    bindings = scope

  def popScope(): Unit = stacks match
    case top :: rest =>
      bindings = top
      stacks = rest
    case _ => ()

  final def find_map[A, B](ls: List[A], p: A => Option[B]): Option[B] =
    var these = ls
    while !these.isEmpty do
      val result = p(these.head)
      if result.isDefined then return result
      these = these.tail
    None

  def get(name: String): Option[DynObj] =
    find_map(bindings, _.get(name))

  def getOrElse(name: String, orElse: => Nothing): DynObj =
    get(name).getOrElse(orElse)

  // Binding scopes outside of the local one cannot be mutated
  def update(key: String, value: DynObj): Unit =
    bindings = (bindings match
      case head :: tl => (head + ((key, value))) :: tl
      case _          => bindings
    )

object Interpreter:
  // Set up FUNCTION TABLES, Binding helpers, and finder
  //
  val keywords = Set("parameters", "operation")
  val functions = Map[String, Buffer[LiftedFn]]()
  val methods = Map[String, Buffer[LiftedMethod]]()
  val macros = Map[String, Seq[Expression] => DynObj]()
  var init_finished = false

  def bind(name: String, pair: LiftedFn) =
    if !init_finished then functions.getOrElseUpdate(name, Buffer()) += pair
    else functions(name) = Buffer(pair)

  def bindOp(name: String, pair: LiftedFn) =
    val (f, predicate) = pair
    val opF = (args: Seq[DynObj]) =>
      Context.num_operations += 1
      f(args)
    bind(name, (opF, predicate))

  def bindF(name: String, f: WrappedFn, m: Seq[VarType] => Boolean) =
    bind(name, (f, m))
  def bindMacro(name: String, f: Seq[Expression] => DynObj) =
    macros(name) = f
  def bindM(name: String, pair: LiftedMethod) =
    if !init_finished then methods.getOrElseUpdate(name, Buffer()) += pair
    else methods(name) = Buffer(pair)

  def bindMOp(name: String, pair: LiftedMethod) =
    val (r, f, predicate) = pair
    val opF = (rcv: DynObj, args: Seq[DynObj]) =>
      Context.num_operations += 1
      f(rcv, args)
    bindM(name, (r, opF, predicate))

  def findFunction(name: String, argTypes: Seq[VarType]): Option[WrappedFn] =
    val allCandidates = functions.getOrElse(name, Buffer())
    val candidates = allCandidates.filter((_, matches) => matches(argTypes))
    candidates match
      case Buffer()  => None
      case Buffer(f) => Some(f(0))
      case _         => error(s"Ambiguous overloads for function $name")

  def findMacro(name: String): Option[Seq[Expression] => DynObj] =
    macros.get(name)

  type MethodFindResult = Either[String, WrappedMethod]

  def findMethod(name: String, rcvType: VarType, argTypes: Seq[VarType]): MethodFindResult =
    val allCandidates = methods.getOrElse(name, Buffer())
    val candidates = allCandidates
      .flatMap((recTyp, f, matches) =>
        subtypeDistance(rcvType, recTyp).flatMap(d => if matches(argTypes) then Some(d, f) else None)
      )
      .sortBy(_(0))
    if candidates.size == 0 then
      Left(
        s"No method of name $name on type $rcvType matches argument signature ${formatArgTypes(argTypes)}"
      )
    else
      val minDistance = candidates(0)(0)
      val minCandidates = candidates.filter((d, _) => d == minDistance)
      if minCandidates.size > 1 then Left(s"Ambiguous overloads for method $name")
      else Right(minCandidates(0)(1))

  // GEOMETRIC FUNCTIONS

  // format: off
  // Primitive Constructors
  bind("pt", liftT[NumT, NumT, PointT](pt(_, _))) // (x,y) => pt(x,y)
  bind("Line", liftT[PointT, PointT, EdgeT](Line(_, _, orphan = true)))
  bindOp("Circle", liftT[PointT, NumT, CircleT](Circle(_, _)))
  bindOp("Square", liftT[NumT, NumT, NumT, RectT](Square(_, _, _)))
  bindOp("Square", liftT[PointT, NumT, RectT]((p, r) => Square(p.x, p.y, r)))
  bindOp("Rectangle", liftT[PointT, NumT, NumT, RectT](Rectangle(_, _, _)))
  bindOp("Rectangle", liftT[NumT, NumT, NumT, NumT, RectT]((x, y, w, h) => Rectangle(pt(x, y), w, h)))
  bindOp("Polygon", liftT[PointT, NumT, NumT, PolygonT]((pt, n, r) => RegularPolygon(pt, n, r)))

  bindOp("IntersectCurves", liftT[EdgeT, EdgeT, SeqT[PointT]](IntersectCurves(_,_).pts))

  // Booleans
  bindOp("Union", liftT[PolygonT, PolygonT, RegionT](UnionPolygons(_, _)))
  bindOp("Union", liftT[RegionT, RegionT, RegionT](UnionRegions(_, _)))
  bindOp("Union", liftT[PolygonT, RegionT, RegionT]((a, b) => UnionRegions(a.asRegion(), b)))
  bindOp("Union", liftT[RegionT, PolygonT, RegionT]((a, b) => UnionRegions(a, b.asRegion())))
  bindOp("Difference", liftT[PolygonT, PolygonT, RegionT](SubtractPolygons(_, _)))
  bindOp("Difference", liftT[RegionT, RegionT, RegionT](SubtractRegions(_, _)))
  bindOp("Difference", liftT[PolygonT, RegionT, RegionT]((a, b) => SubtractRegions(a.asRegion(), b)))
  bindOp("Difference", liftT[RegionT, PolygonT, RegionT]((a, b) => SubtractRegions(a, b.asRegion())))

  bindOp("Intersection", liftT[PolygonT, PolygonT, RegionT](IntersectPolygons(_, _)))
  bindOp("UnionAll",
    ((args: Seq[DynObj]) =>
        val flatPolys = args.flatMap((a,t) =>
          t match
            case SeqT(_) => a.as[DynList[Polygon]].list
            case _ => Seq(a.as[Polygon])
          )
        (UnionAll(flatPolys), RegionT),
     (typs: Seq[VarType]) => typs.length > 0 &&
         typs.forall {
          case SeqT(t) => subtypeOf(t, PolygonT)
          case t       => subtypeOf(t, PolygonT)
        }
      ))
  bindOp("DifferenceAll", liftT[PolygonT, SeqT[PolygonT], RegionT]((p, ps) => subtractAllPolygons(p, ps.list)))
  bindOp("DifferenceAll", liftT[RegionT, SeqT[RegionT], RegionT]((p, ps) => subtractAll(p, ps.list)))
  // bind("DifferenceAll", liftT[PolygonT, SeqT[RegionT], RegionT])
  // bind("DifferenceAll", liftT[RegionT, SeqT[PolygonT], RegionT])

  // Constructive "CAD Ops"
  bindOp("Chamfer", liftT[PolygonT, PointT, NumT, PolygonT]((p, pt, r) => ChamferAll(p, Seq(pt), r)))
  bindOp("Chamfer", liftT[PolygonT, SeqT[PointT], NumT, PolygonT]((p, pts, r) => ChamferAll(p, pts.list, r)))
  bindOp("Fillet", liftT[PolygonT, PointT, NumT, PolygonT]((p, pt, r) => FilletAll(p, Seq(pt), r)))
  bindOp("Fillet", liftT[PolygonT, SeqT[PointT], NumT, PolygonT]((p, pts, r) => FilletAll(p, pts.list, r)))

  // Extrude both polygons and regions, with any spelling
  bindOp("Extrude3D", liftT[RegionT, NumT, ExtrudeT](Extrusion(_, _)))
  bindOp("Extrude3D", liftT[PolygonT, NumT, ExtrudeT]((p, h) => Extrusion(p.asRegion(), h)))
  bindOp("Extrude3d", liftT[RegionT, NumT, ExtrudeT](Extrusion(_, _)))
  bindOp("Extrude3d", liftT[PolygonT, NumT, ExtrudeT]((p, h) => Extrusion(p.asRegion(), h)))
  bindOp("Revolve", liftT[RegionT, EdgeT, RevolveT]((p, a) => Revolve(p, Axis(a))))
  bindOp("Revolve", liftT[PolygonT, EdgeT, RevolveT]((p, a) => Revolve(p.asRegion(), Axis(a))))
  bindOp("Revolve", liftT[RegionT, EdgeT, NumT, NumT, RevolveT]((p, a, s, e) => Revolve(p, Axis(a), deg2rad(s), deg2rad(e))))
  bindOp("Revolve", liftT[PolygonT, EdgeT, NumT, NumT, RevolveT]((p, a, s, e) => Revolve(p.asRegion(), Axis(a), deg2rad(s), deg2rad(e))))

  bindOp("ExtrudeEdge", liftT[PolygonT, EdgeT, NumT, ExtrudeEdgeT](ExtrudeEdge(_, _, _, 0.0, 1.0)))
  bindOp("ExtrudeEdge", liftT[PolygonT, EdgeT, NumT, NumT, NumT, ExtrudeEdgeT](ExtrudeEdge(_, _, _, _, _)))
  bindOp("InsetEdge", liftT[PolygonT, EdgeT, NumT, InsetEdgeT](InsetEdge(_, _, _, 0.0, 1.0)))
  bindOp("InsetEdge", liftT[PolygonT, EdgeT, NumT, NumT, NumT, InsetEdgeT](InsetEdge(_, _, _, _, _)))

  // TODO: Add named arguments to make these easier to use
  bindOp("ExtrudeCurve", liftT[EdgeT, NumT, ExtrudeCurveT](extrudeCurve(_, _, None)))
  bindOp("ExtrudeCurve", liftT[EdgeT, NumT, PointT, ExtrudeCurveT]((e, l, d) => extrudeCurve(e, l, Some(d))))
  bindOp("ExtrudeCurveAngle", liftT[EdgeT, NumT, NumT, ExtrudeCurveT](extrudeCurveAngle(_, _, _)))
  bind("lineage", liftT[AnyT, AnyT, VoidT](userDefinedLineage(_, _)))

  //
  // Arithmetic ops
  bind("-", liftT[NumT, NumT](a => -a))

  bind("+", liftT[NumT, NumT, NumT](_ + _))
  bind("-", liftT[NumT, NumT, NumT](_ - _))
  bind("*", liftT[NumT, NumT, NumT](_ * _))
  bind("/", liftT[NumT, NumT, NumT](_ / _))
  bind(">", liftT[NumT, NumT, BoolT](_ > _))
  bind(">=", liftT[NumT, NumT, BoolT](_ >= _))
  bind("<", liftT[NumT, NumT, BoolT](_ < _))
  bind("<=", liftT[NumT, NumT, BoolT](_ <= _))
  bind("max", liftT[NumT, NumT, NumT](Math.max(_,_)))
  bind("min", liftT[NumT, NumT, NumT](Math.min(_,_)))
  bind("sin", liftT[NumT, NumT](Math.sin(_)))
  bind("cos", liftT[NumT, NumT](Math.cos(_)))
  bind("sqrt", liftT[NumT, NumT](Math.sqrt(_)))

  // Vector arithmetic
  bind("+", liftT[PointT, PointT, PointT](_ + _))
  bind("-", liftT[PointT, PointT, PointT](_ - _))
  bind("*", liftT[PointT, NumT, PointT]((a, b) => a * b))
  bind("*", liftT[NumT, PointT, PointT]((a, b) => b * a)) // Commutative
  bind("/", liftT[PointT, NumT, PointT](_ / _))

  // Set arithmetic
  bind("+", liftT[SeqT[AnyT], SeqT[AnyT], SeqT[AnyT]](_ union _))
  bind("-", liftT[SeqT[AnyT], SeqT[AnyT], SeqT[AnyT]](_ difference _))
  bind("&", liftT[SeqT[AnyT], SeqT[AnyT], SeqT[AnyT]](_ intersect _))

  //
  //
  // METHODS
  bindM("x", liftM[PointT, NumT](_.x))
  bindM("y", liftM[PointT, NumT](_.y))
  bindM("distance", liftM[PointT, PointT, NumT]((a, b) => math.sqrt(a.dist(b))))
  bindM("start", liftM[EdgeT, PointT](_.start))
  bindM("end", liftM[EdgeT, PointT](_.end))
  bindM("midpoint", liftM[EdgeT, PointT](_.eval(0.5)))
  bindM("at", liftM[EdgeT, NumT, PointT](_.eval(_)))
  bindM("length", liftM[EdgeT, NumT](_.length))
  bindM("flipped", liftM[EdgeT, EdgeT](_.flipped))
  bindM("center", liftM[EdgeT, PointT] {
    case e: Arc => e.center
    case _: Line => error("Cannot get radial center of a line")
  })

  import elodin.global.api.*
  bindM("x", liftM[Point3T, NumT](_.physicalPoint.x))
  bindM("y", liftM[Point3T, NumT](_.physicalPoint.y))
  bindM("z", liftM[Point3T, NumT](_.physicalPoint.z))
  bindM("distance", liftM[Point3T, Point3T, NumT]((a, b) => a.physicalPoint.dist(b.physicalPoint)))
  bindM("start", liftM[Edge3T, Point3T](_.start))
  bindM("end", liftM[Edge3T, Point3T](_.end))
  bindM("midpoint", liftM[Edge3T, Point3T](_.eval(0.5)))
  bindM("at", liftM[Edge3T, NumT, Point3T](_.eval(_)))

  // Todo: support edges evaluated from faces. In general this is easy for vertical
  // faces and impossible for horizontal faces which are of arbitrary topology.
  // Actually I don't think it'd actually be _that_ hard. You could assign UVs by
  // the bounding box and allow the edge sampling. But it's sort of pointless. Maybe
  // allow edge/polygon intersection to trim an edge, or get polygons split with edges
  // or something

  bindM("points", liftM[PolygonT, SeqT[PointT]](_.points))
  bindM("edges", liftM[PolygonT, SeqT[EdgeT]](_.edges))

  bindM("points", liftM[RegionT, SeqT[PointT]](_.points))
  bindM("edges", liftM[RegionT, SeqT[EdgeT]](_.edges))
  bindM("polygons", liftM[RegionT, SeqT[PolygonT]](_.polygons))
  bindM("positivePolygons", liftM[RegionT, SeqT[PolygonT]](_.positivePolygons))
  bindM("negativePolygons", liftM[RegionT, SeqT[PolygonT]](_.negativePolygons))
  bindM("single", liftM[RegionT, PolygonT](_.single()))

  bindM("center", liftM[CircleT, PointT](_.center()))
  bindM("topLeft", liftM[RectT, PointT](_.topLeft))
  bindM("topRight", liftM[RectT, PointT](_.topRight))
  bindM("bottomLeft", liftM[RectT, PointT](_.bottomLeft))
  bindM("bottomRight", liftM[RectT, PointT](_.bottomRight))
  bindM("center", liftM[RectT, PointT](_.center()))
  bindM("top", liftM[RectT, EdgeT](_.top))
  bindM("left", liftM[RectT, EdgeT](_.left))
  bindM("bottom", liftM[RectT, EdgeT](_.bottom))
  bindM("right", liftM[RectT, EdgeT](_.right))

  // METHODS ON TRANSFORMED POLYGONS. This is one of the largest hacks in the interpreter right now;
  // we are essentially catching method calls on transformed polygons (losing type safety) and inserting
  // a hidden query to fetch the appropriate element out of a stack of arbitrary size of what are really transforms
  inline def getInput[T <: Polygon](t: TransformPolygon[Polygon]): Option[(T, Seq[Transform])] =
    // We have to do this imperatively as opposed to recursively because we cannot recursively inline,
    // and we need to inline for the type tag check from the type argument to check.
    var current = t
    var tfxs = List[Transform]()
    var result = Option.empty[T]
    var continue = true
    while continue do
     current.input match
      case r: T => result = Some(r); continue = false; tfxs = t.tfx :: tfxs
      case i: TransformPolygon[?] =>
        current = i.as[TransformPolygon[Polygon]]
        tfxs = t.tfx :: tfxs
      case other => result = None; continue = false
    result.map(r => (r,tfxs))

  def transformCenter(t: TransformPolygon[Polygon]): Point =
    val (p, tfxs) = getInput[Circle](t)
        .map((c, tfx) => (c.center(), tfx))
        .orElse(getInput[Rectangle](t).map((c, tfx) => (c.center(), tfx)))
        .getOrElse(error(s"$t cannot be cast as Circle or Rect type"))
    tfxs.foldLeft(p)((e,t) => e.transform(t))

  def getElementFrom[A <: Reference, B <: Reference](t: TransformPolygon[Polygon], l: DynList[A], f: Rectangle => B): A =
    single(l.query(from(f(getInput[Rectangle](t).get(0)))))

  bindM("center", liftM[TransformPolyT, PointT](transformCenter(_)))
  bindM("topLeft", liftM[TransformPolyT, PointT](r => getElementFrom(r,r.points,_.topLeft)))
  bindM("topRight", liftM[TransformPolyT, PointT](r => getElementFrom(r,r.points,_.topRight)))
  bindM("bottomLeft", liftM[TransformPolyT, PointT](r => getElementFrom(r,r.points,_.bottomLeft)))
  bindM("bottomRight", liftM[TransformPolyT, PointT](r => getElementFrom(r,r.points,_.bottomRight)))
  bindM("top", liftM[TransformPolyT, EdgeT](r => getElementFrom(r,r.edges,_.top)))
  bindM("left", liftM[TransformPolyT, EdgeT](r => getElementFrom(r,r.edges,_.left)))
  bindM("bottom", liftM[TransformPolyT, EdgeT](r => getElementFrom(r,r.edges,_.bottom)))
  bindM("right", liftM[TransformPolyT, EdgeT](r => getElementFrom(r,r.edges,_.right)))

  bindM("top", liftM[ExtrudeCurveT, EdgeT](_.oppositeEdge))
  bindM("back", liftM[ExtrudeCurveT, EdgeT](_.backEdge))
  bindM("bottom", liftM[ExtrudeCurveT, EdgeT](_.currentEdge))
  bindM("front", liftM[ExtrudeCurveT, EdgeT](_.frontEdge))

  bindM("negative", liftM[InsetEdgeT, ExtrudeCurveT](_.area))
  bindM("positive", liftM[ExtrudeEdgeT, ExtrudeCurveT](_.area))

  bindM("points", liftM[SolidT, SeqT[Point3T]](_.points))
  bindM("basePoints", liftM[SolidT, SeqT[Point3T]](_.basePoints))
  bindM("outerPoints", liftM[SolidT, SeqT[Point3T]](_.outerPoints))
  bindM("edges", liftM[SolidT, SeqT[Edge3T]](_.edges))
  bindM("verticalEdges", liftM[SolidT, SeqT[Edge3T]](_.verticalEdges))
  bindM("baseEdges", liftM[SolidT, SeqT[Edge3T]](_.baseEdges))
  bindM("outerEdges", liftM[SolidT, SeqT[Edge3T]](_.outerEdges))
  bindM("verticalFaces", liftM[SolidT, SeqT[Face3T]](_.verticalFaces))
  bindM("faces", liftM[SolidT, SeqT[Face3T]](_.faces))
  bindM("outerFace", liftM[SolidT, Face3T](_.outerFace))
  bindM("baseFace", liftM[SolidT, Face3T](_.baseFace))

  // Transforms
  bindM("translate", liftTransform[NumT, NumT]((x, y) => Transform.Translate(pt(x, y))))
  bindM("translate", liftTransform[PointT](p => Transform.Translate(p.duplicate())))
  bindM("rotate", liftTransform[PointT, NumT]((p,t) => Transform.Rotate(p.duplicate(), t)))
  bindM("rotateDeg", liftTransform[PointT, NumT]((c, deg) => Transform.Rotate(c.duplicate(), deg2rad(deg))))
  bindM("scale", liftTransform[PointT, NumT]((p,f)  => Transform.Scale(p.duplicate(), f)))
  bindM("scale", liftTransform[PointT, PointT]((p,f)  => Transform.ScaleNU(p.duplicate(), f)))
  bindM("mirror", liftTransform[EdgeT](l => Transform.Mirror(Axis(l))))
  bindM("mirror", liftTransform[PointT, PointT]((s, e) => Transform.Mirror(Axis(s, e))))


  bindM("translate", liftTransform3[NumT, NumT, NumT](Translate(_,_,_)))
  bindM("translate", liftTransform3[PointT, NumT]((p,z) => Translate(p.x,p.y,z)))
  bindM("translateX", liftTransform3[NumT](Translate(_,0,0)))
  bindM("translateY", liftTransform3[NumT](Translate(0,_,0)))
  bindM("translateZ", liftTransform3[NumT](Translate(0,0,_)))
  bindM("move", liftTransform3[Point3T, Point3T](Transform3D.Move(_,_)))
  bindM("move", liftTransform3[PointT, PointT](Transform3D.Move(_,_)))
  bindM("rotate", liftTransform3[Point3T, Point3T, NumT](Transform3D.RotateAxis(_,_,_)))
  bindM("rotate", liftTransform3[Edge3T, NumT](Transform3D.RotateAxis(_,_)))
  bindM("rotate", liftTransform3[PointT, NumT]((pt, t) => Rotate(Axis3D.ZAt(pt), t)))
  bindM("rotateX", liftTransform3[NumT](Rotate(Axis3D.X,_)))
  bindM("rotateY", liftTransform3[NumT](Rotate(Axis3D.Y,_)))
  bindM("rotateZ", liftTransform3[NumT](Rotate(Axis3D.Z,_)))
  bindM("rotateDeg", liftTransform3[Point3T, Point3T, NumT]((p,q,t) => Transform3D.RotateAxis(p,q, deg2rad(t))))
  bindM("rotateDeg", liftTransform3[Edge3T, NumT]((p, t) => Transform3D.RotateAxis(p, deg2rad(t))))
  bindM("rotateDeg", liftTransform3[PointT, NumT]((pt, t) => Rotate(Axis3D.ZAt(pt), deg2rad(t))))
  bindM("rotateXDeg", liftTransform3[NumT](r => Rotate(Axis3D.X,deg2rad(r))))
  bindM("rotateYDeg", liftTransform3[NumT](r => Rotate(Axis3D.Y,deg2rad(r))))
  bindM("rotateZDeg", liftTransform3[NumT](r => Rotate(Axis3D.Z,deg2rad(r))))


  // format: on

  def execQueryF(args: Seq[DynObj]) =
    val Seq((set, seqT), (queryBody, QueryT)) = args: @unchecked
    val result = kernel_time(set.as[DynList[Reference]].query(queryBody.as[Query]))
    (result, seqT)
  // TODO: More concise predicate syntax for this
  def queryMatch(args: Seq[VarType]) =
    args match
      case Seq(SeqT(t), QueryT) => true
      case _                    => false
  bindF("query", execQueryF, queryMatch)

  def primitiveQueryF(queryBuilder: Seq[Reference] => Query) =
    (args: Seq[DynObj]) =>
      val referenceArgs = args.flatMap((a, t) =>
        t match
          case SeqT(_) => a.as[DynList[Reference]].list
          case _       => Seq(a.as[Reference])
      )
      (queryBuilder(referenceArgs), QueryT)

  // Arguments to queries must either be geometry or collections thereof
  def primitiveQueryMatch(args: Seq[VarType]) =
    args.forall {
      case SeqT(t) => isGeoType(t)
      case t       => isGeoType(t)
    }

  bindF("derivedFrom", primitiveQueryF(args => Queries.fromAll(args*)), primitiveQueryMatch)
  bindF("from", primitiveQueryF(args => Queries.directFromAll(args*)), primitiveQueryMatch)
  bindF("derivedFromAll", primitiveQueryF(args => Queries.fromAll(args*)), primitiveQueryMatch)
  bindF("fromAll", primitiveQueryF(args => Queries.directFromAll(args*)), primitiveQueryMatch)
  bindF("derivedFromAny", primitiveQueryF(args => Queries.fromAny(args*)), primitiveQueryMatch)
  bindF("fromAny", primitiveQueryF(args => Queries.directFromAny(args*)), primitiveQueryMatch)

  // Composite queries are composed of any number of subqueries
  def compositeQuery(queryBuilder: Seq[Query] => Query) =
    (args: Seq[DynObj]) => (queryBuilder(args.map((q, _) => q.as[Query])), QueryT)
  def compositeQueryMatch(args: Seq[VarType]) = args.forall(a => a == QueryT)
  bindF("or", compositeQuery(args => Queries.or(args*)), compositeQueryMatch)
  bindF("and", compositeQuery(args => Queries.and(args*)), compositeQueryMatch)
  bindF("not", compositeQuery(args => Queries.not(args(0))), a => compositeQueryMatch(a) && a.size == 1)
  bind("all", liftConstant(All, QueryT))
  bindF(
    "contains",
    compositeQuery(args => Queries.contains(All, args(0))),
    argsMatch(QueryT)
  )

  def singleF(args: Seq[DynObj]) =
    val (l, SeqT(t)) = args(0): @unchecked
    (Queries.single(l.as[DynList[Any]]), t)
  def emptyF(args: Seq[DynObj]) =
    args.map(a => Queries.empty(a.obj.as[DynList[Any]]))
    (null, VoidT)

  val singleMatcher = (args: Seq[VarType]) => args match { case Seq(SeqT(t)) => true; case _ => false }
  val emptyMatcher = (args: Seq[VarType]) => args.forall { case SeqT(t) => true; case _ => false }

  bind("single", (singleF, singleMatcher))
  bind("empty", (emptyF, emptyMatcher))

  def buildSetF(args: Seq[DynObj]) =
    val result = args.flatMap(arg =>
      arg.obj match
        case list: DynList[?] => list.list
        case other            => Seq(other)
    )
    (DynList[Any](result), SeqT(unifyTypeListSet(args.map(_.typ))))
  bind("Set", (buildSetF, _ => true))

  def buildSeqF(args: Seq[DynObj]) =
    (DynList[Any](args.map(_.obj), true), SeqT(unifyTypeList(args.map(_.typ))))
  bind("Seq", (buildSeqF, _ => true))

  def tabulateF(args: Seq[DynObj]) =
    // TODO: same constancy requirement as RegularPolygon
    val k = args(0).obj.as[Double]
    val f = args(1).obj.as[NativeTypeOf[LambdaT]]
    val results = 0
      .until(k.toInt)
      .map(i => f(Seq((i.toDouble, NumT))))
    (DynList[Any](results.map(_.obj), true), SeqT(unifyTypeList(results.map(_.typ))))
  bindOp("Tabulate", (tabulateF, argsMatch(NumT, LambdaT)))

  def seqLengthF(rcv: DynObj, args: Seq[DynObj]): DynObj =
    val seq = rcv.obj.as[DynList[Any]]
    if !seq.indexable then error("Cannot evaluate length of non-indexable sequence")
    else (seq.list.length.toDouble, NumT)

  bindM("length", (SeqT(AnyT), seqLengthF, argsMatch()))

  def mapF(rcv: DynObj, args: Seq[DynObj]) =
    val SeqT(eltT) = rcv.typ: @unchecked
    val rcvObj = rcv.obj.as[DynList[Any]]
    val seq = rcvObj.list.map(obj => (obj, eltT))
    val f = args(0).obj.as[NativeTypeOf[LambdaT]]
    val result = seq.map(e =>
      val y = f(Seq(e))
      if isReferenceType(e.obj) && isReferenceType(y.obj) then userDefinedLineage(e.obj, y.obj)
      y
    )
    (DynList(result.map(_.obj), rcvObj.indexable), SeqT(unifyTypeList(result.map(_.typ))))
  bindMOp("map", (SeqT(AnyT), mapF, argsMatch(LambdaT)))

  def filterF(rcv: DynObj, args: Seq[DynObj]) =
    val SeqT(eltT) = rcv.typ: @unchecked
    val seq = rcv.obj.as[DynList[Any]].list.map(obj => (obj, eltT))
    val f = args(0).obj.as[NativeTypeOf[LambdaT]]
    val result = seq.filter(o =>
      f(Seq(o)).obj match
        case t: Double  => t >= 0
        case t: Boolean => t
        case t          => error(s"Could not convert object $t to boolean in predicate result")
    )
    val unified = unifyTypeList(result.map(_.typ))
    val t = if unified == VoidT then eltT else unified
    (DynList(result.map(_.obj)), SeqT(t))
  bindMOp("filter", (SeqT(AnyT), filterF, argsMatch(LambdaT)))

  def flattenF(rcv: DynObj, args: Seq[DynObj]) =
    var indexable = true
    def recFlatten(l: DynList[Any], t: VarType): (Seq[Any], VarType) =
      t match
        case SeqT(SeqT(eltT)) =>
          indexable = indexable && l.indexable
          val ls = l.list.map(e => recFlatten(e.as[DynList[Any]], SeqT(eltT)))
          val eType = ls.headOption.map(_(1)).getOrElse(AnyT)
          (ls.flatMap(_(0)), eType)
        case SeqT(eltT) =>
          indexable = indexable && l.indexable
          (l.list, eltT)
        case _ => error(s"Cannot flatten non-sequence type $t")
    val (flatList, flatTyp) = recFlatten(rcv.obj.as[DynList[Any]], rcv.typ)
    (DynList[Any](flatList, indexable), SeqT(flatTyp))

  bindM("flatten", (SeqT(AnyT), flattenF, argsMatch()))

  def colorF(styleF: String => Style) =
    (args: Seq[Expression]) =>
      val getStyle = args(0) match
        case Variable(c) => styleF(c)
        case arg         => error(s"Invalid color expression: ${arg.show}")
      (DynList(Seq(getStyle)), SeqT(StyleT))

  // Styling uses bindExpr to directly read variable names as string literals
  bindMacro("color", colorF(Style.Fill.apply))
  bindMacro("fill", colorF(Style.Fill.apply))
  bindMacro("stroke", colorF(Style.Stroke.apply))

  bindM("typ", (AnyT, (obj, _) => (obj.typ.toString(), AnyT), argsMatch()))

  init_finished = true

case class ExecutionStats(
    runMS: Double,
    queryMS: Double,
    kernelMS: Double,
    numOps: Int,
    numQueries: Int,
    numSegs: Int,
    numFaces: Int,
    numTraversals: Int,
    vertsExpanded: Int,
    lineageVerts: Int,
    lineageEdges: Int
):
  def show(): Seq[String] =
    Seq(
      f"${numOps}",
      f"${numQueries}",
      f"${lineageVerts}",
      f"${lineageEdges}",
      f"${runMS}%.2fms",
      f"${queryMS}%.2fms",
      f"${kernelMS}%.2fms",
      f"${numSegs}",
      f"${numFaces}",
      f"${numTraversals}",
      f"${vertsExpanded}"
    )

  def values(): Buffer[Double] =
    Buffer(
      numOps,
      numQueries,
      lineageVerts,
      lineageEdges,
      runMS,
      queryMS,
      kernelMS,
      numSegs,
      numFaces,
      numTraversals,
      vertsExpanded
    )

// ACTUAL INTERPRETER
class DynamicExecutor(text: String, wrapper: HTMLElement = document.body, renderer: Renderer)
    extends elodin.opt.UserProgram[ExecutionStats](wrapper, renderer):

  var currentText = ""
  var parameters = Vector[Var]()
  var programBody = Seq[AST]()
  updateProgram(text)

  var DEBUG_DRAW = false

  enum ErrorType { case ParseError; case RuntimeError; }

  var ERROR_TYPE: Option[ErrorType] = None
  var errorDialog: Option[ErrorDialog] = None

  def clearError() =
    errorDialog match
      case Some(dialog) => dialog.delete(); errorDialog = None
      case None         => ()

  def setError(message: String, errorType: ErrorType) =
    ERROR_TYPE = Some(errorType)
    if !errorDialog.isDefined then errorDialog = Some(ErrorDialog(wrapper))
    errorDialog.get.setText(message)

  def tryParse(text: String): Option[Seq[AST]] =
    try
      val r = parse(text)
      ERROR_TYPE match
        case Some(ErrorType.ParseError) => clearError()
        case _                          => ()
      Some(r)
    catch
      case e: Throwable =>
        console.warn(e)
        // Do some janky parsing of the exception message to fix the parse error
        // and put it in the dialog in a way the thing doesn't complain.
        val msg = e.getMessage()
        val lines = msg.linesIterator.toBuffer
        if lines.length > 0 && lines(0).startsWith("<input>:") then
          val sp = lines(0).split(":").toBuffer
          sp(0) = sp(0).replace("<input>", "input")
          if sp.size >= 2 then sp(1).toIntOption.map(i => sp(1) = s"${i - 1}")
          lines(0) = sp.mkString(":")
        val text = lines.mkString("<br>")
        setError(text, ErrorType.ParseError)
        None

  def updateProgram(text: String): Boolean =
    if text == currentText then false
    else
      tryParse(text) match
        case Some(Seq(paramBlock, body*)) =>
          parameters = paramBlock match
            case ParameterStmt(ps) =>
              ps.map((name, bounds) => Var(bounds.mid, name, bounds.min, bounds.max)).toVector
            case _ => throw new Exception("Missing parameter block")
          programBody = body
          currentText = text
          true
        case _ => false

  def execute(input: Vector[Var])(using renderContext: Renderer, cc: ConstraintContext) =
    import Interpreter.*
    val start = window.performance.now()
    val heap = new Heap()
    DEBUG_DRAW = false

    heap.update(
      "KERNEL",
      Heap.makeObject(
        liftBuiltin[NumT, VoidT]("point_size", renderContext.setPointSize(_)),
        liftBuiltin[VoidT]("toggle_debug_draw", () => DEBUG_DRAW = !DEBUG_DRAW)
      )
    )

    for v <- input do heap.update(v.name, (v.value, NumT))
    val expressionStack = collection.mutable.Stack[Expression]()
    val drawBuffer = collection.mutable.Buffer[(Renderable, Seq[Style])]()

    val edgeStyle = Seq(Style.Fill("red"))
    val pointStyle = Seq(Style.Fill("blue"))

    Context.reset()
    // println(s"GLOBAL OPERATION GRAPH = ${Context.currentGraph}")

    Kernel.dbgPrint = (s: String) => println(s"\t$s")
    if renderContext.willDraw then
      Kernel.draw = (a: Any, color: String) =>
        if DEBUG_DRAW then
          val style =
            if color == "DEBUG" then Seq(Style.Fill("red"), Style.Opacity(0.25))
            else Seq(Style.Fill(color))
          a match
            case e: Edge     => drawBuffer += ((e, style))
            case p: Point    => drawBuffer += ((p, style))
            case p: Polygon  => drawBuffer += ((p, style))
            case c: Region2D => drawBuffer += ((c, style))
      Queries.error = (s: String) => error(s)
    clearError()

    def error(msg: String) =
      val lastExpr = expressionStack.top.show
      val allExprs = expressionStack.toSeq
        .takeWhile {
          case _: Block => false
          case _        => true
        }
        .map(_.show)
        .mkString("\n\t")
      val ctxMsg = s"$msg\n(in expression: `$lastExpr`)\n\t$allExprs\n" +
        s"at parameters: ${input.display()}"
      setError(ctxMsg.replace("\n", "<br>").replace("\t", "&nbsp;&nbsp;"), ErrorType.RuntimeError)
      // renderContext.draw(drawBuffer.toSeq) // Draw the error state!
      throw new Exception(ctxMsg)

    //
    // DRAWING & PRINTING ////////////////////////////////////////
    //

    // Drawing pushes to a buffer which is then rendered when the program terminates
    def drawF(args: Seq[DynObj]) =
      if !renderContext.willDraw then (null, VoidT)
      else
        val styles = args.flatMap { (obj, typ) =>
          typ match
            case StyleT       => Seq(obj.as[Style])
            case SeqT(StyleT) => obj.as[DynList[Style]].list
            case _            => Seq()
        }
        // Optimization: might be more efficient if we construct the THREE.Material object once
        // On the other hand, ship it.
        def draw(r: Renderable) = drawBuffer += ((r, styles))
        def drawObj(obj: DynObj): Unit =
          obj match
            case (pt, PointT)                           => draw(pt.as[Point])
            case (edge, EdgeT)                          => draw(edge.as[Edge])
            case (polygon, t) if subtypeOf(t, PolygonT) => draw(polygon.as[Polygon])
            case (region, t) if subtypeOf(t, RegionT)   => draw(region.as[Region2D])
            case (solid, t) if subtypeOf(t, SolidT)     => draw(solid.as[Solid])
            case (vert, Point3T)                        => draw(vert.as[Vertex3D])
            case (edge, Edge3T)                         => draw(edge.as[Edge3D])
            case (face, Face3T)                         => draw(face.as[Face3D])
            case (elts, SeqT(t)) => elts.as[DynList[Any]].list.foreach(e => drawObj((e, t)))
            case (_, StyleT)     => ()
            case (_, typ)        => error(s"Don't know how to draw $typ yet")
        args.foreach(drawObj)
        (null, VoidT)
    def drawMatch(args: Seq[VarType]) =
      args.forall(a =>
        subtypeOf(a, StyleT) || subtypeOf(a, SeqT(StyleT))
          || isGeoType(a) || isSeqTypeOf(isGeoType)(a)
      )
    bindF("draw", drawF, drawMatch)
    def lookupStyle(name: String, args: Seq[Expression]): Style =
      name match
        case "dotted" => Style.Dotted
        case "translucent" =>
          Style.Opacity(
            if args.size > 0 then cast(NumT)(executeExpr(args(0))) else 0.5
          )
        case "highlight" => Style.Fill("#FF7700")
        case "orange"    => Style.Fill("#FF7700")
        case "bg"        => Style.Fill("red")
        case _ =>
          error(s"Unknown style: $name")

    bindMacro(
      "style",
      args =>
        val styles = args.map[Style] {
          case Variable(name)          => lookupStyle(name, Seq())
          case FnCall(name, styleArgs) => lookupStyle(name, styleArgs)
          case arg                     => error(s"Invalid argument to `style`: ${arg.show}")
        }
        (DynList(styles), SeqT(StyleT))
    )

    // Printing uses bindExpr because it needs direct access to the AST in order to print the expression itself.
    bindMacro(
      "print",
      (args: Seq[Expression]) =>
        val a = args.map(executeExpr)
        println(s"[${args.map(_.show).mkString(", ")}] : ${a.map(_(0).toString()).mkString(",")}")
        a.lastOption.getOrElse((null, VoidT))
    )

    bindMacro(
      "println",
      (args: Seq[Expression]) =>
        println(s"[${args.map(_.show).mkString(", ")}] : ")
        val a = args.map(executeExpr)
        a.map((arg, _) =>
          arg match
            case a: Seq[?]     => a.foreach(e => println(s"\t$e"))
            case l: DynList[?] => l.list.foreach(e => println(s"\t$e"))
            case other         => println(s"\t$other")
        )
        a.lastOption.getOrElse((null, VoidT))
    )

    def executeApply(rcv: Expression, args: Seq[Expression]): DynObj =
      val rcvObj = executeExpr(rcv)
      val argObjs = args.map(executeExpr)
      evaluateApply(rcv, rcvObj, argObjs)

    def evaluateApply(rcv: Expression, rcvObj: DynObj, args: Seq[DynObj]) =
      rcvObj match
        case (f, LambdaT) => f.as[Seq[DynObj] => DynObj](args)
        case (s, SeqT(t)) =>
          val seq = s.as[DynList[Any]]
          if argsMatch(NumT)(args.map(_.typ)) && seq.indexable then
            (seq.list(args(0).obj.as[Double].toInt), t)
          else if !seq.indexable then error(s"Sequence ${rcv.show} is not indexable.")
          else error(s"Cannot index ${rcv.show} with types ${formatArgTypes(args.map(_.typ))}")
        case other => error(s"Invalid application to type ${rcvObj.typ}")

    def executeFunction(name: String, args: Seq[Expression]) =
      // Prefer builtin macros which are unambiguous and pre-type
      findMacro(name) match
        case Some(f) => f(args)
        case None    =>
          // If there are none (common case) then look at the standard functions
          val evalArgs = args.map(executeExpr)
          val argTypes = evalArgs.map(_.typ)
          findFunction(name, argTypes) match
            case Some(f) => f(evalArgs)
            case None =>
              heap.get(name) match
                case Some(obj) => evaluateApply(Variable(name), obj, evalArgs)
                case _ =>
                  error(
                    s"No function of name $name matches argument signature:\n\t${formatArgTypes(argTypes)}"
                  )

    def executeMethod(name: String, rcv: Expression, args: Seq[Expression]): DynObj =
      // We could somewhat niftily do UFCS here by taking failed method lookups and doing them function style,
      // or taking failed function lookups and doing them method style. This would eliminate the distinction
      // but it would be unclear how the matching would work with type-distance the way we're doing it now
      val evalRcv = executeExpr(rcv)
      val evalArgs = args.map(executeExpr)

      if evalRcv.typ == ObjectT then // This is a dynamic, user-defined object
        evalRcv.obj.as[Heap.Binding].get(name) match
          case Some((f, LambdaT))              => f.as[Seq[DynObj] => DynObj](evalArgs)
          case Some(obj) if evalArgs.size == 0 => obj
          case _ =>
            val argTypes = evalArgs.map(_.typ)
            error(s"No binding $name exists on object $name with signature ${formatArgTypes(argTypes)}")
      else // Use a static method we know about, on a type we know about
        findMethod(name, evalRcv.typ, evalArgs.map(_.typ)) match
          case Right(f) => f(evalRcv, evalArgs)
          case Left(methodResolveErr) => // No such method, Access a field instead
            findMethod(name, evalRcv.typ, Seq()) match
              case Left(_)  => error(methodResolveErr) // No such field
              case Right(f) => evaluateApply(MethodCall(rcv, name, args), f(evalRcv, Seq()), evalArgs)

    // Ideally this should return an interpretation specialized to this program
    // that we can continue to use until the program changes.
    def executeExpr(expr: Expression): DynObj =
      expressionStack.push(expr)
      val result = expr match
        case Variable(name) =>
          heap.getOrElse(name, error(s"Unknown variable $name"))

        case Literal(value) => (value, NumT)
        case Assignment(target, rhs) =>
          var result = executeExpr(rhs)
          heap.update(target, result)
          result

        case UnopExpr(symbol, rhs)       => executeFunction(symbol, Seq(rhs))
        case BinopExpr(symbol, lhs, rhs) => executeFunction(symbol, Seq(lhs, rhs))
        case FnCall(name, args)          => executeFunction(name, args)
        case MethodCall(rcv, name, args) => executeMethod(name, rcv, args)
        case Apply(rcv, args)            => executeApply(rcv, args)
        case Block(exprs)                => exprs.map(executeExpr).lastOption.getOrElse((null, VoidT))

        // Cactus stacks:
        // Bindings inside the lambda are invisible to those outside, and scope is fully captured
        // at the time of lambda definition; later changes to bindings are ignored.
        case Lambda(argNames, body) =>
          val currentBindings = heap.bindings
          val resultFn = (args: Seq[DynObj]) =>
            heap.pushScope(currentBindings)
            for (name, arg) <- argNames.zip(args) do heap.update(name, arg)
            val result = executeExpr(body)
            heap.popScope()
            result
          (resultFn, LambdaT)

        // Right now this is sugar for what is essentially a CLASS definiition, not a function one;
        // and we want method lookup to work by dynamically accessing the interior scope if this is
        // a user defined type. We will (later) want to expose the lineage API to use WITHIN operation
        // components only.
        case FnDefn(name, argNames, body) =>
          val currentBindings = heap.bindings
          val resultFn = (args: Seq[DynObj]) =>
            heap.pushScope(currentBindings)
            for (name, arg) <- argNames.zip(args) do heap.update(name, arg)
            val _ = executeExpr(Block(body)) // There is no result for operation definitions
            val resultBindings = heap.bindings.head // Capture the top-level definitions in this scope
            heap.popScope()
            (resultBindings, ObjectT)
          val resultObj = (resultFn, LambdaT)
          heap.update(name, resultObj)
          resultObj

      expressionStack.pop()
      result

    var lastResult: DynObj = (null, VoidT)
    for clause <- programBody do
      clause match
        case e: Expression =>
          lastResult = executeExpr(e)
        case _ =>
          println(s"Unknown Result: $clause")

    val end = window.performance.now()
    val totalTime = end - start
    println(f"[Executed in $totalTime%.2fms]")

    val drawStart = window.performance.now()
    renderContext.draw(drawBuffer.toSeq)
    val drawEnd = window.performance.now()
    val (vertsExpanded, numTraversals, v, e) = Context.lineageMap.summary()

    val queryTime = Context.query_ms
    val kernelTime = Context.kernel_ms - queryTime
    val overHead = totalTime - Context.kernel_ms
    val renderTime = drawEnd - drawStart
    println(
      f"Queries: $queryTime%.2fms | Kernel: $kernelTime%.2fms | Overhead: $overHead%.2fms | Rendering: $renderTime%.2fms"
    )

    val result = ExecutionStats(
      runMS = totalTime,
      queryMS = queryTime,
      kernelMS = kernelTime,
      numOps = Context.num_operations,
      numQueries = Context.num_queries,
      numFaces = renderContext.num_faces,
      numSegs = renderContext.num_segments,
      vertsExpanded = vertsExpanded,
      numTraversals = numTraversals,
      lineageVerts = v,
      lineageEdges = e
    )
    result

  override val sourceText = Some(text)
