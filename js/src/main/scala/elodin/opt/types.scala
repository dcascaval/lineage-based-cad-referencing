package elodin.opt

import collection.mutable.{Map, Buffer}

import elodin.global.api.*
import elodin.geometry.*
import api.*
import dsl.*

sealed trait VarType:
  override def toString(): String =
    formatType(this)

case object AnyT extends VarType
case object VoidT extends VarType
case object NumT extends VarType
case object BoolT extends VarType

case object PointT extends VarType
case object EdgeT extends VarType
case object Point3T extends VarType
case object Edge3T extends VarType
case object Face3T extends VarType

case object PolygonT extends VarType
case object RectT extends VarType
case object CircleT extends VarType
case object TransformPolyT extends VarType
case object ExtrudeCurveT extends VarType

case object RegionT extends VarType
case object InsetEdgeT extends VarType
case object ExtrudeEdgeT extends VarType

case object ExtrudeT extends VarType
case object RevolveT extends VarType
case object SolidT extends VarType

case object TransformT extends VarType
case object Transform3T extends VarType
case object TransformableT extends VarType
case object Transformable3T extends VarType

case object QueryT extends VarType
case object StyleT extends VarType
case object LambdaT extends VarType
case object ObjectT extends VarType

case class SeqT[T <: VarType](typ: T) extends VarType

type DynObj = (Any, VarType)

extension (o: DynObj)
  def obj = o(0)
  def typ = o(1)

type NativeTypeOf[T <: VarType] = T match
  case AnyT.type            => Any
  case VoidT.type           => Unit
  case NumT.type            => Double
  case BoolT.type           => Boolean
  case PointT.type          => Point
  case EdgeT.type           => Edge
  case Point3T.type         => Vertex3D
  case Edge3T.type          => Edge3D
  case Face3T.type          => Face3D
  case PolygonT.type        => Polygon
  case RectT.type           => Rectangle
  case CircleT.type         => Circle
  case TransformPolyT.type  => TransformPolygon[Polygon]
  case ExtrudeCurveT.type   => ExtrudeCurve
  case RegionT.type         => Region2D
  case InsetEdgeT.type      => InsetEdge
  case ExtrudeEdgeT.type    => ExtrudeEdge
  case SolidT.type          => Solid
  case QueryT.type          => Query
  case TransformT.type      => Transform
  case Transform3T.type     => Transform3D
  case TransformableT.type  => Transformable
  case Transformable3T.type => Transformable3D
  case SeqT[a]              => DynList[NativeTypeOf[a]]
  case LambdaT.type         => Seq[DynObj] => DynObj
  case ObjectT.type         => Heap.Binding

  case Any => Any

type AnyT = AnyT.type
type VoidT = VoidT.type
type BoolT = BoolT.type
type NumT = NumT.type
type PointT = PointT.type
type EdgeT = EdgeT.type
type Point3T = Point3T.type
type Edge3T = Edge3T.type
type Face3T = Face3T.type
type PolygonT = PolygonT.type
type RectT = RectT.type
type CircleT = CircleT.type
type TransformPolyT = TransformPolyT.type
type ExtrudeCurveT = ExtrudeCurveT.type
type RegionT = RegionT.type
type InsetEdgeT = InsetEdgeT.type
type ExtrudeEdgeT = ExtrudeEdgeT.type
type SolidT = SolidT.type
type RevolveT = RevolveT.type
type ExtrudeT = ExtrudeT.type
type TransformT = TransformT.type
type Transform3T = Transform3T.type
type TransformableT = TransformableT.type
type Transformable3T = Transformable3T.type
type LambdaT = LambdaT.type
type ObjectT = ObjectT.type

given AnyT.type = AnyT
given VoidT.type = VoidT
given BoolT.type = BoolT
given NumT.type = NumT
given PointT.type = PointT
given EdgeT.type = EdgeT
given Point3T.type = Point3T
given Edge3T.type = Edge3T
given Face3T.type = Face3T
given PolygonT.type = PolygonT
given RectT.type = RectT
given CircleT.type = CircleT
given TransformPolyT.type = TransformPolyT
given ExtrudeCurveT.type = ExtrudeCurveT
given RegionT.type = RegionT
given InsetEdgeT.type = InsetEdgeT
given ExtrudeEdgeT.type = ExtrudeEdgeT
given SolidT.type = SolidT
given RevolveT.type = RevolveT
given ExtrudeT.type = ExtrudeT
given TransformT.type = TransformT
given Transform3T.type = Transform3T
given TransformableT.type = TransformableT
given Transformable3T.type = Transformable3T
given LambdaT.type = LambdaT
given ObjectT.type = ObjectT

given [T <: VarType](using t: T): SeqT[T] = SeqT(t)

def formatType(t: VarType): String =
  t match
    case _: AnyT            => "Any"
    case _: VoidT           => "Void"
    case _: BoolT           => "Boolean"
    case _: NumT            => "Number"
    case _: PointT          => "Point"
    case _: EdgeT           => "Edge"
    case _: Point3T         => "Point3"
    case _: Edge3T          => "Edge3"
    case _: Face3T          => "Face3"
    case _: PolygonT        => "Polygon"
    case _: RectT           => "Rectangle"
    case _: CircleT         => "Circle"
    case _: TransformPolyT  => "TransformPolygon"
    case _: ExtrudeCurveT   => "ExtrudeCurve"
    case _: RegionT         => "Face"
    case _: InsetEdgeT      => "InsetEdge"
    case _: ExtrudeEdgeT    => "ExtrudeEdge"
    case _: SolidT          => "Solid"
    case _: RevolveT        => "Revolve"
    case _: ExtrudeT        => "Extrude3"
    case _: TransformT      => "Transform"
    case _: Transform3T     => "Transform3"
    case _: TransformableT  => "Transformable"
    case _: Transformable3T => "Transforable3"
    case _: LambdaT         => "Lambda"
    case _: ObjectT         => "Object"
    case _: QueryT.type     => "Query"
    case _: StyleT.type     => "Style"
    case SeqT(t)            => s"Set(${formatType(t)})"

def formatArgTypes(argTypes: Seq[VarType]) =
  "(" + argTypes.map(formatType).mkString(", ") + ")"

val subTypeGraph = {
  val graph = collection.mutable.Map[VarType, VarType]() // Graph edge pointing upwards to supertypes
  def subtype(b: VarType, a: VarType) =
    graph(b) = a

  val geoTypes = Seq(PointT, EdgeT, PolygonT, RegionT)
  val geo3Types = Seq(SolidT, Point3T, Edge3T, Face3T)
  val otherTypes =
    Seq(NumT, TransformT, LambdaT, QueryT, BoolT, SeqT(AnyT), TransformableT, Transformable3T)

  subtype(RectT, PolygonT)
  subtype(CircleT, PolygonT)
  subtype(TransformPolyT, PolygonT)
  subtype(ExtrudeCurveT, PolygonT)
  subtype(ExtrudeEdgeT, RegionT)
  subtype(InsetEdgeT, RegionT)

  subtype(ExtrudeT, SolidT)
  subtype(RevolveT, SolidT)

  for t <- geoTypes do subtype(t, TransformableT)
  for t <- geo3Types do subtype(t, Transformable3T)
  for t <- otherTypes do subtype(t, AnyT)

  graph
}

def subtypeOf(b: VarType, a: VarType): Boolean =
  (b, a) match
    case (_, AnyT)                  => true
    case (SeqT(bType), SeqT(aType)) => subtypeOf(bType, aType) // covariant
    case (_, _) =>
      b == a || (subTypeGraph.get(b) match
        case None    => false
        case Some(c) => subtypeOf(c, a)
      )

def subtypeDistance(b: VarType, a: VarType): Option[Int] =
  def rec(b: VarType, a: VarType, acc: Int): Option[Int] =
    (b, a) match
      case (_, AnyT)                  => Some(acc + 1)
      case (SeqT(bType), SeqT(aType)) => rec(bType, aType, acc)
      case (_, _) =>
        if b == a then Some(acc)
        else subTypeGraph.get(b).flatMap(c => rec(c, a, acc + 1))
  rec(b, a, 0)

/** lift() is a wrapper that lifts a function that operates on native scala types to the two functions
  * that the interpreter needs in order to use the function. This includes:
  *   - A wrapped version of the function that lowers and lifts the virtual types via casts
  *   - A type predicate that takes in an argument list and determines whether this function is a match
  *     for said arglist; or there is a type error.
  */

def liftT[A <: VarType, R <: VarType](f: NativeTypeOf[A] => NativeTypeOf[R]) =
  (aType: A, rType: R) ?=>
    val function = (args: Seq[DynObj]) =>
      val a = cast(aType)(args(0)).as[NativeTypeOf[A]]
      (kernel_time(f(a)), rType)
    val matcher = argsMatch(aType)
    (function, matcher)

def liftT[A <: VarType, B <: VarType, R <: VarType](
    f: (NativeTypeOf[A], NativeTypeOf[B]) => NativeTypeOf[R]
) =
  (aType: A, bType: B, rType: R) ?=>
    val function = (args: Seq[DynObj]) =>
      val a = cast(aType)(args(0)).as[NativeTypeOf[A]]
      val b = cast(bType)(args(1)).as[NativeTypeOf[B]]
      (kernel_time(f(a, b)), rType)
    val matcher = argsMatch(aType, bType)
    (function, matcher)

def liftT[A <: VarType, B <: VarType, C <: VarType, R <: VarType](
    f: (NativeTypeOf[A], NativeTypeOf[B], NativeTypeOf[C]) => NativeTypeOf[R]
) =
  (aType: A, bType: B, cType: C, rType: R) ?=>
    val function = (args: Seq[DynObj]) =>
      val a = cast(aType)(args(0)).as[NativeTypeOf[A]]
      val b = cast(bType)(args(1)).as[NativeTypeOf[B]]
      val c = cast(cType)(args(2)).as[NativeTypeOf[C]]
      (kernel_time(f(a, b, c)), rType)
    val matcher = argsMatch(aType, bType, cType)
    (function, matcher)

def liftT[A <: VarType, B <: VarType, C <: VarType, D <: VarType, R <: VarType](
    f: (NativeTypeOf[A], NativeTypeOf[B], NativeTypeOf[C], NativeTypeOf[D]) => NativeTypeOf[R]
) =
  (aType: A, bType: B, cType: C, dType: D, rType: R) ?=>
    val function = (args: Seq[DynObj]) =>
      val a = cast(aType)(args(0)).as[NativeTypeOf[A]]
      val b = cast(bType)(args(1)).as[NativeTypeOf[B]]
      val c = cast(cType)(args(2)).as[NativeTypeOf[C]]
      val d = cast(dType)(args(3)).as[NativeTypeOf[D]]
      (kernel_time(f(a, b, c, d)), rType)
    val matcher = argsMatch(aType, bType, cType, dType)
    (function, matcher)

def liftT[A <: VarType, B <: VarType, C <: VarType, D <: VarType, E <: VarType, R <: VarType](
    f: (
        NativeTypeOf[A],
        NativeTypeOf[B],
        NativeTypeOf[C],
        NativeTypeOf[D],
        NativeTypeOf[E]
    ) => NativeTypeOf[R]
) =
  (aType: A, bType: B, cType: C, dType: D, eType: E, rType: R) ?=>
    val function = (args: Seq[DynObj]) =>
      val a = cast(aType)(args(0)).as[NativeTypeOf[A]]
      val b = cast(bType)(args(1)).as[NativeTypeOf[B]]
      val c = cast(cType)(args(2)).as[NativeTypeOf[C]]
      val d = cast(dType)(args(3)).as[NativeTypeOf[D]]
      val e = cast(eType)(args(4)).as[NativeTypeOf[E]]
      (kernel_time(f(a, b, c, d, e)), rType)
    val matcher = argsMatch(aType, bType, cType, dType, eType)
    (function, matcher)

def liftBuiltin[R <: VarType](name: String, f: () => NativeTypeOf[R]) =
  (rType: R) ?=>
    val matcher = argsMatch()
    val function = (args: Seq[DynObj]) =>
      if matcher(args.map(_.typ)) then (kernel_time(f()), rType)
      else
        error(
          s"Builtin function $name does not match arguments (Expected: (), got ${args.map(_.typ)}"
        )
    (name, (function, LambdaT))

def liftBuiltin[A <: VarType, R <: VarType](name: String, f: NativeTypeOf[A] => NativeTypeOf[R]) =
  (aType: A, rType: R) ?=>
    val matcher = argsMatch(aType)
    val function = (args: Seq[DynObj]) =>
      if matcher(args.map(_.typ)) then
        val a = cast(aType)(args(0)).as[NativeTypeOf[A]]
        (kernel_time(f(a)), rType)
      else
        error(
          s"Builtin function $name does not match arguments (Expected: $aType, got ${args.map(_.typ)}"
        )
    (name, (function, LambdaT))

def liftM[L <: VarType, R <: VarType](f: NativeTypeOf[L] => NativeTypeOf[R]) =
  (t: L, rType: R) ?=>
    val function = (rcv: DynObj, args: Seq[DynObj]) =>
      val r = cast(t)(rcv).as[NativeTypeOf[L]]
      (f(r), rType)
    val matcher = (args: Seq[VarType]) => args.size == 0
    (t, function, matcher)

def liftM[L <: VarType, A <: VarType, R <: VarType](
    f: (NativeTypeOf[L], NativeTypeOf[A]) => NativeTypeOf[R]
) =
  (t: L, aType: A, rType: R) ?=>
    val function = (rcv: DynObj, args: Seq[DynObj]) =>
      val r = cast(t)(rcv).as[NativeTypeOf[L]]
      val a = cast(aType)(args(0)).as[NativeTypeOf[A]]
      (f(r, a), rType)
    val matcher = argsMatch(aType)
    (t, function, matcher)

def liftConstant[A, Typ <: VarType](constant: A, t: Typ) =
  val function = (args: Seq[DynObj]) => (constant, t)
  val matcher = (args: Seq[VarType]) => true
  (function, matcher)

inline def kernel_time[T](inline f: T) =
  val startTime = System.nanoTime()
  // if Context.tracking_kernel_time then
  //   error("RECURSIVE KERNEL TIME TRACKING")
  // Context.tracking_kernel_time = true
  val result = f
  Context.kernel_ms += ((System.nanoTime() - startTime).toDouble * 1e-6)
  // Context.tracking_kernel_time = false
  result

def transformedType(a: VarType) =
  if subtypeOf(a, TransformableT) && subtypeOf(a, PolygonT) then TransformPolyT else a

def liftTransform[A <: VarType](transformF: NativeTypeOf[A] => Transform) =
  (aType: A) ?=>
    def liftedTransform(rcv: DynObj, args: Seq[DynObj]) =
      val obj = cast(TransformableT)(rcv)
      val a = cast(aType)(args(0))
      (kernel_time(obj.transform(transformF(a))), transformedType(rcv.typ))
    (TransformableT, liftedTransform, argsMatch(aType))

def liftTransform[A <: VarType, B <: VarType](
    transformF: (NativeTypeOf[A], NativeTypeOf[B]) => Transform
) =
  (aType: A, bType: B) ?=>
    def liftedTransform(rcv: DynObj, args: Seq[DynObj]) =
      val obj = cast(TransformableT)(rcv)
      val a = cast(aType)(args(0))
      val b = cast(bType)(args(1))
      (kernel_time(obj.transform(transformF(a, b))), transformedType(rcv.typ))
    (TransformableT, liftedTransform, argsMatch(aType, bType))

def liftTransform[A <: VarType, B <: VarType, C <: VarType](
    transformF: (NativeTypeOf[A], NativeTypeOf[B], NativeTypeOf[C]) => Transform
) =
  (aType: A, bType: B, cType: C) ?=>
    def liftedTransform(rcv: DynObj, args: Seq[DynObj]) =
      val obj = cast(TransformableT)(rcv)
      val a = cast(aType)(args(0))
      val b = cast(bType)(args(1))
      val c = cast(cType)(args(2))
      (kernel_time(obj.transform(transformF(a, b, c))), transformedType(rcv.typ))
    (TransformableT, liftedTransform, argsMatch(aType, bType, cType))

def liftTransform3[A <: VarType](transformF: NativeTypeOf[A] => Transform3D) =
  (aType: A) ?=>
    def liftedTransform(rcv: DynObj, args: Seq[DynObj]) =
      val obj = cast(Transformable3T)(rcv)
      val a = cast(aType)(args(0))
      (kernel_time(obj.withTransform(transformF(a))), transformedType(rcv.typ))
    (Transformable3T, liftedTransform, argsMatch(aType))

def liftTransform3[A <: VarType, B <: VarType](
    transformF: (NativeTypeOf[A], NativeTypeOf[B]) => Transform3D
) =
  (aType: A, bType: B) ?=>
    def liftedTransform(rcv: DynObj, args: Seq[DynObj]) =
      val obj = cast(Transformable3T)(rcv)
      val a = cast(aType)(args(0))
      val b = cast(bType)(args(1))
      (kernel_time(obj.withTransform(transformF(a, b))), transformedType(rcv.typ))
    (Transformable3T, liftedTransform, argsMatch(aType, bType))

def liftTransform3[A <: VarType, B <: VarType, C <: VarType](
    transformF: (NativeTypeOf[A], NativeTypeOf[B], NativeTypeOf[C]) => Transform3D
) =
  (aType: A, bType: B, cType: C) ?=>
    def liftedTransform(rcv: DynObj, args: Seq[DynObj]) =
      val obj = cast(Transformable3T)(rcv)
      val a = cast(aType)(args(0))
      val b = cast(bType)(args(1))
      val c = cast(cType)(args(2))
      (kernel_time(obj.withTransform(transformF(a, b, c))), transformedType(rcv.typ))
    (Transformable3T, liftedTransform, argsMatch(aType, bType, cType))

def argsMatch(fnArgs: VarType*)(appliedArgs: Seq[VarType]) =
  fnArgs.length == appliedArgs.length &&
    fnArgs.zip(appliedArgs).forall((f, a) => subtypeOf(a, f))

def isGeoType(t: VarType) =
  subtypeOf(t, TransformableT) || subtypeOf(t, Transformable3T)

def isReferenceType(t: Any): Boolean =
  t match
    case a: Reference => true
    case b: DynList[?] =>
      b.list.size == 0 || b.list(0).isInstanceOf[Reference]
    case d: Tuple2[?, ?] => isReferenceType(d(0)) // Probably a DynObj
    case _               => false

/** Accepts Seq[T], Seq[Seq[T]], etc. for any T such that f(T) */
def isSeqTypeOf(f: VarType => Boolean)(t: VarType): Boolean =
  t match
    case SeqT(t) => isSeqTypeOf(f)(t)
    case t       => f(t)

type WrappedFn = Seq[DynObj] => DynObj
type WrappedMethod = (DynObj, Seq[DynObj]) => DynObj

type LiftedFn = (WrappedFn, Seq[VarType] => Boolean)
// ( Receiver, Method, ArgTypes )
type LiftedMethod = (VarType, (DynObj, Seq[DynObj]) => DynObj, Seq[VarType] => Boolean)

transparent inline def cast[T <: VarType](inline expectedType: T): DynObj => NativeTypeOf[T] =
  (obj: DynObj) =>
    val (evaluated, typ) = obj
    if !subtypeOf(typ, expectedType) then error(s"Type error [Expected $expectedType, got $typ]")
    else evaluated.asInstanceOf[NativeTypeOf[T]]

// Iterate until we find the upper-bounding type, defaulting to Any if no such type exists.
// There are some niche cases here (nested sequences) that may not infer correctly because
// there is no corresponding edge in the subtype graph, this is almost certainly fine
def unifyTypes(b: VarType, a: VarType): VarType =
  if subtypeOf(b, a) then a
  else
    (b, a) match
      case (SeqT(bElt), SeqT(aElt)) => SeqT(unifyTypes(bElt, aElt))
      case (_, _) =>
        subTypeGraph.get(a) match
          case Some(t) => unifyTypes(b, t)
          case None    => AnyT

def unifyTypeList(types: Seq[VarType]): VarType =
  types match
    case Seq()  => VoidT
    case Seq(t) => t
    case Seq(t, ts*) =>
      ts.foldLeft(t)((upperBound, typ) => unifyTypes(typ, upperBound))

def unifyTypeListSet(types: Seq[VarType]): VarType =
  def unsequence(t: VarType): VarType =
    t match
      case SeqT(t) => t
      case _       => t
  unifyTypeList(types.map(unsequence))
