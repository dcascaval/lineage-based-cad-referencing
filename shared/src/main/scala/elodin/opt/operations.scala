package elodin.opt

import collection.mutable
import mutable.Buffer
import elodin.global.api.*

import elodin.geometry.*
import elodin.opt
import Operation.*

object Operations:
  import Kernel.*
  import Kernel.RegionIntersection.*
  import Queries.*

  val counter = MultiCounter()
  val PRINT_REFERENCE = false

  extension (s: Seq[Point])
    def ccw(): Boolean =
      val area2 = s.wrappedPairs
        .map((p, q) => (p.x * q.y) - (q.x * p.y))
        .fold(0.0)(_ + _)
      return area2 > 0

  /** Transform a polygon according to some spatial mapping function over its points.
    *
    * CAVEATS:
    *   - Any transform that FLIPS THE ORIENTATION (so that edges are now going clockwise instead of CCW)
    *     must also rewire the orientation so that booleans will work correctly.
    *   - Transforms on polygons WITH ARCS are limited to: Translations, Rotations, Mirrors, Uniform
    *     scales (essentially: conformal.)
    */
  class TransformPolygon[T <: Polygon](val input: T, val tfx: Transform) extends Polygon with Operation:
    private val doReverse = tfx match
      case _: Transform.Mirror     => true
      case Transform.ScaleNU(_, f) => (f.x < 0) ^ (f.y < 0)
      case _                       => false

    tfx.isInstanceOf[Transform.Mirror]
    extension [A](s: Seq[A])
      def reverseIf() =
        if doReverse then s.reverse else s

    given Operation = this
    val origin = Context.startOp(this)
    private var tempPts = input.points.list.map(_.transform(tfx))

    // Our issue is that "contained elements" are stored as a field on the object
    // and thus getting those to be the "correct" elements requires us to manually replace
    // the end points so that the edges use the vertices of the polygon instead of making their own,
    // so that the vertices end up being shared.
    val edges = zip3(input.edges.list, tempPts, tempPts.rotate(1))
      .map((edge, start, end) =>
        val tEdge = edge match
          case _: Line => Line(start, end)
          case a: Arc =>
            if input.edges.size > 1 then a.transform(tfx).withPoints(start, end)
            else
              val newArc = a.transform(tfx)
              tempPts = Seq(newArc.start)
              newArc
        if doReverse then tEdge.flipped else tEdge
      )
      .reverseIf()
      .asDynList(input.edges.indexable)
    val points = tempPts.reverseIf().asDynList(input.points.indexable)

    Context.endOp(this)
    arguments(input)
    directLineage(input -> Seq(this))
    directLineage(input.points -> points.list.reverseIf().asDynList())
    directLineage(input.edges -> edges.list.reverseIf().asDynList())

  class IntersectCurves(a: Edge, b: Edge) extends Operation:
    given Operation = this
    val origin = Context.startOp(this)

    val pts = DynList(a.intersect(b).map(_.point).toSeq)

    Context.endOp(this)
    arguments(a, b)
    for pt <- pts.list do //
      directLineage(Seq(a, b) -> pt)

  def extrPoints(edge: Edge, vector: Point) =
    Seq(
      edge.end,
      edge.start,
      edge.start + vector,
      edge.end + vector
    )

  def extrudeCurve(edge: Edge, length: Double, dir: Option[Point]): ExtrudeCurve =
    val vector = dir match
      case Some(p) => p
      case None    => (edge.end - edge.start).rotate(-math.Pi / 2)
    ExtrudeCurve(edge, vector.withLength(length), !extrPoints(edge, vector).ccw())

  def extrudeCurveAngle(edge: Edge, length: Double, angle: Double): ExtrudeCurve =
    val vector = (edge.end - edge.start).rotate((math.Pi / 2) + deg2rad(angle))
    ExtrudeCurve(edge, vector.withLength(length), !extrPoints(edge, vector).ccw())

  class ExtrudeCurve(edge: Edge, vector: Point, flip: Boolean) extends Polygon with Operation:
    given Operation = this
    val origin = Context.startOp(this)

    val a = if !flip then edge else edge.flipped
    val oppositeEdge = a.copy().transform(Transform.Translate(vector))
    val currentEdge = a.copy().flipped

    val points: DynList[Point] = Seq(
      currentEdge.start,
      currentEdge.end,
      oppositeEdge.start,
      oppositeEdge.end
    ).asDynList()

    val frontEdge = Line(currentEdge.end, oppositeEdge.start)
    val backEdge = Line(oppositeEdge.end, currentEdge.start)

    val edges = Seq(currentEdge, frontEdge, oppositeEdge, backEdge).asDynList()

    Context.endOp(this)

    directLineage(a.start -> currentEdge.end)
    directLineage(a.end -> currentEdge.start)
    directLineage(a -> currentEdge)
    lineage(a.start -> Seq(frontEdge, frontEdge.end))
    lineage(a.end -> Seq(backEdge, backEdge.start))
    lineage(a -> oppositeEdge)
    arguments(a)

  class InsetEdge(p: Polygon, e: Edge, len: Double, p0: Double, p1: Double)
      extends Region2D
      with Operation:
    given Operation = this
    val origin = Context.startOp(this)

    // Easy mode implementation:
    val newEdge = e.trimParameters(p0, p1).flipped

    val vec = (newEdge.end - newEdge.start).rotate(-math.Pi / 2).withLength(len)
    val area = ExtrudeCurve(newEdge, vec, !extrPoints(newEdge, vec).ccw())

    val polygons = subtractPolygons(p, area).allPolygons()

    // Subtraction, ironically, does not create negative polygons (ever!) since
    // it is always attached to an edge, so there can never be a new completely-contained
    // polygon
    val positivePolygons = polygons
    val negativePolygons = DynList.empty[Polygon]

    Context.endOp(this)
    arguments(p)

  class ExtrudeEdge(p: Polygon, e: Edge, len: Double, p0: Double, p1: Double)
      extends Region2D
      with Operation:
    given Operation = this
    val origin = Context.startOp(this)

    // Easy mode implementation:
    val newEdge = e.trimParameters(p0, p1)

    val vec = (newEdge.end - newEdge.start).rotate(-math.Pi / 2).withLength(len)
    val area = ExtrudeCurve(newEdge, vec, !extrPoints(newEdge, vec).ccw())

    private val innerUnion = UnionPolygons(p, area)
    val polygons = innerUnion.polygons
    val positivePolygons: DynList[Polygon] = innerUnion.positivePolygons
    val negativePolygons: DynList[Polygon] = innerUnion.negativePolygons

    Context.endOp(this)
    arguments(p)

  class UnionPolygons(a: Polygon, b: Polygon, copy: Boolean = true) extends Region2D with Operation:
    given Operation = this
    val origin = Context.startOp(this)

    val polyIX = unionPolygons(a, b, copy)
    val polygons = polyIX.allPolygons()

    val (positivePolygons, negativePolygons) = polyIX.positiveNegative { ambiguous =>
      if ambiguous.size == 1 then (ambiguous, Seq())
      else
        val actualNegatives = ambiguous.filter(p =>
          ambiguous.exists(outerP => outerP != p && Kernel.containsPoint(outerP, p.points.list(0)))
        )
        val actualPositives = ambiguous.filter(p => !actualNegatives.contains(p))
        (actualPositives, actualNegatives)
    }
    Context.endOp(this)
    arguments(a, b)

    // Lineage has been set by the `unionPolygons` function
    override def toString() = s"Union@${hashCode}"

  class UnionAll(polys: Seq[Polygon], copy: Boolean = true) extends Region2D with Operation:
    given Operation = this
    val origin = Context.startOp(this)

    val polyIX = unionKPolygons(polys, copy)
    val polygons = polyIX.allPolygons()

    val (positivePolygons, negativePolygons) = polyIX.positiveNegative { ambiguous =>
      if ambiguous.size == 1 then (ambiguous, Seq())
      else
        val actualNegatives = ambiguous.filter(p =>
          ambiguous.exists(outerP => outerP != p && Kernel.containsPoint(outerP, p.points.list(0)))
        )
        val actualPositives = ambiguous.filter(p => !actualNegatives.contains(p))
        (actualPositives, actualNegatives)
    }

    Context.endOp(this)
    arguments(polys*)

  class IntersectPolygons(a: Polygon, b: Polygon, copy: Boolean = true) extends Region2D with Operation:
    given Operation = this
    val origin = Context.startOp(this)

    val polyIX = intersectPolygons(a, b, copy)
    val polygons = polyIX.allPolygons()

    val (positivePolygons, negativePolygons) = polyIX.positiveNegative { ambiguous =>
      // Intersections produce multiple positives, but never negative polygons.
      (ambiguous, Seq())
    }
    Context.endOp(this)
    arguments(a, b)

    // Lineage has been set by the `unionPolygons` function
    override def toString() = s"Union@${hashCode}"

  class SubtractPolygons(a: Polygon, b: Polygon, copy: Boolean = true) extends Region2D with Operation:
    given Operation = this
    val origin = Context.startOp(this)

    private val polyIX = subtractPolygons(a, b, copy)
    val polygons = polyIX.allPolygons()

    Context.endOp(this)
    arguments(a, b)

    val (positivePolygons, negativePolygons) = polyIX.positiveNegative(ambiguous =>
      // Subtraction will only produce positive polygons in cases of intersection --
      // negative polygons are only produced when polygons are not touching.
      (ambiguous, Seq())
    )

    override def toString() = s"Diff@${hashCode}"

  /** Merge a new poylgon in. Invariant: none of the polygons in either list intersect any of the others
    * in either list.
    */
  def addPolygonToRegion(
      positives: Seq[Polygon],
      negatives: Seq[Polygon],
      newPolygon: Polygon
  ): (Seq[Polygon], Seq[Polygon]) =
    // println(s"Adding $newPolygon to (+ $positives, - $negatives)")
    // The size of these lists is almost always very small (1-4). If it is
    // more than that we will want to look at more efficient algorithms.

    val posResult = Buffer[Polygon]()
    val newNegatives = Buffer[Polygon]()

    var i = 0; var continue = true
    while i < positives.size && continue do
      val pos = positives(i)
      val union = UnionPolygons(pos, newPolygon, copy = false)

      if union.positivePolygons.size == 2 && union.negativePolygons.size == 0 then
        // println(s"[i = $i] non-touching intersection, trying next polygon.")
        // They do not touch, both are positive polygons
        posResult += pos
        i += 1
      else if union.polygons.size == 1 then
        // println(s"[i = $i] intersection to form single poly, merging rest of region.")

        // They intersected to form one polygon
        continue = false
        // Scan through the rest of the list and add that polygon to it.
        val remainingPositives = positives.slice(i + 1, positives.size)
        val (restPositives, restNegatives) =
          addPolygonToRegion(remainingPositives, Seq(), union.polygons.list(0))
        posResult ++= restPositives
        newNegatives ++= restNegatives
      else
        // They intersected into more than one polygon
        continue = false
        // They intersected to create a new set of positive & negative polygons
        //
        // We make two assumptions:
        // -> created negative does not intersect any of the existing negatives
        // -> multiple POSITIVE polygons cannot be created as a result of a union
        if union.positivePolygons.size != 1 then
          error(s"Expected 1 positive polygon from poly-poly union; Invalid assumption.")

        // println(s"[i = $i] intersection to form region, merging rest of region.")
        // We cannot, however, make the assumption that these negative does not intersect any of the
        // other positive polygons, so we will take care to subtract those from these negative.
        val unionPos = union.positivePolygons.list(0)
        val unionNegs = union.negativePolygons.list
        val remainingPositives = positives.slice(i + 1, positives.size)
        val (restPositives, restNegatives) = addPolygonToRegion(remainingPositives, Seq(), unionPos)
        val unionNegsTrimmed = unionNegs.flatMap(neg =>
          SubtractRegions(
            neg.asRegion(),
            MultiPolyRegion(remainingPositives),
            copy = false
          ).positivePolygons.list
        )
        posResult ++= restPositives
        newNegatives ++= unionNegsTrimmed
        newNegatives ++= restNegatives

    if continue then // It didn't intersect any of them; we can safely add it to the positives list.
      posResult += newPolygon

    // Here, we know that all of the negative polygons don't intersect, and subtracting the positive one
    // from them cannot add mass, so they will continue not to intersect (though they may be swallowed)
    val negResult = negatives.flatMap(neg =>
      val diff = SubtractPolygons(neg, newPolygon, false)
      diff.positivePolygons.list // Might have size > 1 if the positive splits it in two.
    // if !diff.intersect then Seq() // Swallowed
    // else diff.allPolygons().list
    )
    (posResult.toSeq, negResult ++ newNegatives.toSeq)

  def copyIfNotIn(polys: Seq[Polygon], otherPolys: Seq[Polygon], owner: Operation & Reference) =
    polys.map(p => if otherPolys.contains(p) then p.copy() else p)

  class UnionRegions(a: Region2D, b: Region2D) extends Region2D with Operation:
    given Operation = this
    val origin = Context.startOp(this)

    val (positivePolygons, negativePolygons) = {
      val startPositive = a.positivePolygons.list
      val startNegative = a.negativePolygons.list
      val bPos = b.positivePolygons.list
      val bNegs = b.negativePolygons.list

      // First add all of the positive polygons to the region, potentially
      // unifying separate polygons, separating negative polygons, or completely
      // swallowing positive or negative polygons.
      val (pos0, neg0) = bPos
        .foldLeft((startPositive, startNegative)) { case ((pos, neg), poly) =>
          addPolygonToRegion(pos, neg, poly)
        }

      // neg0.map(neg => draw(neg, "red"))
      // println("union regions")
      // println(s"pos0 = $pos0")
      // println(s"neg0 = $neg0")

      val pos0Region = MultiPolyRegion(startPositive)
      val negSubPos = bNegs.flatMap(neg =>
        val resultR = SubtractRegions(neg.asRegion(), pos0Region)
        // if resultR.negativePolygons.size > 0 then
        // println(s"Ignoring doubly-negative results of UnionRegions ${resultR.negativePolygons}")
        resultR.positivePolygons.list
      )

      val negAndNeg = for
        aNeg <- a.negativePolygons.list
        bNeg <- b.negativePolygons.list
        poly <- IntersectPolygons(aNeg, bNeg).positivePolygons.list
      yield poly

      // println(s"nsp = $negSubPos")
      // negSubPos.map(neg => draw(neg, "green"))

      // Add all of the negative polygons together
      val neg1 = neg0 ++ negSubPos ++ negAndNeg

      // If a polygon made it through *completely* unchanged: Does that matter?
      // Yes, because we need to   know for a piece of query geometry what the association is.
      //
      // Now; it may be the case that association should actually be a property of
      // *extraction* rather than a single global term; i.e. an object association is
      // created when the reference is accessed, so we can have references to the same
      // piece of geometry in memory that happen to have different associations.

      val positives = copyIfNotIn(pos0, startPositive ++ bPos, this)
      val negatives = copyIfNotIn(neg1, startNegative ++ bNegs, this)

      (dynamicList(positives), dynamicList(negatives))
    }
    val polygons = positivePolygons.union(negativePolygons)

    Context.endOp(this)
    debug_ref(s"Arguments [unionRegions] = $a, $b")
    arguments(a, b)
    override def toString() = s"UnionRegion@${hashCode}"

  class SubtractRegions(a: Region2D, b: Region2D, copy: Boolean = true) extends Region2D with Operation:
    given Operation = this
    val origin = Context.startOp(this)

    val (positivePolygons, negativePolygons) = {
      val doubleNegatives = {
        if (b.negativePolygons.size > 0) then
          // println(s"Ignoring the negative parts of $b ${b.negativePolygons.list}.")
          val aNegRegion = MultiPolyRegion(a.negativePolygons.list)
          val bNegsStillAround = b.negativePolygons.list.flatMap(bNeg =>
            SubtractRegions(bNeg.asRegion(), aNegRegion).positivePolygons.list
          )
          for
            bNeg <- bNegsStillAround
            aPos <- a.positivePolygons.list
            poly <- IntersectPolygons(bNeg, aPos).positivePolygons.list
          yield poly
        else Seq()
      }

      val startNegative = a.negativePolygons.list
      val unionedNegatives = b.positivePolygons.list
        .foldLeft(startNegative)((negs, poly) => addPolygonToRegion(negs, Seq(), poly)(0))

      var pos0 = a.positivePolygons.list
      val neg0 = unionedNegatives.filter(neg =>
        var escaped = false
        pos0 = pos0.flatMap(pos =>
          val diff = SubtractPolygons(pos, neg, copy = false)
          if diff.negativePolygons.size == 0 then
            escaped = true
            diff.polygons.list // Might have generated multiple, since it touched the boundary.
          else Seq(pos)
        )
        !escaped // Keep it only if it didnt touch something else
      )

      inline def clean(inline l: Seq[Polygon], inline originals: Seq[Polygon]) =
        if copy then copyIfNotIn(l, originals, this) else l
      val positives = clean(pos0 ++ doubleNegatives, a.positivePolygons.list)
      val negatives = clean(neg0, startNegative ++ b.positivePolygons.list)
      (dynamicList(positives), dynamicList(negatives))
    }

    val polygons = positivePolygons.union(negativePolygons)
    Context.endOp(this)

    debug_ref(s"Arguments [subtractRegions] = $a, $b")
    arguments(a, b)
    override def toString() = s"DiffRegion@${hashCode}"

  def subtractAll(dest: Region2D, args: Seq[Region2D]): Region2D =
    // This works, but is overly slow for what we are trying to do
    args.foldLeft(dest)((region, next) => SubtractRegions(region, next))

  def subtractAllPolygons(dest: Polygon, args: Seq[Polygon]): Region2D =
    // Plays fast and loose with the invariant that all the positive polys don't intersect;
    // because in the current implementation it doesn't matter if they do and this saves a lotta copies.
    SubtractRegions(dest.asRegion(), UnionAll(args))
  // subtractAll(dest.asRegion(), args.map(_.asRegion()))

  def transformRegion(input: Region2D, tfx: Transform): Region2D =
    new Region2D with Operation {
      val origin = Context.startOp(this)
      given OperationGraph = origin

      val positivePolygons = input.positivePolygons.map(p => TransformPolygon(p, tfx))
      val negativePolygons = input.negativePolygons.map(p => TransformPolygon(p, tfx))
      val polygons = dynamicList(positivePolygons.list ++ negativePolygons.list)

      Context.endOp(this)
      arguments(input)
    }

  /** Chamfer all points in a set on a given initial polygon. This implementation of ChamferAll does not
    * require any of its input points to continue to exist.
    */
  class ChamferAll(
      initial: Polygon,
      chamferVertices: Seq[Point],
      chamferRadius: Double
  ) extends Polygon
      with Operation:

    val origin = Context.startOp(this)
    given Operation = this

    private val (newPoints, newEdges, createdPoints, createdEdges) =
      computeChamfer(initial, chamferVertices, chamferRadius)

    val points = dynamicList(newPoints)
    val edges = dynamicList(newEdges)
    val chamferPoints = createdPoints
    val chamferEdges = createdEdges

    for (point, newVertices) <- createdPoints do //
      lineage(point -> newVertices.toSeq)
    for (point, newEdges) <- createdEdges do //
      lineage(point -> newEdges(1))

    Context.endOp(this)
    arguments((initial +: chamferVertices)*)

    override def toString() = s"Chamfer@${hashCode}"

  class FilletAll(
      initial: Polygon,
      filletVertices: Seq[Point],
      filletRadius: Double
  ) extends Polygon
      with Operation:

    val origin = Context.startOp(this)
    given Operation = this

    private val (newPoints, newEdges, createdPoints, createdEdges) =
      computeFillet(initial, filletVertices, filletRadius)

    val points = dynamicList(newPoints)
    val edges = dynamicList(newEdges)
    val chamferPoints = createdPoints
    val chamferEdges = createdEdges

    for (point, newVertices) <- createdPoints do //
      lineage(point -> newVertices.toSeq)
    for (point, newEdges) <- createdEdges do //
      lineage(point -> newEdges(1))

    Context.endOp(this)
    arguments((initial +: filletVertices)*)

    override def toString() = s"Fillet@${hashCode}"

  // Lineage, at the moment, does not need any notion of an actual operation, since the operations
  // where elements are created is recorded with the element itself, so who assigns the lineage does
  // not matter
  def userDefinedLineage(a: Any, b: Any) =
    (a, b) match
      case (a: Reference, b: Reference)  => lineage(a -> b)
      case (a: DynList[?], b: Reference) => lineage(a.list.as[Seq[Reference]] -> b)
      case (a: Reference, b: DynList[?]) => lineage(a -> b.list.as[Seq[Reference]])
      case (a: DynList[?], b: DynList[?]) =>
        lineage(a.as[DynList[Reference]] -> b.as[DynList[Reference]])

  //
  // ERGONOMICS HELPERS FOR PLOTTING AND INITIALIZATION
  //

  def pt(x: Double, y: Double) = Point(x, y, orphan = true)

  def zip3[A, B, C](as: Seq[A], bs: Seq[B], cs: Seq[C]): Seq[(A, B, C)] =
    as.zip(bs.zip(cs)).map((a, bc) => (a, bc(0), bc(1)))

  def zip4[A, B, C, D](as: Seq[A], bs: Seq[B], cs: Seq[C], ds: Seq[D]): Seq[(A, B, C, D)] =
    as.zip(bs).zip(cs.zip(ds)).map((ab, cd) => (ab(0), ab(1), cd(0), cd(1)))

  def zip5[A, B, C, D, E](
      as: Seq[A],
      bs: Seq[B],
      cs: Seq[C],
      ds: Seq[D],
      es: Seq[E]
  ): Seq[(A, B, C, D, E)] =
    as.zip(bs).zip(cs.zip(ds)).zip(es).map { case ((ab, cd), e) => (ab(0), ab(1), cd(0), cd(1), e) }
