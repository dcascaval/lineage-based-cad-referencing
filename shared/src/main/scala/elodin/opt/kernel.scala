package elodin.opt

import collection.mutable
import collection.mutable.Buffer
import elodin.opt.Operation.*

import elodin.global.api.*
import elodin.geometry.*

object Kernel:
  import Operations.*
  import Queries.*

  val TOLERANCE = 1e-8

  var draw: (Any, String) => Unit = (a: Any, color: String) => ()
  var dbgPrint: String => Unit = (s: String) => ()

  //
  inline def closestParameterOnLine(start: Point, end: Point, point: Point): Double =
    val s = start.to2; val e = end.to2; val p = point.to2
    val l2 = (s - e).lengthSquared
    if (l2 <= TOLERANCE) then 0.0
    else ((p - s).dot(e - s)) / l2

  inline def inBounds(inline min: Double, inline max: Double, inline value: Double): Boolean =
    min <= value && value <= max

  // Pure geometry, no reference tracking.
  def intersectLines(p0: Point, p1: Point, p2: Point, p3: Point): Buffer[IntersectionResult] =
    import elodin.global.api.*

    val s1 = p1.to2 - p0.to2
    val s2 = p3.to2 - p2.to2

    val d = -s2.x * s1.y + s1.x * s2.y

    // If the two lines are parallel, we return the end points of their interval of intersection
    if math.abs(d) < TOLERANCE then
      val result = Buffer[IntersectionResult]()

      inline def addIntervalPoint(p0: Point, p1: Point, pt: Point, first: Boolean, end: Boolean) =
        val t = closestParameterOnLine(p0, p1, pt)
        val pp0 = p0.to2; val pp1 = p1.to2
        if (pp0 + ((pp1 - pp0) * t)).distSquared(pt.to2) <= TOLERANCE then
          val constParam: Double = if end then 1.0 else 0.0
          val (pA, pB) = if first then (constParam, t) else (t, constParam)
          if inBounds(0.0, 1.0, pA) && inBounds(0.0, 1.0, pB) then
            result += IntersectionResult(pt.duplicate(), pA, pB)
      // Optimization: slightly wasteful redundant comparisons of paramA when paramA is constant
      // Geometrically there will never be more than two of these unless the lines share an endpoint
      // TODO: if they do do that, we probably don't want to return an intersection event for that endpoint.
      //       let's put together a test case for it.
      addIntervalPoint(p2, p3, p0, first = true, end = false)
      addIntervalPoint(p2, p3, p1, first = true, end = true)
      addIntervalPoint(p0, p1, p2, first = false, end = false)
      addIntervalPoint(p0, p1, p3, first = false, end = true)
      result
    else
      val sn = -s1.y * (p0.x - p2.x) + s1.x * (p0.y - p2.y)
      val s = sn / d // Optimization: For ray intersections, don't need this division, just a sign check.
      val tn = s2.x * (p0.y - p2.y) - s2.y * (p0.x - p2.x)
      val t = tn / d

      val resultPoint = Point(p0.x + t * s1.x, p0.y + t * s1.y)
      Buffer(IntersectionResult(resultPoint, t, s))

  inline def order(a: Double, b: Double) =
    if a <= b then (a, b) else (b, a)

  inline def inBoundEps(x: Double, a: Double, b: Double) =
    a - TOLERANCE <= x && x <= b + TOLERANCE

  extension (p: Point)
    inline def polarRadius: Double = math.sqrt((p.x * p.x) + (p.y * p.y))
    inline def polarTheta: Double = math.atan2(p.y, p.x)
    inline def toPolar: Point = Point(polarRadius, polarTheta)

  def intersectCircleCircle(c0: Point, r0: Double, c1: Point, r1: Double): Option[Buffer[Point]] =
    val addRad = r0 + r1
    val invRad = math.abs(r0 - r1)

    val d2 = c0.dist(c1)

    // Todo: do this within some tolerance
    if d2 == 0.0 then
      // Infinitely-many solutions. This... can definitely occur.
      // In practice; what do we do here? Have to delegate to the caller.
      // In our application, we will have two arcs that we are intersecting.
      // The correct behavior in this case is actually to get the boundary points,
      // the same way that we do with lines.
      // We return a None option to denote this case explicitly to the caller.
      if r0 == r1 then None
      // No solutions.
      return Some(Buffer[Point]())

    val d = math.sqrt(d2)
    if d < invRad || d > addRad then //
      // No solutions, too far apart
      return Some(Buffer[Point]())

    val r02 = r0 * r0
    val a = (r02 - r1 * r1 + d2) / (d * 2.0)

    val cc0 = c0.to2
    val cc1 = c1.to2
    val vec = cc1 - cc0 // Duplicative with .dist() above
    val mid = cc0 + vec * (a / d)

    // Circles are tangent and touch in one point
    if math.abs(a - r0) <= TOLERANCE then return Some(Buffer[Point](Point(mid.x, mid.y)))

    val h = math.sqrt(r02 - a * a)
    val offsetY = h * (vec.y) / d
    val offsetX = h * (vec.x) / d

    val ax = mid.x + offsetY; val ay = mid.y - offsetX
    val bx = mid.x - offsetY; val by = mid.y + offsetX
    Some(Buffer[Point](Point(ax, ay), Point(bx, by)))

  def intersectSegmentCircle(start: Point, end: Point, r: Double): Either[Buffer[Point], Buffer[Point]] =
    // Get the implicit form of the line Ax + By - C = 0
    val a: Double = end.y - start.y
    val b: Double = start.x - end.x
    val c: Double = (end.x * start.y) - (start.x * end.y)

    val c2 = c * c
    val r2 = r * r
    val denom = (a * a + b * b)
    val r2d = r2 * denom

    // No Intersections
    val result = Buffer[Point]()
    if (c2 >= (r2d + TOLERANCE)) || (denom == 0.0) then //
      if log then println(s"\t\t\t\tno intersections [c2: $c2  r2d: $r2d denom: $denom]")
      return Right(result)

    val x0 = (-a * c / denom)
    val y0 = (-b * c / denom)

    val (minx, maxx) = order(start.x, end.x)
    val (miny, maxy) = order(start.y, end.y)

    // We know the point is on the line, so if it's in the bounding box,
    // it's on the segment, and we can safely return it.
    inline def addResult(p: Point): Unit =
      if log then println(s"\t\t\t\ttrying $p [x: $minx:$maxx; y: $miny:$maxy]")
      if inBoundEps(p.x, minx, maxx) && inBoundEps(p.y, miny, maxy) then //
        result += p

    // Tangent intersection
    if math.abs(c2 - r2d) < TOLERANCE then //
      if log then println("\t\t\t\ttangent intersection")
      addResult(Point(x0, y0))
      Left(result)
    else
      // Puncture intersection
      if log then println("\t\t\t\tpuncture intersection")
      val d = r2 - c2 / denom
      val mult = math.sqrt(d / denom)
      val am = a * mult; val bm = b * mult
      val ax = x0 + bm; val bx = x0 - bm
      val ay = y0 - am; val by = y0 + am
      addResult(Point(ax, ay))
      addResult(Point(bx, by))
      Right(result)

  // OPTIMIZE: no need to check tt/nt if OOB
  inline def biIntervalParameter(t: Double, t0: Double, t1: Double): Option[Double] =
    val tt = t + tau; val nt = t - tau
    if t0 <= t && t <= t1 then Some((t - t0) / (t1 - t0))
    else if t0 <= tt && tt <= t1 then Some((tt - t0) / (t1 - t0))
    else if t0 <= nt && nt <= t1 then Some((nt - t0) / (t1 - t0))
    else if t1 <= t && t <= t0 then Some((t - t0) / (t1 - t0))
    else if t1 <= tt && tt <= t0 then Some((tt - t0) / (t1 - t0))
    else if t1 <= nt && nt <= t0 then Some((nt - t0) / (t1 - t0))
    else None

  // Todo: the second sqrt is redundant if we have the line itself
  inline def lineParameter(pt: Point, start: Point, end: Point): Double =
    val s = start.to2
    (pt.to2 - s).length / (end.to2 - s).length

  def intersectSegmentArc(
      start: Point,
      end: Point,
      center: Point,
      r: Double,
      theta0: Double,
      theta1: Double
  ): Buffer[IntersectionResult] =
    // Treat the arc as a circle for the purpose of intersections. Note; these
    // points are normalized in circle-space, not world-space.
    val ixresult = intersectSegmentCircle(start - center, end - center, r)
    val lcx = ixresult match
      case Left(l)  => l
      case Right(r) => r

    // println(s"lcx: $lcx")
    val result = lcx
      .flatMap(pt =>
        // Find the parameter on the arc, if it is on the arc.
        val ap = pt.polarTheta // .mod(tau)
        // println(s"\t$pt->$ap")
        biIntervalParameter(ap, theta0, theta1)
          // And finally, move them back to world-space.
          .map(arcParameter =>
            val result = pt + center
            IntersectionResult(result, lineParameter(result, start, end), arcParameter)
          )
      )
    // println(s"r: $result")
    result

  def intersectArcArc(
      c0: Point,
      r0: Double,
      start0: Double,
      end0: Double,
      c1: Point,
      r1: Double,
      start1: Double,
      end1: Double
  ): Buffer[IntersectionResult] =
    val ccx = intersectCircleCircle(c0, r0, c1, r1)
    if !ccx.isDefined then
      println(s"CCX: start: $start0->$end0 | end: $start1-$end1")
      return Buffer()

    // Hmm, do we need to differentiate this? I don't *think* so; it's used later on
    // only in the boolean decision of how to sort it / whether to include it at all,
    // the point value itself is the only thing that actually matters.
    inline def angleOf(pt: Point, center: Point) = (pt - center).polarTheta //.mod(tau)

    for
      pt <- ccx.get
      a <- biIntervalParameter(angleOf(pt, c0), start0, end0)
      b <- biIntervalParameter(angleOf(pt, c1), start1, end1)
    yield IntersectionResult(pt, a, b)

  def intersectSegments(p0: Point, p1: Point, p2: Point, p3: Point): Buffer[IntersectionResult] =
    intersectLines(p0, p1, p2, p3).flatMap(ix =>
      val IntersectionResult(pt, paramA, paramB) = ix
      val condition = inBounds(0.0, 1.0, paramA) && inBounds(0.0, 1.0, paramB)
      if condition then Some(ix) else None
    )

  def intersectSegmentRay(p0: Point, p1: Point, p2: Point, dir: Point): Buffer[IntersectionResult] =
    intersectLines(p0, p1, p2, p2 + dir).flatMap(ix =>
      val IntersectionResult(pt, paramA, paramB) = ix
      val condition = inBounds(0.0, 1.0, paramA) && paramB >= 0.0
      if condition then Some(ix) else None
    )

  // Dereference lines into real, double values
  inline def concretizeLines(input: Seq[Line]): Seq[(Double, Double, Double, Double)] =
    input.map(l => (l.start.x, l.start.y, l.end.x, l.end.y))

  def containsPoly(a: Polygon, b: Polygon): Boolean =
    // Is B inside A? We know that:
    // - Their bounding boxes DO touch
    // - They do not intersect in any way
    if b.edges.size == 0 then error(s"Cannot test degenerate polygon $b")
    else containsPoint(a, b.edges.list(0).eval(0.5))

  var log = false

  /* Test containment quickly, returns a boolean result; not anything trackable. */
  def containsPoint(polygon: Polygon, pp: Point): Boolean =
    if !polygon.bbox.containsPoint(pp.x, pp.y) then return false

    // Algorithm: Pick some ray that hopefully isn't parallel to any of the segments.
    // There is at least one remaining issue here:
    //  - Point p could be very very close to segment, resulting in numerical issue.
    //    Not a clear fix for this: formally correct algorithm would report "doesn't know" when
    //    within machine epsilon. It is unclear what to do in this case, but given the segments
    //    we are testing this is unlikely; you would need a vertex (e.g. an interior corner) on
    //    one polygon that is very close but not quite touching another. We err on the side of reporting
    //    false, since if such a vertex exists and intersections reported false already then that is
    //    consistent. Unclear if tangent arc case affects this.
    val px = pp.x
    val py = pp.y

    // We have made it more robust, but there is still one edge case:
    // - It's possible that the new ray is tangent to some arc, which we are not accounting for
    val vecAngles = polygon.edges.list
      .map(e => (e, (e.start.x - px, e.start.y - py).diamondAngle))
      .sortBy(_(1))
      .toBuffer

    val vec = {
      // If we pick a static ray, the ray could go through one or more vertices.
      // Instead, we can test relative diamond angle against all vertices, and
      // then construct a ray that selects the midpoint of the largest gap, i.e.
      // guaranteeing it will be as far from a vertex as it can be.
      var largestGap = 0.0; var targetAngle = 0.0
      var i = 0; var n = vecAngles.size
      while (i + 1) < n do
        val dAngle = vecAngles(i + 1)(1) - vecAngles(i)(1)
        if dAngle > largestGap then
          largestGap = dAngle
          targetAngle = vecAngles(i)(1) + (dAngle * 0.5)
        i += 1
      val dAngle = (4 + vecAngles(0)(1)) - vecAngles(i)(1)
      if dAngle > largestGap then targetAngle = (vecAngles(i)(1) + (dAngle * 0.5)) % 4.0

      inline def dist1D(a: Double, b: Double): Double =
        math.abs(b - a)

      // Ray need not go farther than this length to intersect all polygon edges
      // compute ray direction and final ray from diamond angle
      val dx = dist1D(2.0, targetAngle) - 1 // -1 to 1
      val dy =
        if targetAngle > 2 then -(1.0 - dist1D(3.0, targetAngle))
        else 1.0 - dist1D(targetAngle, 1.0)

      val maxLen = polygon.bbox.width + polygon.bbox.height
      // draw(Line(pp, pp + pt(dx, dy).withLength(maxLen)), "yellow")
      // println(s"[$pp] TargetAngle = $targetAngle ($dx, $dy) | ${vecAngles.map(_(1))}")
      (dx, dy).withLength(maxLen)
    }

    // if log then println(s"\t\ttesting $pp?")
    var hits = 0
    for edge <- polygon.edges.list do
      val ix = edge match
        case l: Line =>
          val sx = l.start.x; val sy = l.start.y
          val ex = l.end.x; val ey = l.end.y
          val r = intersectSegmentRayConcrete(sx, sy, ex, ey, px, py, px + vec.x, py + vec.y)
          if log then println(s"\t\t\t$pp x $l [$r]")
          if r then hits += 1
        case a: Arc =>
          val r =
            intersectArcRayConcrete(pp, Point(vec.x, vec.y), a.center, a.radius, a.theta0, a.theta1)
          if log then println(s"\t\t\t$pp x $a [hits = $r]")
          hits += r
    if log then println(s"\t\thits = $hits")
    hits % 2 != 0

  // A concrete version of the function `intersectSegmentRay`. While that is written in a
  // much nicer functional style, this one is *significantly* more performant,
  // and we will use it for boolean intersection checks.
  private def intersectSegmentRayConcrete(
      p0x: Double,
      p0y: Double,
      p1x: Double,
      p1y: Double,
      p2x: Double,
      p2y: Double,
      p3x: Double,
      p3y: Double
  ): Boolean =
    val s1x = p1x - p0x
    val s1y = p1y - p0y
    val s2x = p3x - p2x
    val s2y = p3y - p2y
    val d = -s2x * s1y + s1x * s2y
    if math.abs(d) < TOLERANCE then
      if log then print("\t\t\t\t lines parallel, no hit")
      return false
    val sn = -s1y * (p0x - p2x) + s1x * (p0y - p2y)
    // val s = sn / d // Eliding this, doing sign check
    val tn = s2x * (p0y - p2y) - s2y * (p0x - p2x)
    val t = tn / d
    if log then println(s"\t\t\t\t sn = $sn | d = $d | t = $t")
    return ((sn >= 0.0) == (d > 0.0)) && t >= 0.0 && t <= 1.0
  // return ((sn >= 0.0) == (d >= 0.0)) && t >= TOLERANCE && t <= (1.0 - TOLERANCE)

  /** Likewise, when we are checking containment, we want to check if a ray intersects an arc an odd
    * number of times (true), or an even number (false). For this we could use an algorithm that is
    * faster because it does not have to calculate the point positions. (TODO: Do that.)
    */
  def intersectArcRayConcrete(
      start: Point,
      vector: Point,
      center: Point,
      r: Double,
      theta0: Double,
      theta1: Double
  ): Int =
    val end = start + vector
    val lcx = intersectSegmentCircle(start - center, end - center, r)
    // if log then println(lcx)
    lcx match
      case Left(l) => 0
      case Right(r) =>
        r.flatMap(pt =>
          if log then println(s"\t\t\t\tpArc = ${pt.polarTheta}")
          biIntervalParameter(pt.polarTheta /*.mod(tau)*/, theta0, theta1)
        ).size

  object RegionIntersection:
    inline def intersect(a: Edge, b: Edge): Buffer[IntersectionResult] = a.intersect(b)

    // Here, each point returned should actually be *both* the start point of each line
    // *and* the end point of the previous line, so we merge their parents.
    private def applyPolygonReferences(
        allEdges: Seq[Seq[Edge]]
    ) =
      allEdges.map(edges =>
        val startPoints = edges.map(_.start)
        // We want to unify the points so that neighboring edges share their intersecting vertex, to satisfy
        // the polygon data invariant. However we can't simply discard some of the (duplicate) points, as that
        // would lose important lineage information. Instead we want to merge that information into the point.
        val endPoints = edges.map(_.end)
        val newEndPoints = startPoints.rotate(1)
        directLineage(endPoints -> newEndPoints)

        val newEdges = zip3(edges, startPoints, newEndPoints).map((edge, s, e) => edge.withPoints(s, e))

        val result = new Polygon {
          val origin = Context.currentGraph
          val points = startPoints.asDynList()
          val edges = newEdges.asDynList()
        }
        result
      )

    type EdgePoint = (Point, Double)
    extension (inline p: EdgePoint)
      inline def point = p(0)
      inline def param = p(1)

    type Intersections = mutable.Map[Edge, Buffer[(Point, Double)]]

    def debugSplit(ix: Intersections, splitA: Seq[Edge], splitB: Seq[Edge]) =
      ix.foreach((_, v) => v.foreach(ix => draw(ix.point.duplicate(), "purple")))
      splitA.foreach(e => draw(e.copy(), "red"))
      splitA.foreach(e => draw(e.start.copy(), "orange"))
      splitB.foreach(e => draw(e.copy(), "blue"))
      splitB.foreach(e => draw(e.start.copy(), "green"))
    // println(splitA)
    // println(splitB)

    def debugMerge(edgesOnBoth: Seq[Edge], keepEdges: Seq[Edge]) =
      edgesOnBoth.foreach(e => draw(e.eval(0.5).copy(), "green"))
      keepEdges.foreach(e => draw(e.copy(), "red"))

    // Could model this as an ADT also; Might do that later.
    class PolyIntersection(
        val intersect: Boolean,
        val positives: Seq[Polygon],
        val negatives: Seq[Polygon],
        val ambiguous: Seq[Polygon]
    ):
      def allPolygons() =
        (positives ++ negatives ++ ambiguous).asDynList()

      def positiveNegative(sortAmbiguous: Seq[Polygon] => (Seq[Polygon], Seq[Polygon])) =
        val (ambiPos, ambiNeg) = sortAmbiguous(ambiguous)
        ((positives ++ ambiPos).asDynList(), (negatives ++ ambiNeg).asDynList())

    object PolyIntersection:
      def Positive(positive: Polygon*) =
        PolyIntersection(false, positive, Seq(), Seq())
      def PositiveNegative(positive: Polygon, negative: Polygon) =
        PolyIntersection(false, Seq(positive), Seq(negative), Seq())
      def Ambiguous(ambiguous: Seq[Polygon]) =
        PolyIntersection(true, Seq(), Seq(), ambiguous)

    def unionKPolygons(polys: Seq[Polygon], copy: Boolean = true): PolyIntersection =
      val (ixs, _) = allintersectionsK(polys)
      if ixs.size > 0 then
        val splits = polys.map(splitPolyWithIntersections(_, ixs))
        val result = mergeUnionK(splits, polys)
        PolyIntersection.Ambiguous(applyPolygonReferences(result))
      else PolyIntersection.Positive(polys*)

    def unionPolygons(a: Polygon, b: Polygon, copy: Boolean = true): PolyIntersection =
      inline def clean(inline p: Polygon) = if copy then p.copy() else p
      if !a.bbox.intersects(b.bbox) then
        // B cannot be inside or touching A in any way
        PolyIntersection.Positive(clean(a), clean(b))
      else
        val ix = allIntersections(a, b)
        if ix.size > 0 then
          val splitA = splitPolyWithIntersections(a, ix)
          val splitB = splitPolyWithIntersections(b, ix)
          // debugSplit(ix, splitA, splitB)
          val result = mergeUnion(splitA, splitB, a, b)
          PolyIntersection.Ambiguous(applyPolygonReferences(result))
        else
        // Their bboxes intersect, but they do not:
        if containsPoly(a, b) then // A swallows B
          PolyIntersection.Positive(clean(a))
        else if containsPoly(b, a) then // B swallows A
          PolyIntersection.Positive(clean(b))
        else // Free
          PolyIntersection.Positive(clean(a), clean(b))

    def subtractPolygons(a: Polygon, b: Polygon, copy: Boolean = true): PolyIntersection =
      inline def clean(inline p: Polygon) = if copy then p.copy() else p
      if !a.bbox.intersects(b.bbox) then
        // B cannot be inside or touching A in any way
        PolyIntersection.Positive(clean(a))
      else
        val ix = allIntersections(a, b)
        if ix.size > 0 then // We actually have to boolean
          val splitA = splitPolyWithIntersections(a, ix)
          val splitB = splitPolyWithIntersections(b, ix)
          // debugSplit(ix, splitA, splitB)
          val result = mergeDifference(splitA, splitB, a, b)
          PolyIntersection.Ambiguous(applyPolygonReferences(result))
        else
        // Their bboxes intersect but they do not
        if containsPoly(a, b) then // A swallows B, B becomes directly negative
          PolyIntersection.PositiveNegative(clean(a), clean(b))
        else if containsPoly(b, a) then PolyIntersection.Positive() // B swallows A, no result
        else // Free
          PolyIntersection.Positive(clean(a))

    def intersectPolygons(a: Polygon, b: Polygon, copy: Boolean = true): PolyIntersection =
      inline def clean(inline p: Polygon) = if copy then p.copy() else p
      if !a.bbox.intersects(b.bbox) then
        // B cannot be inside or touching A in any way
        PolyIntersection.Positive()
      else
        val ix = allIntersections(a, b)
        if ix.size > 0 then // We actually have to boolean
          val splitA = splitPolyWithIntersections(a, ix)
          val splitB = splitPolyWithIntersections(b, ix)
          // debugSplit(ix, splitA, splitB)
          val result = mergeIntersection(splitA, splitB, a, b)
          PolyIntersection.Ambiguous(applyPolygonReferences(result))
        else
        // Their bboxes intersect but they do not
        if containsPoly(a, b) then PolyIntersection.Positive(clean(b)) // A swallows B => B
        else if containsPoly(b, a) then PolyIntersection.Positive(clean(a)) // B swallows A => A
        else PolyIntersection.Positive() // Free

    def allIntersections(a: Polygon, b: Polygon) =
      val edgeIxMap = mutable.Map[Edge, Buffer[EdgePoint]]()
      for aEdge <- a.edges.list do
        for bEdge <- b.edges.list do
          for p <- intersect(aEdge, bEdge) do
            directLineage(Seq(aEdge, bEdge) -> p.point)
            edgeIxMap.getOrElseUpdate(aEdge, Buffer()) += ((p.point, p.paramA))
            edgeIxMap.getOrElseUpdate(bEdge, Buffer()) += ((p.point, p.paramB))
      edgeIxMap

    def allintersectionsK(polys: Seq[Polygon]) =
      val edgeIXMap = mutable.Map[Edge, Buffer[EdgePoint]]()
      val polyIXRecord = mutable.Set[Polygon]() // This polygon intersects non-zero other polygons
      for (aPoly, i) <- polys.zipWithIndex do
        for bPoly <- polys.drop(i + 1) do
          if aPoly.bbox.intersects(bPoly.bbox) then
            for aEdge <- aPoly.edges.list do
              for bEdge <- bPoly.edges.list do
                val ixs = intersect(aEdge, bEdge)
                for p <- ixs do
                  directLineage(Seq(aEdge, bEdge) -> p.point)
                  edgeIXMap.getOrElseUpdate(aEdge, Buffer()) += ((p.point, p.paramA))
                  edgeIXMap.getOrElseUpdate(bEdge, Buffer()) += ((p.point, p.paramB))
                if ixs.size != 0 then
                  polyIXRecord += aPoly
                  polyIXRecord += bPoly
      (edgeIXMap, polyIXRecord)

    def extractPointsFromEdge(
        intersections: Intersections,
        edge: Edge
    ) =
      val result = intersections
        .getOrElse(edge, Buffer())
        .dedupDouble(_.param, TOLERANCE)
        // .filter(x => f(x) >= TOLERANCE && f(x) <= 1.0 - TOLERANCE)
        .sortBy(_.param)
      result

    inline def getSplitPairs(
        start: Point,
        end: Point,
        ix: Buffer[EdgePoint]
    ) =
      val splitPoints = Buffer[(Point, Double)]()
      if ix(0).param <= TOLERANCE then directLineage(start -> ix(0).point)
      else splitPoints += ((start.copy(), 0.0))
      splitPoints ++= ix
      if ix.last.param >= 1.0 - TOLERANCE then directLineage(end -> ix.last.point)
      else splitPoints += ((end.copy(), 1.0))
      splitPoints

    def equalEdges(e1: Edge, e2: Edge): Boolean =
      (e1, e2) match
        case (e1: Line, e2: Line) =>
          e1.start.dist(e2.start) <= TOLERANCE && e1.end.dist(e2.end) <= TOLERANCE
        case (e1: Arc, e2: Arc) =>
          e1.center.dist(e2.center) <= TOLERANCE &&
            math.abs(e1.radius - e2.radius) <= TOLERANCE &&
            math.abs(e1.theta0 - e2.theta0) <= TOLERANCE &&
            math.abs(e1.theta1 - e2.theta1) <= TOLERANCE
        case (_, _) => false

    def reverseEdges(e1: Edge, e2: Edge): Boolean =
      equalEdges(e1, e2.flipped)

    def splitPolyWithIntersections(
        a: Polygon,
        intersections: Intersections
    ): Seq[Edge] =
      val splitEdges = a.edges.list.flatMap(edgeA =>
        val ix = extractPointsFromEdge(intersections, edgeA)
        val result: Seq[Edge] = edgeA match
          case lineA: Line =>
            if ix.length == 0 then Seq(Line(lineA.start.copy(), lineA.end.copy()))
            else
              getSplitPairs(lineA.start, lineA.end, ix)
                .mapSequencePairs((a, b) => Line(a.point, b.point))
                .toSeq
          case a: Arc =>
            if ix.length == 0 then
              Seq(Arc(a.center, a.radius, a.theta0, a.theta1).withPoints(a.start.copy(), a.end.copy()))
            else
              getSplitPairs(a.start, a.end, ix)
                .mapSequencePairs((start, end) =>
                  val ts = a.thetaFromParameter(start(1))
                  val te = a.thetaFromParameter(end(1))
                  Arc(a.center, a.radius, ts, te).withPoints(start(0), end(0))
                )
                .toSeq
        // println(s"\t intersection parameters = [${ix.map(extract)}] ($edgeA -> $result)")
        directLineage(edgeA -> result)
        result
      )
      // val validLineage = splitEdges.forall(e => Context.lineageMap.isFrom(e, a))
      // if !validLineage then
      //   println(s"Invalid Lineages: ${splitEdges.filter(e => !Context.lineageMap.isFrom(e, a))}")
      // assert(false)
      splitEdges

    // Static enum to distinguish which line came from where
    private trait PolygonID
    private case object A extends PolygonID
    private case object B extends PolygonID

    extension [A](s: Seq[A])
      def +(element: A) = s :+ element
      def -(element: A) = s.filter(e => e != element)

    def mergeUnion(
        splitPolyA: Seq[Edge],
        splitPolyB: Seq[Edge],
        polyA: Polygon,
        polyB: Polygon
    ): Seq[Seq[Edge]] =
      var edgesOnBoth = Seq[(Edge, PolygonID)]()
      var allEdges = Seq[(Edge, PolygonID)]()

      for edge <- splitPolyA.map((_, A)) ++ splitPolyB.map((_, B)) do
        // If we have already seen an equivalent edge, keep one of the two in a new set
        // since we know it'll be on the resulting polygon.
        allEdges.find((other, _) => equalEdges(other, edge(0))) match
          case Some(otherEdge) =>
            allEdges = allEdges - otherEdge
            edgesOnBoth = edgesOnBoth + otherEdge
          case None =>
            // If one edge is the reverse of another, then, in a union, we discard both.
            allEdges.find((other, _) => reverseEdges(other, edge(0))) match
              case Some(otherEdge) =>
                allEdges = allEdges - otherEdge
              // Otherwise, this is a fresh edge.
              case None => allEdges = allEdges + edge

      // println(s"edgesOnBoth: $edgesOnBoth")
      // println(s"allEdges: $allEdges")
      val keepEdges = allEdges
        .filter((edge, polyID) =>
          val otherPoly = if polyID == A then polyB else polyA
          val result = !containsPoint(otherPoly, edge.eval(0.5))
          result
        )
      // debugMerge(allEdges.map(_(0)), keepEdges.map(_(0)))

      val finalEdges = keepEdges
        .concat(edgesOnBoth)
        .map((edge, polyID) =>
          // val sourcePolygon = if polyID == A then polyA else polyB
          // lineage(sourcePolygon -> edge)
          edge
        )
      // println(s"edgesOnBoth: $edgesOnBoth")
      // println(s"keepEdges: $keepEdges")
      reconstructRegion(finalEdges.toSeq)

    def mergeUnionK(
        splits: Seq[Seq[Edge]],
        polys: Seq[Polygon]
    ): Seq[Seq[Edge]] =
      var edgesOnBoth = Seq[(Edge, Polygon)]()
      var allEdges = Seq[(Edge, Polygon)]()

      for edge <- splits.zip(polys).flatMap((edges, poly) => edges.map((_, poly))) do
        // If we have already seen an equivalent edge, keep one of the two in a new set
        // since we know it'll be on the resulting polygon.
        allEdges.find((other, _) => equalEdges(other, edge(0))) match
          case Some(otherEdge) =>
            allEdges = allEdges - otherEdge
            edgesOnBoth = edgesOnBoth + otherEdge
          case None =>
            // If one edge is the reverse of another, then, in a union, we discard both.
            allEdges.find((other, _) => reverseEdges(other, edge(0))) match
              case Some(otherEdge) =>
                allEdges = allEdges - otherEdge
              // Otherwise, this is a fresh edge.
              case None => allEdges = allEdges + edge

      // println(s"edgesOnBoth: $edgesOnBoth")
      // println(s"allEdges: $allEdges")
      val keepEdges = allEdges
        .filter((edge, polyID) =>
          polys.forall(otherPoly => (polyID == otherPoly) || !containsPoint(otherPoly, edge.eval(0.5)))
        )
      // debugMerge(allEdges.map(_(0)), keepEdges.map(_(0)))

      val finalEdges = keepEdges
        .concat(edgesOnBoth)
        .map((edge, polyID) =>
          // val sourcePolygon = if polyID == A then polyA else polyB
          // lineage(sourcePolygon -> edge)
          edge
        )
      // println(s"edgesOnBoth: $edgesOnBoth")
      // println(s"keepEdges: $keepEdges")
      reconstructRegion(finalEdges.toSeq)

    def mergeDifference(
        splitPolyA: Seq[Edge],
        splitPolyB: Seq[Edge],
        polyA: Polygon,
        polyB: Polygon
    ): Seq[Seq[Edge]] =
      // The difference implementation looks very similar, structurally, to the union one -- the main
      // distinction is which lines go in which sets.
      var edgesOnBoth = Seq[(Edge, PolygonID)]()
      var allEdges = Seq[(Edge, PolygonID)]()

      for edge <- splitPolyA.map((_, A)).iterator ++ splitPolyB.map((_, B)).iterator do
        allEdges.find((other, _) => equalEdges(other, edge(0))) match
          // If one edge is the same as another, in a difference, we discard both.
          case Some(otherEdge) =>
            allEdges = allEdges - otherEdge
          case None =>
            allEdges.find((other, _) => reverseEdges(other, edge(0))) match
              case Some(otherEdge) =>
                // If one is the reverse of another, we know we're keeping one of them.
                allEdges = allEdges - otherEdge
                edgesOnBoth = edgesOnBoth :+ otherEdge
              case None =>
                // Haven't seen this before, might keep it, might not.
                allEdges = allEdges + edge

      val keepEdges = allEdges
        .filter((edge, polyID) =>
          val pt = edge.eval(0.5)
          polyID match
            case A => !containsPoint(polyB, pt)
            case B => containsPoint(polyA, pt)
        )

      // debugMerge(allEdges.map(_(0)), keepEdges.map(_(0)))
      val flippedEdges = keepEdges
        .concat(edgesOnBoth)

        // Assuming the inputs are in the correct orientation, we need to flip
        // the edges coming from B to get a consistent edge loop. ... well, this isn't actually
        // always the case, with regions. But it's hard to know this ahead of time.
        // ^ what
        .map((edge, polyID) =>
          val (result, sourcePolygon) = polyID match
            case A => (edge, polyA)
            case B =>
              val flip = edge.flipped
              directLineage(edge -> flip)
              (flip, polyB)
          result
        )

      // println(s"edgesOnBoth: $edgesOnBoth")
      // println(s"keepEdges: $keepEdges")
      reconstructRegion(flippedEdges.toSeq)

    def mergeIntersection(
        splitPolyA: Seq[Edge],
        splitPolyB: Seq[Edge],
        polyA: Polygon,
        polyB: Polygon
    ): Seq[Seq[Edge]] =
      var edgesOnBoth = Seq[(Edge, PolygonID)]()
      var allEdges = Seq[(Edge, PolygonID)]()

      // This is... I think the same as Union?
      for edge <- splitPolyA.map((_, A)) ++ splitPolyB.map((_, B)) do
        // If we have already seen an equivalent edge, keep one of the two in a new set
        // since we know it'll be on the resulting polygon
        allEdges.find((other, _) => equalEdges(other, edge(0))) match
          case Some(otherEdge) =>
            allEdges = allEdges - otherEdge
            edgesOnBoth = edgesOnBoth + otherEdge
          case None =>
            // If one edge is the reverse of another, then, in a intersection, we discard both.
            allEdges.find((other, _) => reverseEdges(other, edge(0))) match
              case Some(otherEdge) =>
                allEdges = allEdges - otherEdge
              // Otherwise, this is a fresh edge.
              case None => allEdges = allEdges + edge

      // println(s"edgesOnBoth: $edgesOnBoth")
      // println(s"allEdges: $allEdges")
      val keepEdges = allEdges
        .filter((edge, polyID) =>
          val otherPoly = if polyID == A then polyB else polyA
          val result = containsPoint(otherPoly, edge.eval(0.5))
          result
        )
      // debugMerge(allEdges.map(_(0)), keepEdges.map(_(0)))

      val finalEdges = keepEdges
        .concat(edgesOnBoth)
        .map((edge, polyID) =>
          // val sourcePolygon = if polyID == A then polyA else polyB
          // lineage(sourcePolygon -> edge)
          edge
        )
      // println(s"edgesOnBoth: $edgesOnBoth")
      // println(s"keepEdges: $keepEdges")
      reconstructRegion(finalEdges.toSeq)

    /** Input: a soup of edges that make up a set of region paths. Output: a list of polygon paths with
      * collinear edges merged. It is important that the edges be in the correct order for
      * reconstruction. This order is dependent on the caller.
      */
    private def reconstructRegion(
        allSegments: Seq[Edge]
    ): Seq[Seq[Edge]] =
      inline def eq(inline a: Point, inline b: Point): Boolean =
        val d = a.dist(b)
        d <= TOLERANCE

      if allSegments.length == 0 then return Seq()
      val Seq(first, rest*) = allSegments: @unchecked
      val unmatched = mutable.Set.from(rest)
      val sequences = Buffer(Buffer(first))

      // Starting with an arbitrary line, for each line, continue looking through all lines
      // to find one that continues this path. If one is not found, then that means the lines
      // form disjoint path sets, so we pick an arbitrary one and keep going until all lines
      // are partitioned.
      while unmatched.size > 0 do
        val current = sequences.last.last
        val nextLine = unmatched
          .find(line => eq(line.start, current.end))
        nextLine match
          case Some(foundLine) =>
            unmatched.remove(foundLine)
            sequences.last += foundLine
          case None =>
            // println(s"Found no match in $unmatched for $current")
            val newStart = unmatched.head
            unmatched.remove(newStart)
            sequences += mutable.Buffer(newStart)

      // Take an already-ordered list of lines and merge collinear runs of edges
      def stitchCollinear(poly: Seq[Edge]): Seq[Edge] =
        if poly.size == 0 then return Seq()
        def isCollinear(a: Edge, b: Edge): Boolean =
          (a, b) match
            case (a: Line, b: Line) =>
              val dxA = a.end.x - a.start.x
              val dxB = b.end.x - b.start.x
              if (math.abs(dxA) <= TOLERANCE) then math.abs(dxB) <= TOLERANCE
              else
                val dyA = a.end.y - a.start.y
                val dyB = b.end.y - b.start.y
                math.abs((dyA / dxA) - (dyB / dxB)) <= TOLERANCE

            // WIP: merge arcs.
            // To do this; we have to check that they have the same center/radius,
            // and that the end of A is the start of B or vice versa.
            // After this we need to reconstruct the arcs, where care must be taken
            // to preserve the references attached to either.
            case (a: Arc, b: Arc) =>
              math.abs(a.center.x - b.center.x) <= TOLERANCE &&
                math.abs(a.center.y - b.center.y) <= TOLERANCE &&
                math.abs(a.radius - b.radius) <= TOLERANCE &&
                math.abs(a.theta1.mod(tau) - b.theta0.mod(tau)) <= TOLERANCE &&
                ((a.theta0 <= a.theta1 && b.theta0 <= b.theta1) || // Arcs merge CCW
                  (a.theta1 <= a.theta0 && b.theta1 <= b.theta0)) // Clockwise

            case _ => false

        val workingSet = mutable.Stack[Edge](poly.head)

        // Merge all collinear edges in sequence.
        // Open question: intermediate points are merged out of this -- how do we handle
        //  this at reference-time? I think the logical thing to do is not return the
        //  anything to do with that, but one could imagine e.g. the results of a line
        //  in the union depending on this. Picture the following:
        //   -> Three boxes stacked on top of each other, slightly offset to the left/right.
        //   -> Unioned all together.
        //  At some parameter values they all align to make a single column. At that point,
        //  the middle rectangle is no longer part of the union because it gets merged out.
        //  Should u.lines(from(middle.left)) return the leftmost line? I would argue yes,
        //  on the principle that that leftmost line would not exist without `middle.left`.

        def mergeEdges(a: Edge, b: Edge) =
          (a, b) match
            case (a: Line, b: Line) =>
              val mergedLine = Line(a.start, b.end)
              lineage(Seq(a, b) -> mergedLine) //
              mergedLine
            case (a: Arc, b: Arc) =>
              // Do we need to check if this overflows?
              val (t0, t1) =
                val (s0, s1) = (a.theta0, b.theta1)
                if math.abs(a.theta1 - b.theta0) <= TOLERANCE then (s0, s1)
                else
                  val (r0, r1) = if s1 > s0 then (s0, s1 - tau) else (s0 - tau, s1)
                  if r0 < -tau || r1 < -tau then (r0 + tau, r1 + tau)
                  else if r0 > tau || r1 > tau then (r0 - tau, r1 - tau)
                  else (r0, r1)
              val mergedArc = Arc(a.center, a.radius, t0, t1).withPoints(a.start, b.end)
              lineage(Seq(a, b) -> mergedArc)
              mergedArc
            case (_, _) => error("Cannot merge differently-typed edges")

        for b <- poly.tail do
          val a = workingSet.pop()
          if isCollinear(a, b) then workingSet.push(mergeEdges(a, b))
          else workingSet.push(a, b)

        val result = workingSet.toSeq.reverse
        // Merge the start and end lines if needed
        if result.size > 1 then
          val a = result.head
          val b = result.last
          if isCollinear(a, b) then result.drop(1).dropRight(1) :+ mergeEdges(b, a)
          else result
        else result

      sequences.map(poly => stitchCollinear(poly.toSeq)).toSeq

  /** Determine if the edge intersects any of the other edges besides the ones it is being actively
    * trimmed from.
    */
  def intersectsAnyEdge(edges: Seq[Edge], edge: Edge, exceptA: Edge, exceptB: Edge) =
    val intersectingEdges = edges.filter(e =>
      (e != exceptA && e != exceptB) &&
        e.bounds.intersects(edge.bounds) && // Check the bbox first to avoid spurious comparisons
        e.intersect(edge).size > 0
    )
    // intersectingEdges.foreach(e => draw(e, "red"))
    intersectingEdges.size > 0

  private def computeVertexTrim(
      polygon: Polygon,
      targetPoints: Seq[Point],
      radius: Double,
      f: (Point, Edge, Edge, List[Edge]) => Option[(Point, Point, Edge)]
  ) =
    var points = polygon.points.list.toList
    var edges = polygon.edges.list.toList
    val createdPoints = mutable.Map[Point, Buffer[Point]]()
    val createdSegments = mutable.Map[Point, Buffer[Edge]]()

    if radius <= 2.0 * TOLERANCE then (points, edges, createdPoints, createdSegments)
    else
      for point <- targetPoints do
        val index = points.indexOf(point)
        if index == -1 then ()
        else
          // If first, before is empty. If last, rest is of length 1 and after is empty.
          val (before, rest) = points.splitAt(index)
          val after = rest.drop(1)
          val prevIndex = if (index == 0) then edges.length - 1 else index - 1

          val (lineA, lineB) = (edges(prevIndex), edges(index))
          val trimResult = f(point, lineA, lineB, edges)
          if !trimResult.isDefined then ()
          else
            val (a, b, newLine) = trimResult.get
            val newPoints = Seq(a, b)

            val oldToNew: Edge = lineA.trimSegment(a, reverse = false)
            val newToOld: Edge = lineB.trimSegment(b, reverse = true)

            directLineage(lineA -> Seq(oldToNew, a))
            directLineage(lineB -> Seq(newToOld, b))
            lineage(lineA -> newLine)
            lineage(lineB -> newLine)
            lineage(point -> Seq(a, b, newLine))

            val newEdges = List(oldToNew, newLine, newToOld)
            createdPoints.getOrElseUpdate(point, Buffer()) ++= newPoints
            createdSegments.getOrElseUpdate(point, Buffer()) ++= newEdges

            // Patch in the new points and new lines
            points = before ++ newPoints ++ after
            edges =
              if index == 0 then List(newLine, newToOld) ++ (edges.drop(1).dropRight(1)) :+ oldToNew
              else
                val (beforeEdges, afterEdges) = edges.splitAt(prevIndex)
                beforeEdges ++ newEdges ++ afterEdges.drop(2)

      /* TODO: propagate reference information into chamfer; do the appropriate copies, etc. */
      (points, edges, createdPoints, createdSegments)

  def computeChamfer(polygon: Polygon, chamferPoints: Seq[Point], radius: Double) =
    def chamferAtPoint(point: Point, lineA: Edge, lineB: Edge, edges: List[Edge]) =
      val maxLength = math.min(lineA.length, lineB.length)
      // TODO: parameterize this; sometimes we will want to chamfer all of them
      // by a radius, doing those edges that can be chamfered as far as possible
      // even if all of them don't make it to the target radius.
      // TODO: Check case where we chamfer so far that the old edge should just
      // be deleted and we no longer have a trimmed subsegment. We may have to be
      // careful that the point lookup succeeds for subsequent points. On the other
      // hand we may not as it would have errored long before getting there.
      if (radius >= (maxLength - TOLERANCE)) then
        error(s"Cannot chamfer farther than neighboring face [distance = $maxLength]")

      val a = lineA.offsetPoint(radius, reverse = true)
      val b = lineB.offsetPoint(radius, reverse = false)
      val newLine = Line(a, b)

      if intersectsAnyEdge(edges, newLine, lineA, lineB) then
        error("Chamfer causes intersection with other face of polygon.")
      Some((a, b, newLine))
    computeVertexTrim(polygon, chamferPoints, radius, chamferAtPoint)

  def computeFillet(polygon: Polygon, filletPoints: Seq[Point], radius: Double)(using
      Operation
  ) =
    def filletAtPoint(
        point: Point,
        lineA: Edge,
        lineB: Edge,
        edges: List[Edge]
    ): Option[(Point, Point, Edge)] =
      val vec1 = lineA.endTangent
      val vec2 = lineB.startTangent
      val crossZ = vec1.crossZ(vec2)
      val dir1 = (if (crossZ >= 0.0) then 1.0 else -1.0)
      val filletDistance = radius * dir1

      // println(s"v1: $vec1 | v2: $vec2 | crossZ = $crossZ | dir = $dir1")
      // println(s"A = $lineA | B = $lineB")

      val offset1 = lineA.offset(filletDistance)
      val offset2 = lineB.offset(filletDistance)

      // draw(offset1, "turquoise")
      // draw(offset2, "green")

      val ix = offset1.intersect(offset2)

      // This... shouldn't be a problem but we'll see.
      if ix.size != 1 then
        if math.abs(crossZ) < TOLERANCE then
          return None //error(s"Edges are parallel, cannot perform fillet")
        else if ix.size == 0 then error(s"Fillet radius too large, cannot perform fillet")
        else error(s"Got ${ix.size} points when computing fillet")

      val filletCenter = ix(0).point
      // draw(filletCenter, "red")

      val a: Point = lineA.offsetNormal(filletCenter, -filletDistance)
      val b: Point = lineB.offsetNormal(filletCenter, -filletDistance)
      if /* both on respective segments */ false then
        error("Fillet point not on segment (radius too large)")

      val newLine: Edge = {
        var theta0 = Arc.thetaFromPoint(filletCenter, a)
        var theta1 = Arc.thetaFromPoint(filletCenter, b)
        // println(s"t0 = $theta0 | t1 = $theta1 | dir = $dir1")
        if theta0 <= theta1 && dir1 < 0.0 then theta0 += tau // Keep 'em clockwise
        if theta0 >= theta1 && dir1 > 0.0 then theta1 += tau
        Arc(filletCenter, radius, theta0, theta1)
      }

      if intersectsAnyEdge(edges, newLine, lineA, lineB) then
        error("Chamfer causes intersection with other face of polygon.")
      Some((a, b, newLine))

    computeVertexTrim(polygon, filletPoints, radius, filletAtPoint)
