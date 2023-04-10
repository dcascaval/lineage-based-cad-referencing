package elodin.opt

import scala.annotation.targetName

import collection.mutable
import mutable.Buffer

import elodin.global.api.*
import elodin.geometry

inline val DEBUG_REFERENCES = false
inline def debug_ref(s: String) = if DEBUG_REFERENCES then println(s) else ()
inline def debug_ref(f: => Unit) = if DEBUG_REFERENCES then f else ()

object Queries:
  import Kernel.*

  /** Query encoding. Queries specify the semantics behind a selection intent. */
  sealed trait Query

  case class From(ancestor: Reference, direct: Boolean) extends Query

  /** Inverse of From */
  case class To(descendant: Reference) extends Query

  /** Conjunction: fromAll(a,b) is the set of elements from both A and B */
  case class FromAll(ancestors: Seq[Reference], direct: Boolean) extends Query

  /** Disjunction: fromAny(a,b) is the set of elements from either A or B */
  case class FromAny(ancestors: Seq[Reference], direct: Boolean) extends Query
  case class ToAll(ancestors: Seq[Reference]) extends Query
  case class ToAny(ancestors: Seq[Reference]) extends Query
  case class Filter(query: Query, predicate: Reference => Boolean) extends Query
  case class Contains(query: Query, containedQuery: Query) extends Query

  /** General purpose boolean combinators. */
  case class Or(queries: Seq[Query]) extends Query
  case class And(queries: Seq[Query]) extends Query
  case class Not(query: Query) extends Query

  /** All is all of the values in the set, e.g. `single(All)` to assert set size == 1. */
  case object All extends Query

  // Reference primitives
  def from(ancestor: Reference): From = From(ancestor, false)
  def directFrom(ancestor: Reference): From = From(ancestor, true)

  def to(descendant: Reference): To = To(descendant)

  // Multiple entities
  def fromAll(ancestors: Reference*): FromAll = FromAll(ancestors, false)
  def directFromAll(ancestors: Reference*): FromAll = FromAll(ancestors, true)
  def fromAny(ancestors: Reference*): FromAny = FromAny(ancestors, false)
  def directFromAny(ancestors: Reference*): FromAny = FromAny(ancestors, true)

  def toAll(descendants: Reference*): ToAll = ToAll(descendants)
  def toAny(descendants: Reference*): ToAny = ToAny(descendants)

  @targetName("fromAllDynList")
  def fromAll[T <: Reference](ancestors: DynList[T]*): FromAll =
    FromAll(ancestors.flatMap(_.list), false)
  @targetName("directFromAllDynList")
  def directFromAll[T <: Reference](ancestors: DynList[T]*): FromAll =
    FromAll(ancestors.flatMap(_.list), true)
  @targetName("fromAnyDynList")
  def fromAny[T <: Reference](ancestors: DynList[T]*): FromAny =
    FromAny(ancestors.flatMap(_.list), false)
  @targetName("directFromAnyDynList")
  def directFromAny[T <: Reference](ancestors: DynList[T]*): FromAny =
    FromAny(ancestors.flatMap(_.list), true)

  def and(queries: Query*): And = And(queries)
  def or(queries: Query*): Or = Or(queries)
  def not(query: Query): Not = Not(query)

  def contains(q1: Query, q2: Query) = Contains(q1, q2)

  //
  //    DYNAMIC LISTS
  //

  def dynamicList[T <: Reference](t: Seq[T]): DynList[T] = DynList(t)

  object DynList:
    def empty[V <: Reference]: DynList[V] = DynList[V](Seq())

  /** User-facing error logging. */
  var error: String => Nothing =
    (s: String) => throw new Exception(s)

  /** List of an arity that changes over execution paths. This has set semantics, in that you cannot
    * index into it and two DynLists are considered equivalent if they have the same elements regardless
    * of the order. We store this internally as a list primarily so that we have deterministic behavior
    * in tests, particularly w/r/t ordering, and because a lot of the data structures that we want to be
    * dynamicLists are already lists, and we don't necessarily want to incur the overhead of allocating a
    * set.
    *
    * NOTE: DynList does not explicitly check for duplicates (may want to change this.)
    *
    * TODO: profile this. We incur set conversion at query-time.
    */
  class DynList[V](val list: Seq[V], val indexable: Boolean = false):
    export list.size

    def union[T <: V](other: DynList[T]) =
      DynList[V](Set.from(list.concat(other.list)).toSeq)
    def difference[T <: V](other: DynList[T]) =
      DynList[V](Set.from(list).diff(Set.from(other.list)).toSeq)
    def intersect[T <: V](other: DynList[T]) =
      DynList[V](Set.from(list).intersect(Set.from(other.list)).toSeq)

    def map[B <: Reference](f: V => B) = DynList[B](list.map(f), this.indexable)
    def filter(f: V => Boolean) = DynList[V](list.filter(f))

    inline def ofType[B <: V]: DynList[B] =
      DynList[B](list.filter(_.isInstanceOf[B]).asInstanceOf[Seq[B]])

    inline def toSet = list.toSet

    def apply(id: Reference): Option[V] =
      list.find(a => a == id).map(_.asInstanceOf[V])

    override def toString() = s"[ ${list.map(_.toString()).mkString(", ")} ]"
    override def equals(o: Any) =
      o match
        // TODO: remove allocation
        case other: DynList[?] => Set.from(list) == Set.from(other.list)
        case _                 => false

  object OPTS: // RTX ON
    var contains = true
    var traverseAll = true
    var traverseAny = true
    var earlyAndOr = true

    def enable() =
      contains = true
      traverseAll = true
      traverseAny = true
      earlyAndOr = true

    def disable() =
      contains = false
      traverseAll = false
      traverseAny = false
      earlyAndOr = false

  extension [V <: Reference](dyn: DynList[V])

    /** Returns true if the query is satisfied for ANY of the elements in this list, and attempts to
      * perform algebraic optimizations to minimize the work done to determine the query resolution
      */
    def queryContains(refQuery: Query): Boolean =
      refQuery match
        case All               => true
        case From(src, direct) => dyn.list.orMap(e => Context.lineageMap.isFrom(e, src, direct))
        case FromAll(sources, direct) =>
          if OPTS.traverseAll then dyn.list.orMap(e => Context.lineageMap.isFromAll(e, sources, direct))
          else dyn.list.orMap(e => sources.andMap(src => Context.lineageMap.isFrom(e, src, direct)))
        case FromAny(sources, direct) =>
          if OPTS.traverseAny then
            val parentSet = sources.containedElements()
            dyn.list.orMap(e => Context.lineageMap.isFromAnySet(e, parentSet, direct))
          dyn.list.orMap(e => sources.orMap(src => Context.lineageMap.isFrom(e, src, direct)))
        case To(dst) =>
          dyn.list.orMap(e => Context.lineageMap.contributesTo(e, dst))
        case ToAll(dests) =>
          dyn.list.orMap(e => dests.andMap(dst => Context.lineageMap.contributesTo(e, dst)))
        case ToAny(dests) =>
          dyn.list.orMap(e => dests.orMap(dst => Context.lineageMap.contributesTo(e, dst)))
        case Filter(q1, p) =>
          // Even one element passing the predicate will succeed, which may be suitable to
          // a generator-style approach if this turns out to be a bottleneck
          queryNaive(q1).list.orMap(p)
        case Contains(q1, q2) =>
          queryNaive(q1).list // Likewise here
            .orMap(r => r.containedElements.asDynList().queryContains(q2))
        case Or(queries) => // Will have 1 if any have 1
          queries.orMap(queryContains)
        case And(queries) =>
          // Can't do much about this until all of them have it,
          // but we can optimize if one of them is empty since we know it'll nullify the intersection
          if OPTS.earlyAndOr then queryIntersction(queries).size != 0
          else
            queries
              .andMapExit(queryNaive, f => f.size == 0)
              .map(s => s.intersect().size != 0)
              .getOrElse(false)
        case Not(q) => !queryContains(q)

    def queryIntersction(queries: Seq[Query]): DynList[V] =
      // Optimization: don't have to filter the whole thing, just what
      // the earlier queries haven't yet eliminated.
      var kept = mutable.Set.from[V](dyn.list) // These have been in every query result so far
      var candidates: DynList[V] = dyn
      for query <- queries do
        val result = candidates.queryNaive(query)
        kept = kept.intersect(result.toSet)
        candidates = kept.toSeq.asDynList()
      candidates

    def queryUnion(queries: Seq[Query]): DynList[V] =
      // Optimization: don't have to check the elements already in the
      // set against the following queries, as they'll be there.
      val marked = mutable.Set[V]()
      var unmarked: DynList[V] = dyn
      for query <- queries do
        val result = unmarked.queryNaive(query)
        marked ++= result.list
        unmarked = unmarked.filter(e => !marked.contains(e))
      marked.toSeq.asDynList()

    def queryNaive(refQuery: Query): DynList[V] =
      refQuery match
        case All => dyn
        case From(src, direct) =>
          dyn.list.filter(e => Context.lineageMap.isFrom(e, src, direct)).asDynList()
        case FromAll(sources, direct) =>
          if OPTS.traverseAll then
            // Is there something better we can do here to reuse reachability info between traversals?
            // (Perhaps a BFS downwards; but it's hard to say which vertices are reachable from any member of all sets.)
            dyn.list.filter(e => Context.lineageMap.isFromAll(e, sources, direct)).asDynList()
          else //
            dyn.list
              .filter(e => sources.andMap(src => Context.lineageMap.isFrom(e, src, direct)))
              .asDynList()
        case FromAny(sources, direct) =>
          if OPTS.traverseAny then
            val parentSet = sources.containedElements()
            // Likewise here: which of the vertices in the upwards traversal are known "reachable"?
            dyn.list.filter(e => Context.lineageMap.isFromAnySet(e, parentSet, direct)).asDynList()
          else //
            dyn.list
              .filter(e => sources.orMap(src => Context.lineageMap.isFrom(e, src, direct)))
              .asDynList()
        case To(dst) =>
          dyn.list.filter(e => Context.lineageMap.contributesTo(e, dst)).asDynList()
        case ToAll(dests) =>
          dyn.list.filter(e => dests.andMap(dst => Context.lineageMap.contributesTo(e, dst))).asDynList()
        case ToAny(dests) =>
          dyn.list.filter(e => dests.orMap(dst => Context.lineageMap.contributesTo(e, dst))).asDynList()

        case Filter(q1, p) =>
          queryNaive(q1).list
            .filter(p)
            .asDynList()

        case Contains(q1, q2) =>
          if OPTS.contains then
            queryNaive(q1).list
              .filter(r => r.containedElements.asDynList().queryContains(q2))
              .asDynList()
          else
            queryNaive(q1).list
              .filter(r => r.containedElements.asDynList().queryNaive(q2).size != 0)
              .asDynList()

        case Or(queries) =>
          if OPTS.earlyAndOr then queryUnion(queries)
          else queries.map(queryNaive).union()

        case And(queries) =>
          if OPTS.earlyAndOr then queryIntersction(queries)
          else queries.map(queryNaive).intersect()

        case Not(q) =>
          val result = queryNaive(q).toSet
          dyn.filter(e => !result.contains(e))

    /** Right now query forwards to the naive variant. */
    def query(q: Query): DynList[V] =
      val t0 = System.nanoTime()
      Context.num_queries += 1
      // if Context.tracking_query_time then //
      //   error("RECURSIVE QUERY TIME TRACKING")
      // else if !Context.tracking_kernel_time then //
      //   error("Query executed outside of kernel tracking")
      // Context.tracking_query_time = true
      val r = queryNaive(q)
      val t1 = System.nanoTime()
      Context.query_ms += ((t1 - t0).toDouble * 1e-6)
      // Context.tracking_query_time = false
      // println(s"[Query Result] $q => $r")
      r

  inline def expect(inline expr: Boolean, inline msg: String) =
    val e = expr
    if !e then error(msg)

  def empty(list: DynList[?]) =
    expect(list.list.size == 0, s"Expected empty set, got size = ${list.size}")

  def empty(list: Seq[?]) =
    expect(list.size == 0, s"Expected empty set, got size = ${list.size}")

  def single[A](list: DynList[A]) =
    expect(list.list.size == 1, s"Expected single set, got size = ${list.size}")
    list.list(0)

  def single[A](list: Seq[A]) =
    expect(list.size == 1, s"Expected single set, got size = ${list.size}")
    list(0)

  def single[A](set: Set[A]) =
    expect(set.size == 1, s"Expected single set, got size = ${set.size}")
    set.toList(0)

  @targetName("functionStyleQuery")
  def query[A <: Reference](list: DynList[A], query: Query): DynList[A] =
    list.query(query)

  def query[A <: Reference](list: Seq[A], query: Query) =
    list.asDynList().query(query)

  def query[A <: Reference](list: Set[A], query: Query) =
    list.asDynList().query(query)

  extension (region: geometry.Region2D) //
    def single(): geometry.Polygon = //
      Queries.single(region.polygons)

  extension [T <: Reference](seq: Seq[T]) //
    inline def asDynList(indexable: Boolean = false) = DynList(seq, indexable)

  extension [T <: Reference](seq: Seq[DynList[T]]) //
    inline def union(): DynList[T] =
      val resultSet = mutable.Set[T]()
      seq.foreach(resultSet ++= _.list)
      DynList(resultSet.toSeq)
    inline def intersect(): DynList[T] =
      seq match
        case Seq() => DynList(Seq())
        case elems => // Not sure there's a more efficient way to do this if we've computed evreything in advance
          elems.map(_.list.toSet).reduce(_ intersect _).asDynList()

  extension [T <: Reference](set: Set[T])
    /** Shorthand to convert a set to a dynList */
    inline def asDynList() = DynList(set.toSeq)

    /** Filter a set to elements of a certain subtype */
    inline def ofType[B <: T]: Set[T] =
      set.filter(_.isInstanceOf[B])

def dfs[T](ops: Seq[T], dependencies: T => Seq[T]): Seq[T] =
  import collection.mutable.*

  val order = Buffer[T]()
  val seen = Set.from[T](ops)
  val temporary = Set[T]()
  val permanent = Set[T]()

  def visit(current: T): Unit =
    if permanent contains current then return
    if temporary contains current then throw new Exception("Cyclic dependency")
    temporary += (current)
    for dep <- dependencies(current) do visit(dep)
    temporary -= (current)
    permanent += (current)
    order += current

  for origin <- seen do visit(origin)
  order.toSeq

extension [V <: Reference](sources: Seq[V])
  def containedElements(): Set[Reference] =
    sources.foldLeft(Set.empty[Reference])((set, source) => set ++ source.containedElements)

extension [A](as: Seq[A])
  def displayQuery(msg: String) =
    println(s"$msg: $as"); as

  def andMap(f: A => Boolean) =
    var result = true; var i = 0
    while result && i < as.size do
      result = result && f(as(i))
      i += 1
    result

  def andMapExit[B](f: A => B, p: B => Boolean): Option[Seq[B]] =
    var result = true; var i = 0;
    val buf = Buffer[B]()
    while result && i < as.size do
      val fi = f(as(i))
      result = result && p(fi)
      buf += fi
    if result then Some(buf.toSeq) else None

  def orMap(f: A => Boolean) =
    var result = false; var i = 0
    while !result && i < as.size do
      result = result || f(as(i))
      i += 1
    result
