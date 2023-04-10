package elodin.opt

import collection.mutable
import collection.mutable.Buffer
import elodin.opt.Queries.DynList

import scala.annotation.targetName
import elodin.global.api.*

def program[T](f: OperationGraph ?=> T) =
  given OperationGraph = Context.reset()
  f

class Bijection[A, B]:
  private val fwd = collection.mutable.Map[A, B]()
  private val rev = collection.mutable.Map[B, A]()

  def set(a: A, b: B) =
    if fwd.contains(a) || rev.contains(b) then
      throw new Exception(s"Duplicate Bijective elements: $a $b")
    else
      fwd += ((a, b))
      rev += ((b, a))

  @targetName("getA")
  def apply(a: A): B =
    fwd.get(a).getOrElse(throw new Exception(s"No matching element in bijection for $a"))

  @targetName("getB")
  def apply(b: B): A =
    rev.get(b).getOrElse(throw new Exception(s"No matching element in bijection for $b"))

class LineageMap:
  val forwardEdges = mutable.Map[Reference, Buffer[Reference]]()
  val reverseEdges = mutable.Map[Reference, Buffer[Reference]]()

  // Alternative implementation:
  // Keep a set of tuples of edges that are direct; filter dynamically the traversal.
  // Not sure that uses any less memory, so we do this instead.
  val directEdges = mutable.Map[Reference, Buffer[Reference]]() // Reverse only

  def addEdge(parent: Reference, child: Reference) =
    // forwardEdges.getOrElseUpdate(parent, Buffer()) += child
    reverseEdges.getOrElseUpdate(child, Buffer()) += parent

  def addDirectEdge(parent: Reference, child: Reference) =
    directEdges.getOrElseUpdate(child, Buffer()) += parent
    addEdge(parent, child)

  var iters = 0
  var traversals = 0

  /** Naive implementation of lineage. Traverses all edges in a given direction, and returns true if ANY
    * of the target set is reachable from the starting set.
    */
  def traverseLineage(
      map: mutable.Map[Reference, Buffer[Reference]],
      start: Set[Reference],
      targets: Set[Reference]
  ): Boolean =
    traversals += 1

    val seen = mutable.Set[Reference]()
    val queue = mutable.Queue.from[Reference](start)
    var found = false
    while !queue.isEmpty && !found do
      iters += 1
      val current = queue.dequeue()
      if targets contains current then found = true
      else if !seen.contains(current) then
        seen += current
        queue ++= map.getOrElse(current, Buffer()).filter(e => !(seen contains e))
    found

  /** Traverses all edges in a given direction, given a set of target sets. Returns true if for ALL sets
    * in the target, there EXISTS an element of that set that is reachable from the starting set.
    */
  def traverseLineageSet(
      map: mutable.Map[Reference, Buffer[Reference]],
      start: Set[Reference],
      targets: Seq[Set[Reference]]
  ): Boolean =
    traversals += 1
    val unseenTargets = mutable.Set.from[Set[Reference]](targets)
    val seen = mutable.Set[Reference]()
    val queue = mutable.Queue.from[Reference](start)

    var found = false
    while !queue.isEmpty && !found do
      iters += 1
      val current = queue.dequeue()
      unseenTargets.filter(_.contains(current)) match //
        case seenTargets =>
          if seenTargets.isEmpty then ()
          else
            seenTargets.foreach(t => (unseenTargets -= t))
            if unseenTargets.isEmpty then //
              found = true
      if !found && !seen.contains(current) then
        seen += current
        queue ++= map.getOrElse(current, Buffer()).filter(e => !(seen contains e))
    found

  inline def getReverseEdges(direct: Boolean) =
    if direct then directEdges else reverseEdges

  /** Propagate upwards from element to see if we find a "parent". */
  def isFrom(element: Reference, parent: Reference, direct: Boolean): Boolean =
    traverseLineage(getReverseEdges(direct), Set(element), parent.containedElements)

  /** Perform the "fromAny" query in a single traversal */
  def isFromAny(element: Reference, parents: Seq[Reference], direct: Boolean): Boolean =
    traverseLineage(getReverseEdges(direct), Set(element), parents.containedElements())

  /** Preferred to `isFromAny()` because we do not have to do the set creation many times */
  def isFromAnySet(element: Reference, parents: Set[Reference], direct: Boolean): Boolean =
    traverseLineage(getReverseEdges(direct), Set(element), parents)

  /** Perform the "fromAll" query in a single traversal */
  def isFromAll(element: Reference, parents: Seq[Reference], direct: Boolean): Boolean =
    traverseLineageSet(
      getReverseEdges(direct),
      Set(element),
      parents.toSet.toSeq.map(_.containedElements.toSet)
    )

  /** Propagate downwards from element to see if we find a "child" */
  def contributesTo(element: Reference, child: Reference): Boolean =
    traverseLineage(forwardEdges, Set.from(element.containedElements), Set(child))

  def summary(): (Int, Int, Int, Int) =
    val numVerts = reverseEdges.size
    val numEdges = reverseEdges.foldLeft(0) { case (total, (_, edges)) => total + edges.size }
    val vertsPerTraversal = iters.toDouble / traversals.toDouble
    println(
      f"Lineage map contains $numVerts vertices and $numEdges edges; expanded $iters vertices over $traversals traverals ($vertsPerTraversal%.2f)"
    )
    (iters, traversals, numVerts, numEdges)

  def fullSummary(): Unit =
    summary()
    for (vert, edges) <- forwardEdges do
      println(s"\t[lineage element] $vert")
      for e <- edges do println(s"\t\t[to] $e")

object Context:
  /** Current stack of graphs, each operation creates its own subgraph, so this mirrors the dynamic call
    * stack of CAD operations.
    */
  private var graphStack = mutable.Stack[OperationGraph](new OperationGraph(None))

  private var graphToOp = mutable.Map[OperationGraph, Operation]()

  /** Keep track of the individual reference-edge structure as defined by operations */
  /** These point "upwards", i.e. each operation defines from Reference -> Parents */
  var lineageMap = new LineageMap()
  var kernel_ms = 0.0
  var tracking_kernel_time = false
  var query_ms = 0.0
  var tracking_query_time = false
  var num_operations = 0
  var num_queries = 0

  /** Architecture: We will have nested operation graphs; the global context will dictate which one is
    * used for a given execution.
    */
  class OperationGraph(val superGraph: Option[OperationGraph]):
    /** Keep track of the op-graph structure */
    val operationArguments = mutable.Map[Operation, Buffer[Operation]]()

    /** Keep a pointer to the subgraphs of each operation */
    val subgraphs = Bijection[Operation, OperationGraph]()

    /** Test if a reference's origin is contained within any subgraph of this graph. */
    def containsReference(ref: Reference): Boolean =
      var origin = ref.origin
      var found = false; var continue = true
      while !found && continue do
        if origin == this then found = true
        else
          origin.superGraph match
            case Some(graph) => origin = graph
            case None        => continue = false
      found

    def associatedOperation(ref: Reference): Operation = throw new Exception(
      "Subgraph association not implemented"
    )

  /** */
  def currentGraph: OperationGraph = graphStack.top

  def logOp(m: String) =
    val indent = " " * graphStack.size
    debug_ref(s"$indent$m")

  /** Declare a unit (leaf) operation that produces *no* additional geometry internally in addition to
    * literally the object in memory that the operation is.
    */
  def unitOp(op: Operation, args: Reference*) =
    Context.addOpArgs(op, args.flatMap(_.associatedOp()))
    val newGraph = new OperationGraph(Some(currentGraph))
    graphToOp(newGraph) = op
    // println(s"\tAdded ($newGraph -> $op) to $graphToOp")
    currentGraph.subgraphs.set(op, newGraph)
    newGraph

  def startOp(op: Operation) =
    val newGraph = new OperationGraph(Some(currentGraph))
    graphStack.push(newGraph)
    graphToOp(newGraph) = op
    // println(s"\tAdded ($newGraph -> $op) to $graphToOp")
    debug_ref(s"Starting $op : graphStack = $graphStack")
    newGraph

  def endOp(op: Operation) =
    debug_ref(s"Ending $op")
    val newGraph = graphStack.pop()
    currentGraph.subgraphs.set(op, newGraph)

  def associatedOp(element: Reference): Option[Operation] =
    // currentGraph.associatedOperation(element)
    // println(graphToOp)
    graphToOp.get(element.origin)
  //.getOrElse(
  // throw new Exception(s"Disassociated element: $element")
  // )

  def addOpArgs(op: Operation, args: Seq[Operation]) =
    currentGraph.operationArguments
      .getOrElseUpdate(op, Buffer()) ++= args
    for arg <- args do //
      if !currentGraph.operationArguments.contains(arg) then //
        currentGraph.operationArguments(arg) = Buffer()

  def addLineage(source: Reference, dest: Iterable[Reference]) =
    for d <- dest do //
      lineageMap.addEdge(source, d)

  def addDirectLineage(source: Reference, dest: Iterable[Reference]) =
    for d <- dest do //
      lineageMap.addDirectEdge(source, d)

  def reset() =
    // println("RESETTING CONTEXT")
    graphStack = mutable.Stack[OperationGraph](new OperationGraph(None))
    graphToOp = mutable.Map[OperationGraph, Operation]()
    lineageMap = new LineageMap()

    kernel_ms = 0.0
    query_ms = 0.0
    tracking_kernel_time = false
    tracking_query_time = false
    num_operations = 0
    num_queries = 0
    currentGraph

/** Marker trait for objects we can refer to */
trait Reference:
  val origin: OperationGraph
  lazy val containedElements: Set[Reference] = Set(this)
  def associatedOp() =
    Context.associatedOp(this)

/** Operation is a marker trait for operation results */
trait Operation:
  inline def arguments(args: Reference*) =
    Context.addOpArgs(this, args.flatMap(_.associatedOp()))

object Operation:
  @targetName("referenceLineage")
  inline def lineage(edges: (Reference, Seq[Reference])*) =
    for (source, dest) <- edges do //
      Context.addLineage(source, dest)

  @targetName("referenceLineageSet")
  inline def lineage(edges: (Reference, Set[Reference])*) =
    for (source, dest) <- edges do //
      Context.addLineage(source, dest)

  @targetName("referenceLineageSingle")
  inline def lineage(edges: (Reference, Reference)*) =
    for (source, dest) <- edges do //
      Context.addLineage(source, Seq(dest))

  @targetName("referenceLineageInverse")
  inline def lineage(edges: (Seq[Reference], Reference)*) =
    for (source, dest) <- edges do //
      source.map(src => Context.addLineage(src, Seq(dest)))

  @targetName("referenceLineageInverseSet")
  inline def lineage(edges: (Set[Reference], Reference)*) =
    for (source, dest) <- edges do //
      source.map(src => Context.addLineage(src, Seq(dest)))

  @targetName("dynListLineage")
  inline def lineage[T <: Reference](edges: (DynList[T], DynList[T])*) =
    for (sources, dests) <- edges do
      sources.list.zip(dests.list).map((src, dst) => Context.addLineage(src, Seq(dst)))

  @targetName("seqSeqLineage")
  inline def lineage[T <: Reference](edges: (Seq[T], Seq[T])*) =
    for (sources, dests) <- edges do
      sources.zip(dests).map((src, dst) => Context.addLineage(src, Seq(dst)))

  @targetName("directReferenceLineage")
  inline def directLineage(edges: (Reference, Seq[Reference])*) =
    for (source, dest) <- edges do //
      Context.addDirectLineage(source, dest)

  @targetName("directReferenceLineageSet")
  inline def directLineage(edges: (Reference, Set[Reference])*) =
    for (source, dest) <- edges do //
      Context.addDirectLineage(source, dest)

  @targetName("directReferenceLineageSingle")
  inline def directLineage(edges: (Reference, Reference)*) =
    for (source, dest) <- edges do //
      Context.addDirectLineage(source, Seq(dest))

  @targetName("directReferenceLineageInverse")
  inline def directLineage(edges: (Seq[Reference], Reference)*) =
    for (source, dest) <- edges do //
      source.map(src => Context.addDirectLineage(src, Seq(dest)))

  @targetName("directReferenceLineageInverseSet")
  inline def directLineage(edges: (Set[Reference], Reference)*) =
    for (source, dest) <- edges do //
      source.map(src => Context.addDirectLineage(src, Seq(dest)))

  @targetName("directDynListLineage")
  inline def directLineage[T <: Reference](edges: (DynList[T], DynList[T])*) =
    for (sources, dests) <- edges do
      sources.list.zip(dests.list).map((src, dst) => Context.addDirectLineage(src, Seq(dst)))

  @targetName("directSeqSeqLineage")
  inline def directLineage[T <: Reference](edges: (Seq[T], Seq[T])*) =
    for (sources, dests) <- edges do
      sources.zip(dests).map((src, dst) => Context.addDirectLineage(src, Seq(dst)))

type OperationGraph = Context.OperationGraph
