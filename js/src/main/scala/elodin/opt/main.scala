package elodin.opt

import scalajs.js
import org.scalajs.dom.{document, console, window, Element, MouseEvent}
import collection.mutable.{Map, Set, Buffer}

import elodin.global.api.*
import elodin.dom.api.*
import elodin.dom.svg.*

import UI.*
import org.scalajs.dom.raw.KeyboardEvent
import typings.monaco.monaco.editor
import View.*
import org.scalajs.dom.raw.HTMLElement

inline def debug(inline s: String) = () // println(s)

object Composed:
  var renderMode: View = View2D
  var renderToggle: Option[RenderMode] = None

  class interpret(text: String, viewMode: View):
    renderMode = viewMode
    renderToggle.map(_.setStateNoCallback(viewMode))
    val container = tags.div
    val renderer = new Renderer3D(container, viewMode)
    val program = DynamicExecutor(text, container, renderer)
    val editor = DynamicEditor(program, drawHandles = true, renderer.onWindowResize, wrapper = container)
    document.body(container)

  def bench_program(text: String, numEvals: Int, topK: Int) =
    // We don't draw or generate geometry, just get statistics.
    val outputs = (0 until numEvals).map(_ =>
      val renderer = new MeasuringRenderer()
      val program = DynamicExecutor(text, tags.div, renderer)
      val editor = DynamicEditor(program, drawHandles = false, () => (), tags.div)
      editor.currentOutputs
    )

    def avg(s: Seq[Double]) =
      s.sorted.take(topK).reduce(_ + _) / topK.toDouble

    val avg_time = avg(outputs.map(_.runMS))
    val avg_qtime = avg(outputs.map(_.queryMS))
    val avg_ktime = avg(outputs.map(_.kernelMS))
    val const = outputs(0)

    ExecutionStats(
      avg_time,
      avg_qtime,
      avg_ktime,
      const.numOps,
      const.numQueries,
      const.numSegs,
      const.numFaces,
      const.numTraversals,
      const.vertsExpanded,
      const.lineageVerts,
      const.lineageEdges
    )

  def iterateTimeout[A](as: Seq[A], f: (Int, A) => Unit, done: () => Unit) =
    var i = 0
    var timeoutF: js.Function0[Unit] = null;
    timeoutF = () =>
      if i < as.size then
        f(i, as(i))
        i += 1
        window.setTimeout(timeoutF, 0)
      else done()
    window.setTimeout(timeoutF, 0)

  class benchmark(programs: Seq[(String, String, View)], no_time: Boolean = false):
    val (numRuns, topK, numWarmup) = if no_time then (1, 1, 0) else (10, 5, 5)

    val container = tags.div.withStyle(("position", "relative"), ("top", "60px"), ("left", "10px"))
    document.body(container)

    val warmup = tags.div
    val progress = tags.div
    container(warmup((tags.div / "Warming up..."), progress))
    var table = tags.div("bench-table")

    var dataTable = Buffer[Buffer[Double]]()
    def medianColumn(i: Int) =
      val axis = (for buf <- dataTable yield buf(i))
      val result = axis.sorted
      result(axis.length / 2)

    def addRow(name: String, items: Seq[String]) =
      val row = tags.div("bench-row")
      row.appendChild(tags.div("bench-elt").withStyle(("min-width", "200px")) / name)
      items.foreach(i => row.appendChild(tags.div("bench-elt") / i))
      table(row)

    def measureProg(i: Int, p: (String, String, View)) =
      val (name, prog, _) = p
      if name == "----" then container(tags.p / " ")
      else
        val output = bench_program(prog, numRuns, topK)
        addRow(name, output.show())
        dataTable += output.values()

    def justRunProg(i: Int, p: (String, String, View)) =
      if !no_time then
        val (name, prog, _) = p
        if name != "----" then
          bench_program(prog, numWarmup, 1)
          progress / s"$i / ${programs.size}"

    val rowHdr = Seq(
      "Ops",
      "Queries",
      "LVertices",
      "LEdges",
      "Runtime",
      "QueryTime",
      "KernelTime",
      "Segments",
      "Faces",
      "Traversals",
      "Visited"
    )

    iterateTimeout(
      programs,
      justRunProg,
      () =>
        container.removeChild(warmup)
        container(tags.p / "OPTS OFF")
        container(table)
        addRow("-", rowHdr)
        Queries.OPTS.disable()
        iterateTimeout(
          programs,
          measureProg,
          () =>
            val medians = (0 until rowHdr.size).map(i => f"${medianColumn(i)}%.2f".stripSuffix(".00"))
            addRow("MEDIAN", medians.toSeq)
            dataTable.clear()
            container(tags.p / "OPTS ON")
            table = tags.div("bench-table")
            container(table)
            addRow("-", rowHdr)
            Queries.OPTS.enable()
            iterateTimeout(
              programs,
              measureProg,
              () =>
                val medians =
                  (0 until rowHdr.size).map(i => f"${medianColumn(i)}%.2f".stripSuffix(".00"))
                addRow("MEDIAN", medians.toSeq)
            )
        )
    )

  def run() =
    val empty = Seq(("----", """parameters{}""", View2D))

    val examples3D = Seq( // 3D test cases; impl. realistic CAD parts
      ("3d-clamp", clampPin, View3D),
      ("3d-gear", gear, View3D),
      ("3d-pivot", stackedPlates, View3D),
      ("3d-table", jamesBench, View3D),
      ("3d-controller", controller3d, View3D),
      ("3d-quad", quad, View3D)
    )

    // Models from the user study.
    val examplesUserStudy = Seq(
      ("us-read-twoSquareDiff", twoSquareDiffEx, View2D),
      ("us-read-twoSquareRotate", twoSquareRotateEx, View2D),
      ("us-read-squareL", squareLExample, View2D),
      ("us-read-fig2a", squiggle, View2D),
      ("us-read-key", circleDiff, View2D),
      ("us-read-tetris", tetris, View2D),
      ("us-write-controller", controller, View2D),
      ("us-write-chairFillet", chairFillet, View2D),
      ("us-write-divot", divotBoolean, View2D),
      ("us-write-insertExtrude", insertExtrude, View2D),
      ("us-write-reflect", rflx, View2D)
    )

    val miscExamples = Seq(
      ("3d-hex", hex, View3D),
      ("3d-latheCollet", latheCollet, View3D),
      ("2d-intersectLinks", intersectLinks, View2D),
      ("2d-arcRect", arcRectExample, View2D),
      ("2d-cheese", cheese, View2D),
      ("2d-circleRect", circleRectExample, View2D)
    )

    // Test Programs for our internal use. Not representative of
    // any realistic CAD model, built only to exercise kernel features.
    val testExamples = Seq(
      ("test-2d-mergeRects", mergeRects, View2D),
      ("test-2d-circleMidpoint", circleMidpoint, View2D),
      ("test-2d-transitive", diamondExample, View2D),
      ("test-3d-chamfer", chamferTest, View3D),
      ("test-3d-fillet", filletTest, View3D)
    )

    val examples = Seq() ++
      examples3D ++
      empty ++
      examplesUserStudy ++
      empty ++
      miscExamples ++
      empty

    var state = interpret(examples(0)(1), examples(0)(2))
    var currentOriginText = examples(0)(1)

    def setActiveExample(text: String, viewMode: View) =
      currentOriginText = text
      state.renderer.dispose()
      try document.body.removeChild(state.container)
      catch case ex: Throwable => ()
      state = interpret(text, viewMode)

    def patchRenderMode(view: View) =
      state.renderer.dispose()
      document.body.removeChild(state.container)
      state = interpret(state.editor.getText(), view)

    renderToggle = Some(RenderMode(renderMode, patchRenderMode))
    val resetProgram = GenericButton("Reset Program")
    resetProgram.listen(() => setActiveExample(currentOriginText, renderMode))

    val centerView = GenericButton("Center View")
    centerView.listen(() => state.renderer.centerView())

    val runBenchmark = GenericButton("Run Benchmarks")
    runBenchmark.listen(() =>
      state.renderer.dispose()
      document.body.removeChild(state.container)
      benchmark(examples, no_time = false)
    )

    document.body(
      dropdown(
        "Examples",
        examples.map((name, program, viewMode) => (name, () => setActiveExample(program, viewMode)))
      ).withClass(
        "example-menu"
      )(
        HelpDialog().button,
        renderToggle.get.button,
        resetProgram.button,
        centerView.button,
        runBenchmark.button
      )
    )

    // This is *in addition to* the listener inside the code editor, which traps all of its keys.
    // It is here just in case a user hits Ctrl+S outside of the editor and expects changes to happen.
    window.addEventListener(
      "keydown",
      (e: KeyboardEvent) =>
        if (e.key == "s") && (e.ctrlKey || e.metaKey) then // CTRL/CMD+S
          state.editor.runFromText()
          e.preventDefault()
          e.stopPropagation()
    )

extension (vars: Vector[Var])
  def display() =
    s"[${vars.map(v => s"${v.name}: ${v.value}").mkString(", ")}]"
  def values = vars.map(_.value)
  def withValues(values: Seq[Double]) =
    vars.zip(values).map(_ withValue _)
