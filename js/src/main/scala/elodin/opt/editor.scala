package elodin.opt

import scalajs.js
import org.scalajs.dom.{document, console, window, Element, MouseEvent}
import org.scalajs.dom.raw.HTMLElement
import collection.mutable.{Map, Set, Buffer}

import elodin.global.api.*
import elodin.global.js.*
import elodin.geometry.*

import elodin.dom.api.*
import elodin.dom.svg.*

import UI.*
import api.*
import tags.div

sealed trait ConstraintContext:
  def constrain(values: => Seq[Double]): Unit

class CollectConstraints extends ConstraintContext:
  val constraints = Buffer[Double]()
  def constrain(values: => Seq[Double]) =
    val newConstraints = values
    constraints ++= newConstraints

class DiscardConstraints extends ConstraintContext:
  def constrain(values: => Seq[Double]) = ()

def constrain(values: => Double*)(using ctx: ConstraintContext) =
  ctx.constrain(values)

type ReferenceFn = Vector[Var] => (Vector[Double], Double)

trait UserProgram[+Output](wrapper: HTMLElement = document.body, renderContext: Renderer):
  val sourceText: Option[String] = None
  var parameters: Vector[Var]
  protected[this] def execute(input: Vector[Var])(using Renderer, ConstraintContext): Output

  def compose(lossFn: (Vector[Var], Output) => (Vector[Double], Double)): ReferenceFn =
    var called = false
    (input: Vector[Var]) =>
      counter.clear()
      val renderContext = NullRenderer()
      val constraintContext = new CollectConstraints()
      val shape = execute(input)(using renderContext, constraintContext)
      val (lossConstraints, res) = lossFn(input, shape)
      val accumulatedConstraints = constraintContext.constraints.toSeq
      val allConstraints = (accumulatedConstraints ++ lossConstraints).distinct.toVector
      if (!called) then called = true
      (allConstraints, res)

  // Obtain a concrete instantiation
  def run(values: Vector[Double]): Option[Output] =
    if values.length != parameters.length then
      throw new Exception(
        s"Called with wrong arity, expected ${parameters.length} parameters, got ${values.length}."
      )
    counter.clear()
    val tempParameters = parameters.withValues(values)
    // If the program fails at runtime, then recover gracefully
    try Some(execute(tempParameters)(using renderContext, new DiscardConstraints()))
    catch
      case e: Throwable => {
        console.warn(s"$e")
        None
      }

/** Bidirectional editor for a dynamic program */
class DynamicEditor(
    program: DynamicExecutor,
    drawHandles: Boolean = false,
    resized: () => Unit,
    wrapper: HTMLElement = document.body
):

  var currentParameters: Vector[Var] = null
  var currentOutputs: ExecutionStats = null

  var displayContainer = div("program-container")
  val displayedParams = ParameterDisplay(displayContainer, compute)
    .patchParameters(program.parameters)

  val getText = program.sourceText
    .map(displayEditableCode(displayContainer)(runWithText, resized))
    .get

  def runWithText(newText: String) =
    if program.updateProgram(newText) then
      // console.log(program.programBody.map(_.toString).mkString("\n"))
      displayedParams.patchParameters(program.parameters)
      compute(displayedParams.currentValues)
  // else println("not new text")

  def resetParameters() =
    compute(program.parameters)

  def runFromText() = runWithText(getText())

  wrapper(displayContainer)
  compute(displayedParams.currentValues)

  /** Execute the program with these parameters and draw the result */
  def compute(parameters: Vector[Var]): Unit =
    program
      .run(parameters.values)
      .map(output =>
        currentParameters = parameters
        currentOutputs = output
        displayedParams.updateDisplay(currentParameters)
      )

def logOptRun(current: Point, parameters: Vector[Double], targetX: Double, targetY: Double) =
  println(s"Current Point: (${current.x}, ${current.y}) [ID = ${current}]")
  println(s"Target Point: ($targetX, $targetY)")
  println(s"Starting At: [${parameters.mkString(", ")}]")

def logPointResult(r: Point, targetX: Double, targetY: Double) =
  def distance(ax: Double, ay: Double, bx: Double, by: Double) =
    math.sqrt((bx - ax) * (bx - ax) + (by - ay) * (by - ay))
  val dist = distance(r.x, r.y, targetX, targetY)

  val opacity = 0.25

  console.log(
    s"%cResulting Point: (${r.x}, ${r.y}) " +
      s"[Distance = ${dist}]",
    s"background-color: ${if dist > 25.0 then s"rgba(255,0,0,$opacity)"
    else s"rgba(0,255,0,$opacity)"};"
  )
