package elodin.dom

import org.scalajs.dom.{window, document}
import org.scalajs.dom.{Document, Element}
import org.scalajs.dom.raw.SVGElement
import collection.mutable.{ArrayBuffer, HashMap}

import elodin.global.api.*
import elodin.dom.api.*

object svgapi:
  def svgTag(tag: String) =
    document.createElementNS(SVG.URI, tag)

  object SVG:
    val URI = "http://www.w3.org/2000/svg"

    def apply(root: Element, parent: SVG, width: Int, height: Int): SVG =
      val result = new SVG(width, height)(using document)
      result.dwg = root.asInstanceOf[SVGElement]
      result.def_element = parent.def_element
      result

    def apply(root: Element)(using outerContext: SVGContext): SVG =
      val (w, h) = outerContext.current.dims
      SVG(root, outerContext.current, w, h)

  class SVG(width: Int, height: Int)(using doc: Document):
    var dwg = svgTag("svg").asInstanceOf[SVGElement]
    dwg.setAttributeNS(SVG.URI, "version", "1.1")
    dwg.attr(
      "width" -> width,
      "height" -> height
    )

    var def_element = svgTag("defs")
    dwg.appendChild(def_element)

    val elts = ArrayBuffer[Element]()

    def dims =
      (width, height)

    def addDefinition(element: Element): Unit =
      def_element.appendChild(element)

    def addElement(element: Element): Unit =
      elts += element
      dwg.appendChild(element)

  class SVGContext:
    val layers: ArrayBuffer[SVG] = ArrayBuffer()
    def push(v: SVG) = layers.append(v)
    def pop = layers.remove(layers.length - 1)
    def current = layers.last

  //
  // REGISTERED DEFINITIONS
  //

  object Definitions:
    var map = new HashMap[String, Int]()
    def freshTemp(prefix: String) =
      val id = map.getOrElse(prefix, 0)
      map.update(prefix, id + 1)
      s"${prefix}_$id"

  sealed trait Gradient:
    val id: String
    val element: Element

  class RadialGradient(stops: (Int, String)*)(global: Boolean = true)(using SVGContext) extends Gradient:
    val id = Definitions.freshTemp("rg")
    private val sortStops = stops.toVector.sortBy { case (a, _) => a }

    val element = svgTag("radialGradient")
      .attr("id" -> id)
    if global then element.attr("gradientUnits" -> "userSpaceOnUse")
    for (percent, color) <- sortStops do
      val stopElement = svgTag("stop")
        .attr("offset" -> percent, "stop-color" -> color)
      element.appendChild(stopElement)
    element.define()

  def hexToRGB(hexString: String) =
    val rgb = hexString.replace("#", "")
    Seq(rgb.slice(0, 2), rgb.slice(2, 4), rgb.slice(4, 6))
      .map(Integer.parseInt(_, 16))

  def rgbToHex(rgb: Seq[Int]) = s"#${rgb.map(h => f"$h%02x").mkString("")}"

  def invertColor(hexString: String) =
    val rgb = hexString.replace("#", "")
    val inverted = Seq(rgb.slice(0, 2), rgb.slice(2, 4), rgb.slice(4, 6))
      .map(s => (255 - Integer.parseInt(s, 16)).toHexString)
      .mkString("")
    s"#$inverted"

  class LinearGradient(stops: (Int, String)*)(global: Boolean = false)(using SVGContext)
      extends Gradient:
    val id = Definitions.freshTemp("lg")
    private val sortStops = stops.toVector.sortBy { case (a, _) => a }

    val element = svgTag("linearGradient")
      .attr("id" -> id)
    if global then element.attr("gradientUnits" -> "userSpaceOnUse")
    for (percent, color) <- sortStops do
      val stopElement = svgTag("stop")
        .attr("offset" -> percent, "stop-color" -> color)
      element.appendChild(stopElement)
    element.define()

    def inverted(): LinearGradient =
      LinearGradient(
        stops.map((i, c) => (i, invertColor(c)))*
      )(global = global)

  case class Blur(stdDev: Double)(using ctx: SVGContext):
    val name = Definitions.freshTemp("blur")
    val filter = svgTag("filter").attr(
      "id" -> name
      //"x" -> "-250%",
      //"y" -> "-250%",
      //"width" -> "650%",
      //"height" -> "650%"
    )(
      svgTag("feGaussianBlur").attr("in" -> "SourceGraphic", "stdDeviation" -> stdDev)
    )
    filter.define()

  // Draw in black to clip; by default does nothing.
  class Mask[T](element: SVGContext ?=> T)(using context: SVGContext):
    val id = Definitions.freshTemp("mask")
    element.withRoot(
      svgTag("mask")
        .attr("id", id)(
          svgTag("rect")
            .attr("fill" -> "white", "width" -> "100%", "height" -> "100%")
        )
        .define()
    )

  // Identical to Mask, except that it will mask by default, draw in white to not clip.
  class InverseMask[T](element: SVGContext ?=> T)(using context: SVGContext):
    val id = Definitions.freshTemp("mask")
    element.withRoot(
      svgTag("mask")
        .attr("id", id)(
          svgTag("rect")
            .attr("fill" -> "black", "width" -> "100%", "height" -> "100%")
        )
        .define()
    )

  // Like mask, but without opacity; though potentially better performance.
  class Clip[T](element: SVGContext ?=> T)(using context: SVGContext):
    val id = Definitions.freshTemp("clip")
    element.withRoot(svgTag("clipPath").attr("id", id).define())

  class Group[T](element: SVGContext ?=> T)(using context: SVGContext):
    val id = Definitions.freshTemp("g")
    val root = svgTag("g").attr("id", id).draw()
    val contents = element.withRoot(root)

  /* Compiler gets confused with exports without these. */
  object Group:
    def apply[T](element: SVGContext ?=> T)(using context: SVGContext): Group[T] =
      new Group(element)(using context)
  object Clip:
    def apply[T](element: SVGContext ?=> T)(using context: SVGContext): Clip[T] =
      new Clip(element)(using context)
  object Mask:
    def apply[T](element: SVGContext ?=> T)(using context: SVGContext): Mask[T] =
      new Mask(element)(using context)
  object InverseMask:
    def apply[T](element: SVGContext ?=> T)(using context: SVGContext): InverseMask[T] =
      new InverseMask(element)(using context)

  case class Transformation(text: String):
    def apply(): String = text
    def compose(other: Transformation) =
      Transformation(text + " " + other())
    def >(other: Transformation) =
      Transformation(text + " " + other())
    override def toString() = text

  object Transform:
    def empty: Transformation = Transformation("")
    def apply: Transformation = Transformation("")
    def rotate(degree: Double) =
      Transformation(s"rotate($degree)")
    def rotate(degree: Double, center: (Double, Double)) =
      Transformation(s"rotate($degree,${center(0)},${center(1)})")
    def translate(dx: Double, dy: Double) =
      Transformation(s"translate($dx,$dy)")
    def translate(pt: (Double, Double)): Transformation = translate(pt(0), pt(1))
    def scale(sx: Double, sy: Double = 1.0) =
      Transformation(s"scale($sx,$sy)")
    def scale(sx: Double, sy: Double, centerX: Double, centerY: Double) =
      val e = centerX - sx * centerX
      val f = centerY - sy * centerY
      Transformation(s"matrix($sx, 0, 0, $sy, $e, $f)")

    /** Flips around the axis at this x-value */
    def flipX(x: Double) = scale(-1, 1, x, 0)
    def flipY(y: Double) = scale(1, -1, 0, y)

  class Style(args: (String, Any)*)(using context: SVGContext):
    val className = Definitions.freshTemp("style")
    val root = svgTag("style")
    root.innerHTML = s".$className {\n\t${args.toSeq.map((k, v) => s"$k: $v;").mkString("\n\t")}\n}"
    root.draw()

  class Background(element: String)(using outerContext: SVGContext):
    // Ex:
    // <defs>
    //   <pattern id="img1" patternUnits="userSpaceOnUse" width="100" height="100">
    //     <image href="wall.jpg" x="0" y="0" width="100" height="100" />
    //   </pattern>
    // </defs>

    val id = Definitions.freshTemp("bg")
    val (w, h) = outerContext.current.dims

    val root = svgTag("pattern")
      .attr(
        "id" -> id,
        "patternUnits" -> "userSpaceOnUse",
        "width" -> w,
        "height" -> h
      )(
        svgTag("image").attr(
          "href" -> element,
          "x" -> 0,
          "y" -> 0,
          "width" -> w,
          "height" -> h
        )
      )
    root.define()

  //
  // PATH ELEMENTS
  //

  // CIRCLE

  case class Circle(var position: (Double, Double), var radius: Double)(using SVGContext):
    val circ = svgTag("circle")
      .attr("r", radius)
      .draw()

    def update(p: (Double, Double), r: Double) =
      position = p
      radius = r
      circ
        .attr("cx" -> position(0), "cy" -> position(1), "r" -> radius)

    def setPosition(p: (Double, Double)) =
      circ.attr("cx" -> p(0), "cy" -> p(1))
      position = p
    def setRadius(r: Double) =
      circ.attr("r", r)
      radius = r

    update(position, radius)

  // CURVE (CUBIC BEZIER)

  case class Curve(points: Seq[(Double, Double)])(using ctx: SVGContext) { self =>
    val path = svgTag("path")
    // We wanna make a spline through these points. This means figuring out
    // the control points and the handles. To approximate this we'll just
    // take each handle as the average of the vectors in each direction.

    def update(points: Seq[(Double, Double)]) =
      def getPointSaturating(i: Int): (Double, Double) =
        val k = if i < 0 then 0 else if i >= points.length then points.length - 1 else i
        points(k)

      val handles = points.zipWithIndex.map((p, i) =>
        val prev = getPointSaturating(i - 1)
        val next = getPointSaturating(i + 1)
        val dPrev = p - prev
        val dNext = next - p
        val handle = (dPrev + dNext) * 0.25
        handle
      )

      val paths = (0 until (points.length - 1))
        .map(i =>
          val p1 = points(i)
          val p2 = points(i + 1)
          val h1 = p1 + handles(i)
          val h2 = p2 - handles(i + 1)
          s"M ${p1.rawSVG} C ${h1.rawSVG}, ${h2.rawSVG}, ${p2.rawSVG}"
        )
        .mkString(" ")
      path.attr("d", paths)

    update(points)
    path.draw()
  }

  // RECTANGLE

  def order(a: Double, b: Double) =
    if a <= b then (a, b) else (b, a)

  case class Rectangle(var p1: (Double, Double), var p2: (Double, Double))(using ctx: SVGContext):
    val path = svgTag("rect")

    def update(nextP1: (Double, Double), nextP2: (Double, Double)) =
      p1 = nextP1; p2 = nextP2
      val (minx, maxx) = order(p1(0), p2(0))
      val (miny, maxy) = order(p1(1), p2(1))
      path
        .attr("x", minx.toInt)
        .attr("y", miny.toInt)
        .attr("width", (maxx - minx).toInt)
        .attr("height", (maxy - miny).toInt)

    update(p1, p2)
    path.draw()

  // PATH

  object Path:
    def empty(using SVGContext): Path = Path(Seq())

  case class Path(var points: Seq[(Double, Double)])(using ctx: SVGContext):
    val path = svgTag("path")
    var closed = false

    def update(newPoints: Seq[(Double, Double)]) =
      points = newPoints
      var pathString = if points.length > 0 then s"M ${points(0).rawSVG}" else ""
      var rest =
        if points.length > 1 then
          points.drop(1).map(p => s"L ${p.rawSVG}").reduce(_ + _) + (if closed then " z" else "")
        else ""
      path
        .attr("d", pathString + rest)

    def makeClosed =
      closed = true
      update(points)
      this

    update(points)
    path.draw()

  object PointPath:
    def empty(using SVGContext): PointPath = PointPath(Seq())

  case class PointPath(var points: Seq[(Double, Double)])(using ctx: SVGContext) { self =>
    val path = svgTag("path")
    val group = svgTag("g")(path)

    def newPathPoint =
      Circle((0, 0), 0).attr("opacity" -> 0).withClass("path-point")

    val pathPoints = ArrayBuffer.from(points.map(_ => newPathPoint))
    group(pathPoints.map(_.circ))

    var closed = false

    def update(newPoints: Seq[(Double, Double)]) =
      points = newPoints
      var pathString = if points.length > 0 then s"M ${points(0).rawSVG}" else ""
      var rest =
        if points.length > 1 then
          points.drop(1).map(p => s"L ${p.rawSVG}").reduce(_ + _) + (if closed then " z" else "")
        else ""
      path
        .attr("d", pathString + rest)

      // Lengthen collection if need be
      var diff = points.length - pathPoints.length
      while diff > 0 do
        val newPoint = newPathPoint
        pathPoints += newPoint
        group(newPoint.circ)
        diff -= 1
      while diff < 0 do pathPoints(pathPoints.length + diff).setRadius(0)
      for (point, circle) <- points.zip(pathPoints) do circle.update(point, 12)

    def makeClosed =
      closed = true
      update(points)
      self

    def getPoint(i: Int) =
      pathPoints(i)

    update(points)
    group.draw()
  }

  case class Line(var start: (Double, Double), var end: (Double, Double))(using ctx: SVGContext) {
    self =>
    val path = svgTag("line")
      .attr("stroke", "black")

    def update(newStart: (Double, Double), newEnd: (Double, Double)) =
      start = newStart
      end = newEnd
      path.attr(
        "x1" -> newStart(0),
        "y1" -> newStart(1),
        "x2" -> newEnd(0),
        "y2" -> newEnd(1)
      )

    update(start, end)
    path.draw()
  }

  case class Text(var content: String, var location: (Double, Double), var center: Boolean = true)(using
      ctx: SVGContext
  ):
    val path = svgTag("text")

    if center then
      path
        .attr("text-anchor", "middle")
        .attr("dominant-baseline", "central")

    def update(newText: String, newLoc: (Double, Double)) =
      content = newText
      location = newLoc
      path
        .attr(
          "x" -> location(0),
          "y" -> location(1)
        )
        .withHTML(content)

    update(content, location)
    path.draw()

  extension [T](element: SVGContext ?=> T)
    def withRoot(root: Element)(using context: SVGContext): T =
      context.push(SVG(root))
      val content = element(using context)
      for e <- context.current.elts do root.appendChild(e)
      context.pop
      content
