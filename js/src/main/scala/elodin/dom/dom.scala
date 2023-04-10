package elodin.dom

import scalajs.js
import org.scalajs.dom.{document, Element, Event, KeyboardEvent}
import org.scalajs.dom.raw.{
  HTMLOptionElement,
  HTMLSelectElement,
  HTMLButtonElement,
  HTMLParagraphElement,
  HTMLDivElement,
  HTMLCanvasElement,
  HTMLAnchorElement,
  HTMLImageElement,
  HTMLInputElement,
  HTMLLabelElement,
  HTMLElement,
  HTMLTextAreaElement,
  HTMLStyleElement
}

import collection.mutable
import elodin.global.js.*
import svgapi.*

object dom:
  trait Domable[A]:
    def element(a: A): Element

  given [T <: Element]: Domable[T] with
    def element(e: T): T = e

  given Domable[Circle] = c => c.circ
  given Domable[Curve] = c => c.path
  given Domable[Rectangle] = p => p.path
  given Domable[Path] = p => p.path
  given Domable[PointPath] = p => p.path
  given Domable[Line] = l => l.path
  given Domable[Text] = t => t.path

  given [A]: Domable[Group[A]] = g => g.root
  given [T, R]: Domable[DynamicList[T, R]] = a => a.parent

  // Extensions to get a builder-pattern style syntax for SVG attributes,
  // Useful for things like styles, hierarchies, clipping, so on.
  extension [T, A <: T](element: A)(using Domable[T])
    def elt: Element = summon[Domable[T]].element(element)

    def withClass(cls: String): A =
      elt.classList.add(cls)
      element

    def withHTML(inner: String): A =
      elt.innerHTML = inner
      element

    def withStyle(styles: (String, Any)*): A =
      val text = styles.map((k, v) => s"${k}:${v};").reduce(_ + _)
      elt.setAttributeNS(null, "style", text)
      element

    def attr(key: String, value: Any): A =
      elt.setAttributeNS(null, key, value.toString)
      element

    def attr(mapping: (String, Any)*): A =
      val localElt = elt
      for (k, v) <- mapping do localElt.setAttributeNS(null, k, v.toString)
      element

    def style(styles: Style*): A = // Direct copy of `attr`
      val localElt = elt
      for style <- styles do elt.classList.add(style.className)
      element

    def draw()(using ctx: SVGContext): A =
      ctx.current.addElement(elt)
      element

    def define()(using ctx: SVGContext): A =
      ctx.current.addDefinition(elt)
      element

    def blur(b: Blur): A =
      elt.attr("filter", s"url(#${b.name})")
      element

    def clip(c: Clip[?]): A =
      elt.attr("clip-path", s"url(#${c.id})")
      element

    def mask(m: Mask[?]): A =
      elt.attr("mask", s"url(#${m.id})")
      element

    def mask(m: InverseMask[?]): A =
      elt.attr("mask", s"url(#${m.id})")
      element

    def maskOpacity(d: Double): A =
      elt.attr("fill" -> "white", "opacity" -> d)
      element

    def background(b: Background): A =
      elt.attr("fill", s"url(#${b.id})")
      element

    def gradient(g: Gradient): A =
      elt.attr("fill", s"url(#${g.id})")
      element

    /** Add text content `text` to this element's `.innerHTML` */
    inline def /(text: String): A =
      elt.innerHTML = text
      element

    /** Add class `cls` to this element */
    inline def apply(cls: String): A =
      elt.classList.add(cls)
      element

    /** Add class `cls` and/or id `id` to this element */
    inline def apply(cls: Option[String] = None, id: Option[String] = None): A =
      cls.map(clsName => elt.classList.add(clsName))
      id.map(idName => elt.setAttributeNS(null, "id", idName))
      element

    /** Add child nodes to this element */
    def apply(children: => Element*): A =
      val localElt = elt
      for child <- children do localElt.appendChild(child)
      element

    def apply(children: Traversable[Element]): A =
      for child <- children do elt.appendChild(child)
      element

    // Register an event listener only for a single invocation. We immediately remove
    // the listener when no longer needed, and additionally add a safeguard to make
    // sure the interior action is never called more than once even if there are pending
    // events already fired towards this listener.
    def listenOnce[Q <: Event](event: String, action: js.Function1[Q, ?]): A =
      var called = false
      var stopListening: () => Unit = null
      var listener: js.Function1[Q, ?] = (e: Q) =>
        stopListening()
        if !called then
          action(e)
          called = true
      stopListening = element.listen[Q](event, listener)
      element

    def listen[Q <: Event](event: String, action: js.Function1[Q, ?]): () => Unit =
      var listener: js.Function1[Q, ?] = (e: Q) =>
        try {
          action(e)
        } catch {
          case except: Any =>
            scalaJSStackTrace(except.asInstanceOf[Throwable])
        }

      elt.addEventListener(event, listener)
      val stopListening = () => elt.removeEventListener(event, listener)
      stopListening

    def listenBlocking[Q <: Event](event: String, action: js.Function1[Q, ?]): Option[() => Unit] =
      val activeEvents = DOMEventMapping.map.getOrElse(elt, mutable.Set())
      if !(activeEvents contains event) then
        activeEvents += event
        val stop = elt.listen(event, action)
        Some(stop)
      else None

    def keyDown(key: String, stop: Boolean = false)(f: => Any): A =
      elt.listen(
        "keydown",
        (e: KeyboardEvent) =>
          // println(s"key = ${e.key}")
          if e.key == key then
            f
            if stop then
              e.preventDefault()
              e.stopPropagation()
      )
      element

    def keyPress(key: String)(f: => Any): A =
      elt.listen(
        "keypress",
        (e: KeyboardEvent) => if e.key == key then f
      )
      element

    def withHover(pt: (Double, Double), text: String): A =
      val textElt = () =>
        tag("p")
          .withHTML(text)
          .withClass("reference-hover")
          .attr("style", s"position: absolute; top:${pt(1)}px; left:${pt(0)}px;")
      var lastElt: Element = null;
      elt.listen(
        "mouseover",
        _ =>
          lastElt = textElt()
          document.body.appendChild(lastElt)
      )
      elt.listen("mouseout", _ => document.body.removeChild(lastElt))
      element

  object DOMEventMapping:
    val map = mutable.Map[Element, mutable.Set[String]]()

  type Tag = "option" | "select" | "button" | "div" | "p" | "a" | "canvas" | "img" | "input" | "label" |
    "code" | "style" | "textarea"

  type ConstructElement[T <: Tag] = T match {
    case "option"   => HTMLOptionElement
    case "select"   => HTMLSelectElement
    case "button"   => HTMLButtonElement
    case "div"      => HTMLDivElement
    case "p"        => HTMLParagraphElement
    case "canvas"   => HTMLCanvasElement
    case "a"        => HTMLAnchorElement
    case "img"      => HTMLImageElement
    case "input"    => HTMLInputElement
    case "label"    => HTMLLabelElement
    case "code"     => HTMLElement
    case "style"    => HTMLStyleElement
    case "textarea" => HTMLTextAreaElement
    case Any        => Element
  }

  transparent inline def tag[T <: Tag](tag: T): ConstructElement[T] =
    document.createElement(tag).asInstanceOf[ConstructElement[T]]

  object tags:
    def div = tag("div")
    def code = tag("code")
    def label = tag("label")
    def input = tag("input")
    def p = tag("p")

  class DynamicList[T, R: Domable](val parent: Element, init: T => R, update: (T, R) => Unit):
    val retainedList = mutable.Stack[R]()

    def apply(newElements: Seq[T]) =
      val previousLength = retainedList.length
      while (retainedList.length > newElements.length) do
        val oldElt = retainedList.pop().elt
        parent.removeChild(oldElt)
      while (newElements.length > retainedList.length) do
        val index = retainedList.length
        val newElt = init(newElements(index))
        parent.appendChild(newElt.elt)
        retainedList.push(newElt)

      for i <- 0 until retainedList.length do update(newElements(i), retainedList(i))

  class StaticList[T, R: Domable](val parent: Element, init: T => R, update: (T, R) => Unit):
    val innerList = DynamicList(parent, init, update)
    def apply(newElements: Seq[T]) = innerList(newElements)
