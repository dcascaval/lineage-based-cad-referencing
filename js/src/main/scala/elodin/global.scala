package elodin.global.js

import scalajs.js
import org.scalajs.dom.{console, window}

extension [T](promise: js.Promise[T])
  def map[Q](f: T => Q): js.Promise[Q] =
    promise.`then`(f)
  def map[Q](f: => Q): js.Promise[Q] =
    promise.`then`(_ => f)
  def logErrors[Q] =
    promise.`catch`({
      val f = (error: Any) => console.warn(error)
      f
    })

def scalaJSStackTrace(error: Throwable) =
  console.warn(error)

def noWarn[T](f: => T): T =
  val l = js.Dynamic.global.console.warn
  val empty: js.Function1[Any, Unit] = (a: Any) => ()
  js.Dynamic.global.console.warn = empty
  val result = f
  js.Dynamic.global.console.warn = l
  result

inline val logging = true

inline def log(message: String, objects: Any*): Unit =
  inline if logging then console.log(message, objects*)

inline def time[T](inline f: T): T =
  val start = window.performance.now()
  val result = f
  val end = window.performance.now()
  println(f"[${end - start}%.2fms]")
  result

extension (i: Int) def dbl = i.toDouble
extension (d: Double) def int = d.toInt
