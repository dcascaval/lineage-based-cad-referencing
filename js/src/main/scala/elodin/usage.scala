import scalajs.js
import org.scalajs.dom.{document, console}
import org.scalajs.dom.raw.Event

import elodin.global.js.*

object Main:
  def main(args: Array[String]): Unit =
    val errorFn: js.Function5[String, String, Int, Int, Any, Unit] =
      (message: String, source: String, line: Int, column: Int, throwable: Any) =>
        if throwable.isInstanceOf[Throwable] then scalaJSStackTrace(throwable.asInstanceOf[Throwable])
        else console.log(throwable)

    val fnEmpty: js.Function0[Unit] = () => ()

    document.addEventListener(
      "DOMContentLoaded",
      (e: Event) =>
        js.Dynamic.global.window.onerror = errorFn

        try {
          elodin.opt.Composed.run()
        } catch {
          case except: Any =>
            scalaJSStackTrace(except.asInstanceOf[Throwable])
        }
    )
