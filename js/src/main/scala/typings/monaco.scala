package typings.monaco

import scalajs.js
import org.scalajs.dom.{Element}
import org.scalajs.dom.raw.{KeyboardEvent, HTMLElement}
import js.annotation.*

trait MinimapOptions extends js.Object:
  var enabled: js.UndefOr[Boolean] = js.undefined

trait CreateEditorOptions extends js.Object:
  var value: js.UndefOr[String] = js.undefined
  var language: js.UndefOr[String] = js.undefined
  var theme: js.UndefOr[String] = js.undefined
  var minimap: js.UndefOr[MinimapOptions] = js.undefined
  var automaticLayout: js.UndefOr[Boolean] = js.undefined
  var lineNumbersMinChars: js.UndefOr[Int] = js.undefined

trait ITokenThemeRule extends js.Object:
  var token: String
  var foreground: js.UndefOr[String] = js.undefined
  var background: js.UndefOr[String] = js.undefined
  var fontStyle: js.UndefOr[String] = js.undefined

trait IThemeData extends js.Object:
  var base: String
  var inherit: Boolean
  var rules: js.Array[ITokenThemeRule]
  var colors: IColors

trait IColors extends js.Object:
  @JSName("editor.background")
  var background: js.UndefOr[String] = js.undefined

@js.native
@JSGlobal
object monaco extends js.Object:

  @js.native
  trait IKeyboardEvent extends js.Object:
    val browserEvent: KeyboardEvent
    val target: HTMLElement
    val ctrlKey: Boolean
    val shiftKey: Boolean
    val altKey: Boolean
    val metaKey: Boolean
    val keyCode: Int
    val code: String
    @JSName("equals")
    def _equals(keybinding: Int): Boolean
    def preventDefault(): Unit
    def stopPropagation(): Unit

  @js.native
  trait ITextModel extends js.Object:
    def getLinesContent(): js.Array[String] = js.native

  @js.native
  trait ICodeEditor extends js.Object:
    def onKeyDown(listener: js.Function1[IKeyboardEvent, Any]): Unit = js.native
    def getModel(): ITextModel = js.native

  @js.native
  object editor extends js.Object:
    def defineTheme(themeName: String, themeData: IThemeData): Unit = js.native
    def setTheme(themeName: String): Unit = js.native
    def create(element: Element, opts: CreateEditorOptions): ICodeEditor = js.native
