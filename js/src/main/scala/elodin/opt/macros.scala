package elodin.opt

import scala.quoted.*

def showFunctionImpl[T: Type](f: Expr[T])(using Quotes): Expr[(String, T)] =
  import quotes.reflect.*
  val pos = f.asTerm.pos
  val src = pos.sourceFile
  // Windows might have touched this file, and we can't have that.
  val clean = pos.sourceCode.get.replace("\r", "")
  val cleanExpr = Expr(clean)
  '{ ($cleanExpr, $f) }

inline def showFunction[T](inline x: T): (String, T) = ${ showFunctionImpl('x) }

def showExecuteImpl[T: Type](using qctx: Quotes): Expr[String] =
  import qctx.reflect.*
  val sym = TypeRepr.of[T].typeSymbol
  val tree = sym.declaredMethod("execute")(0).tree.asInstanceOf[DefDef]
  val pos = tree.rhs.get.pos
  val clean = pos.sourceCode.get.replace("\r", "")
  Expr(clean)

inline def showExecute[T]: String = ${ showExecuteImpl[T] }

object Source:
  inline def line: Int = ${ lineImpl() }
  def lineImpl()(using Quotes): Expr[Int] =
    import quotes.reflect.*
    val pos = Position.ofMacroExpansion
    Expr(pos.startLine + 1)

def showTypeImpl[T: Type](using Quotes): Expr[String] = Expr(Type.show[T])

inline def typeName[T]: String = ${ showTypeImpl[T] }
