package elodin.opt.dsl

import org.scalajs.dom.{console, window}
import scala.meta.*
import elodin.opt.VarType
import typings.three.geometries.EdgesGeometry

def defaultBound(mid: Double) =
  val min = math.min(0.0, mid * 2.0)
  val max = math.max(0.0, mid * 2.0)
  ParamBounds(min, mid, max)

def asNumber(t: Term): Double =
  t match
    case d: Lit.Double => d.value.asInstanceOf[Double]
    case i: Lit.Int    => i.value.toDouble

def parseParameterBlock(tree: Term.Apply): ParameterStmt =
  val stmts = tree.args(0).asInstanceOf[Term.Block].stats
  ParameterStmt(stmts.map(s =>
    val assign = s.asInstanceOf[Term.Assign]
    val varName = assign.lhs.asInstanceOf[Term.Name].value
    val bound = assign.rhs match
      case d: Lit.Double => defaultBound(d.value.asInstanceOf[Double])
      case i: Lit.Int    => defaultBound(i.value.toDouble)
      case op: Term.ApplyInfix =>
        val upper = asNumber(op.args(0))
        val lhs = op.lhs.asInstanceOf[Term.ApplyInfix]
        val lower = asNumber(lhs.lhs)
        val mid = asNumber(lhs.args(0))
        ParamBounds(lower, mid, upper)
    (varName, bound)
  ))

def parseExpr(tree: Tree): Expression =
  tree match
    case n: Term.Name =>
      Variable(n.value)
    case d: Lit.Double => Literal(d.value.asInstanceOf[Double])
    case i: Lit.Int    => Literal(i.value)
    case call: Term.Apply =>
      val args = call.args.map(parseExpr)
      call.fun match
        case name: Term.Name => FnCall(name.value, args)
        case access: Term.Select =>
          val rcv = parseExpr(access.qual)
          val name = access.name.value
          MethodCall(rcv, name, args)
        case other =>
          val rcv = parseExpr(other)
          Apply(rcv, args)
    case op: Term.ApplyInfix =>
      val lhs = parseExpr(op.lhs)
      val symbol = op.op.value
      val rhs = parseExpr(op.args(0))
      BinopExpr(symbol, lhs, rhs)
    case op: Term.ApplyUnary =>
      val symbol = op.op.value
      val rhs = parseExpr(op.arg)
      UnopExpr(symbol, rhs)
    case fn: Term.Function =>
      val ps = fn.params.map(_.name.value)
      val body = parseExpr(fn.body)
      Lambda(ps, body)
    case access: Term.Select =>
      val lhs = parseExpr(access.qual)
      val rhs = access.name.value
      MethodCall(lhs, rhs, Seq())
    case exprs: Term.Block =>
      Block(exprs.stats.map(parseStmt))

def isOpDefn(defn: Term.ApplyInfix) =
  defn.lhs.isInstanceOf[Term.Name] &&
    defn.lhs.asInstanceOf[Term.Name].value == "operation"

def parseArgList(t: Term): Seq[String] =
  def parseArg(t: Term): String =
    t match
      case a: Term.Ascribe => a.expr.asInstanceOf[Term.Name].value
      case a: Term.Name    => a.value
  t match
    case lit: Lit.Unit   => Seq()
    case tup: Term.Tuple => tup.args.map(parseArg)
    case _               => Seq(parseArg(t))

def parseStmt(tree: Tree): Expression =
  tree match
    case a: Term.Assign =>
      val lhs = a.lhs.asInstanceOf[Term.Name].value
      val rhs = parseExpr(a.rhs)
      Assignment(lhs, rhs)
    case opDefn: Term.ApplyInfix if isOpDefn(opDefn) =>
      val opName = opDefn.op.value
      val argBlock = opDefn.args(0).asInstanceOf[Term.Apply]
      val argNames = parseArgList(argBlock.fun)
      val fnBody = argBlock.args(0).asInstanceOf[Term.Block].stats.map(parseStmt)
      FnDefn(opName, argNames, fnBody)
    case _ => parseExpr(tree)

def toDSLTree(t: Tree): Seq[AST] =
  val List(ps, stmts*) = t
    .asInstanceOf[Source]
    .stats(0)
    .asInstanceOf[Defn.Object]
    .templ
    .stats: @unchecked

  val block = parseParameterBlock(ps.asInstanceOf[Term.Apply])
  val rest = stmts.map(parseStmt)
  block +: rest

def parse(src: String): Seq[AST] =
  val start = window.performance.now()

  val patch = s"object Main {\n$src\n}"
  val tree = dialects.Scala3(patch).parse[Source].get
  val result = toDSLTree(tree)

  val end = window.performance.now()
  // println(f"[Parsed in ${end - start}%.2fms]")
  result
