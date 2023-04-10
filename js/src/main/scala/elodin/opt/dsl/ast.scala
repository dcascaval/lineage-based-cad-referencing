package elodin.opt.dsl

sealed trait AST:
  def show: String

sealed trait Expression extends AST

case class ParamBounds(min: Double, mid: Double, max: Double)

case class ParameterStmt(parameters: Seq[(String, ParamBounds)]) extends AST:
  def show = parameters.map((n, v) => f"$n = ${v.mid}%.2f").mkString(", ")

case class Variable(name: String) extends Expression:
  def show = name

case class Literal(value: Double) extends Expression:
  def show = f"$value%.2f"

case class BinopExpr(
    symbol: String,
    lhs: Expression,
    rhs: Expression
) extends Expression:
  def show = f"(${lhs.show} $symbol ${rhs.show})"

case class UnopExpr(
    symbol: String,
    argument: Expression
) extends Expression:
  def show = f"($symbol${argument.show})"

case class Assignment(
    target: String,
    expression: Expression
) extends Expression:
  def show = f"$target = ${expression.show}"

case class FnCall(
    name: String,
    arguments: Seq[Expression]
) extends Expression:
  def show = f"$name(${arguments.show()})"

case class MethodCall(
    reciever: Expression,
    method: String,
    arguments: Seq[Expression]
) extends Expression:
  def show = f"${reciever.show}.$method(${arguments.show()})"

case class Lambda(
    args: Seq[String],
    body: Expression
) extends Expression:
  def show = f"(${args.mkString(", ")}) => ${body.show}"

case class Apply(
    reciever: Expression,
    arguments: Seq[Expression]
) extends Expression:
  def show = f"${reciever.show}(${arguments.show()})"

case class Block(
    statements: Seq[Expression]
) extends Expression:
  def show = f"{${statements.map(_.show).mkString("; ")}}"

case class FnDefn(
    name: String,
    args: Seq[String],
    statements: Seq[Expression]
) extends Expression:
  def show = f"operation $name(${args.mkString(", ")}) ${Block(statements).show}"

extension (args: Seq[Expression])
  def show(separator: String = ", ") =
    args.map(_.show).mkString(separator)
