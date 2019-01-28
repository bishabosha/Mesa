package eec
package compiler
package ast

import EEC._

object EEC {
  type Ast = (List[ModuleInfo], List[TopStatement], List[Statement])
  type TopStatement = FixityStatement
  type Statement = Expression
  type Expression = Literals | Ident | PrefixExpr | TupleExpr | Expr | Let | Application | Lambda
  type Expressions = Statement | Operator
  type Literals = IntegerLiteral | LongLiteral | FloatLiteral | DoubleLiteral
  val eecUnit = TupleExpr(Vector())
}

enum Fixity {
  case Infixl, Infixr, Infix, Postfix, Prefix
}

case class Let(exp: Ident, be: Expression, in: Expression)
case class Application(exp: Ident, args: List[Expression])
case class Lambda(args: List[Ident], expr: Expression)
case class Ident(name: String)
case class TupleExpr(exprs: Vector[Expression])
case class FixityStatement(fixity: Fixity, strength: Int, ops: Vector[Operator])
case class IntegerLiteral(value: Int)
case class LongLiteral(value: Long)
case class FloatLiteral(value: Float)
case class DoubleLiteral(value: Double)
case class ModuleInfo(symbol: List[String])
case class Operator(symbol: String)
case class Expr(expr: List[Expressions])
case class PrefixExpr(op: Operator, expr: Expression)