package eec
package compiler
package ast

import EEC._

object EEC {
  type Ast = (List[ModuleInfo], List[TopStatement], List[Statement])
  type TopStatement = FixityStatement
  type Statement = Expression
  type Expression = Literals | PrefixExpr | Expr
  type Expressions = Statement | Operator
  type Literals = IntegerLiteral | LongLiteral | FloatLiteral | DoubleLiteral
}

enum Fixity {
  case Infixl, Infixr, Infix, Postfix, Prefix
}

case class FixityStatement(fixity: Fixity, strength: Int, op: Operator)
case class IntegerLiteral(value: Int)
case class LongLiteral(value: Long)
case class FloatLiteral(value: Float)
case class DoubleLiteral(value: Double)
case class ModuleInfo(symbol: String)
case class Operator(symbol: String)
case class Expr(expr: List[Expressions])
case class PrefixExpr(op: Operator, expr: Expression)