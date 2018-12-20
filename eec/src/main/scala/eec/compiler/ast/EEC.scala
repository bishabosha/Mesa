package eec
package compiler
package ast

import EEC._

object EEC {
  type Ast = (List[ModuleInfo], List[TopStatement])
  type TopStatement = FixityStatement | Expr
  type Expr = List[Expressions]
  type Expressions = Literals | PrefixExpr | Operator | PrimaryExpr
  type Literals = IntegerLiteral
}

enum Fixity {
  case Infixl(strength: Int)
  case Infixr(strength: Int)
  case Infix(strength: Int)
  case Postfix(strength: Int)
  case Prefix(strength: Int)
}

case class FixityStatement(fixity: Fixity, op: Operator)
case class IntegerLiteral(value: Int)
case class ModuleInfo(symbol: String)
case class Operator(symbol: String)

case class PrefixExpr(op: Operator, expr: Expr)
case class PrimaryExpr(expr: Expr)