package eec.compiler
package ast

import Trees.{Tree, TreeOps}
import Tree._
import TreeOps._
import types.Types._
import Type._
import Bootstraps._
import core.Names.Name._
import core.Contexts._
import core.Constants._

object untyped {
  val nt       = EmptyType
  val litTrue  = Literal(constTrue)(nt)
  val litFalse = Literal(constFalse)(nt)
  val unit     = Parens(Nil)(nt)
}

object typed {
  val unit     = untyped.unit.withTpe(UnitType)
  val litTrue  = untyped.litTrue.withTpe(BooleanType)
  val litFalse = untyped.litFalse.withTpe(BooleanType)
}

object any {
  val wildcardIdent = Ident(Wildcard)(Id.empty, WildcardType)
}