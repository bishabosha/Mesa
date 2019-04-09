package eec.compiler
package ast

import Trees.Tree._
import types.Types._
import Type._
import core.Names.Name._
import core.Contexts._
import core.Constants._

object untyped {
  val uTpe     = Untyped
  val litTrue  = Literal(constTrue)(uTpe)
  val litFalse = Literal(constFalse)(uTpe)
}

object any {
  val wildcardIdent = Ident(Wildcard)(Id.noId, WildcardType)
}