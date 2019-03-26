package eec.compiler
package ast

import Trees.Tree._
import types.Types._
import Type._
import core.Names.Name._
import core.Contexts._

object untyped {
  type Tree       = Trees.Tree
  val uTpe        = Untyped
}

object any {
  val wildcardIdent = Ident(Wildcard)(Id.noId, WildcardType)
}