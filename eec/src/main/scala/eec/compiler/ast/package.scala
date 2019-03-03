package eec
package compiler
package ast

import Trees.Tree._
import types.Types._
import core.Names.Name._
import core.Contexts._

object untyped {
  type Tree = Trees.Tree
  val uTpe = Type.Untyped
  val uTpeBoolean = Type.UntypedExpect(Bootstraps.BooleanType)
}

object typed {
  type Tree = Trees.Tree
}

val emptyIdent = Ident(EmptyName)(Id.noId, Type.NoType)
val wildcardIdent = Ident(Wildcard)(Id.noId, Type.NoType)