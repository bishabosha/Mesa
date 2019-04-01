package eec
package compiler
package types

import error.CompilerErrors._
import ast.Trees._
import core.Contexts._
import Mode._

import implied TreeOps._
import implied ModeOps._

object NamerErrors {
  def namingMissing(tree: Tree) given Mode = {
    CompilerError.IllegalState(
      s"Naming not implemented for <${mode.show}, ${tree.show}>")
  }
}