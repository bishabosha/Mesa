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
      s"Tree given by `${tree.show}` has no implementation for naming in mode ${mode.show}.")
  }
}