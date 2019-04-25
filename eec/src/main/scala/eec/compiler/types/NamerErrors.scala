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
    CompilerError.Internal(
      s"No implementation for naming in mode ${mode.show} for tree given by ${tree.show}")
  }
}