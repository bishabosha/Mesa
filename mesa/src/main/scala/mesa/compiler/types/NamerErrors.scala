package mesa
package compiler
package types

import error.CompilerErrors._
import ast.Trees._
import core.Contexts._
import Mode._

import TreeOps.given
import ModeOps.given

object NamerErrors {
  def namingMissing(tree: Tree)(using Mode) = {
    CompilerError.Internal(
      s"No implementation for naming in mode ${mode.show} for tree given by ${tree.show}")
  }
}
