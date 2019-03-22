package eec
package compiler
package types

object NamerErrors {
  import error.CompilerErrors._
  import ast.Trees._
  import core.Contexts._

  def namingMissing(tree: Tree) given Mode = {
    import Mode._
    import implied TreeOps._
    import implied ModeOps._
    CompilerError.IllegalState(
      s"Naming not implemented for <${mode.show}, ${tree.show}>")
  }
}