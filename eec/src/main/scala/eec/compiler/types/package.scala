package eec
package compiler
package object types {
  
  import Types._
  import ast.Trees._
  import errors.TyperErrors._
  import errors.TyperErrors.TyperError._

  def (expr: Tree) typ: Type | TyperError =
    TypeError("Types are not implemented")
}