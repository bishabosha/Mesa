package mesa
package compiler
package core

import Names._
import Contexts.Id
import error.CompilerErrors._

import given NameOps._

object ContextErrors {

  def nameNotFound(name: Name) =
    CompilerError.UnknownIdentifier(s"name not found: ${name.define}")

  def dataNotFound(name: Name) =
    CompilerError.UnknownIdentifier(s"data definition not found: ${name.define}")

  def noCtors(data: Name) =
    CompilerError.UnexpectedType(s"${data.define} has no constructors.")

  def notACtor(name: Name) =
    CompilerError.UnexpectedType(
      s"${name.define} does not qualify to be a constructor.")

  def noVarInScope(name: Name) =
    CompilerError.UnknownIdentifier(
      s"No variable in immediate scope found for name `${name.define}`")

  def noVarInLinearScope(name: Name) =
    CompilerError.UnknownIdentifier(
      s"No variable in immediate linear scope found for name `${name.define}`")

  def noCtxForId(id: Id) =
    CompilerError.Internal(s"No context found for Id(${id})")

  def shadowVar(name: Name) =
    CompilerError.NameCollision(
      s"Illegal shadowing in scope of variable: ${name.define}")

  def shadowData(name: Name) =
    CompilerError.NameCollision(
      s"Illegal shadowing of imported data type ${name.define}")

  def shadowLinearMulti(name: Name) =
    CompilerError.NameCollision(
      s"Illegal attempt to put ${name.define} in linear context when it is already full.")

  def noType(name: Name) =
    CompilerError.UnknownIdentifier(s"no type found for term: ${name.define}")

  def noData(name: Name) =
    CompilerError.UnknownIdentifier(
      s"no data definition found for type: ${name.define}")

  def noParentPkg(parent: Name) =
    CompilerError.UnknownIdentifier(
      s"parent `${parent.define}` does not refer to a package")

  val noFreshScope = CompilerError.Internal("Non-fresh _root_ context")

  val noFreshIdGen = CompilerError.Internal("Non-fresh IdGen context")

  val emptyNameDefs =
    CompilerError.Internal("Definitions can not exist for empty name.")
}