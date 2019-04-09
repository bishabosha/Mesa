package eec
package compiler
package core

import scala.collection.mutable
import scala.annotation.tailrec

import Names._
import Name._
import NameOps._
import Derived._
import error.CompilerErrors._
import CompilerErrorOps._
import types.Types
import Types._
import Type._
import TypeOps._
import util.Showable

import implied NameOps._
import implied TypeOps._

object Contexts {
  import Mode._
  import Id._
  import IdGen._
  import Context._

  type Scope            = mutable.Buffer[(Sym, Context)]
  type TypeTable        = mutable.Map[Name, Type]
  type PrimTable        = mutable.Buffer[Name]
  type ConstructorTable = mutable.MultiMap[Name, (Name, Type)]
  type Modal[X]         = given Mode => X
  type Contextual[O]    = given Context => O
  type IdReader[O]      = given IdGen => O
  opaque type Id        = Long

  enum Mode derives Eql {
    case PrimitiveType, Typing, Term, Pat, PatAlt, LinearPat
  }

  object Mode {
    inline def mode given (mode: Mode) = mode

    def isLPattern given Mode =
      LinearPat == mode

    def isPattern given Mode =
      Pat == mode || PatAlt == mode || LinearPat == mode

    def isType given Mode =
      Typing == mode || PrimitiveType == mode

    def isTerm given Mode =
      Term == mode
  }

  object ModeOps {
    implied for Showable[Mode] {
      def (m: Mode) show: String = m match {
        case LinearPat              => "linear pattern"
        case Pat | PatAlt           => "pattern"
        case Term                   => "term"
        case PrimitiveType | Typing => "typing"
      }
    }
  }

  object Id {
    private[Contexts] val rootId: Id = 0l
    val noId: Id = -1l
    val initId: Id = 1l
    def (x: Id) succ : Id = x + 1l
  }

  final class IdGen {
    private[this] var _id: Id = Id.initId

    def fresh(): Id = {
      val id = _id
      _id = _id.succ
      id
    }

    def id = _id
  }

  object IdGen {
    def idGen given (gen: IdGen) = gen
  }

  case class Sym(id: Id, name: Name)

  private val rootPkg =
    PackageInfo(BaseType(rootName), rootName)

  private val rootSym =
    Sym(Id.rootId, rootName)

  sealed trait Context (
    val termScope: Scope,
    var linearScope: Option[Name],
    val termTypeTable: TypeTable,
    val dataTypeTable: TypeTable,
    val constructorTable: ConstructorTable,
    val constructorNames: mutable.Buffer[Name],
    val localIdGen: IdGen,
  )

  final class RootContext() extends Context(
    new mutable.ArrayBuffer,
    None,
    new mutable.AnyRefMap,
    new mutable.AnyRefMap,
    new mutable.HashMap[Name, mutable.Set[(Name, Type)]] with ConstructorTable,
    new mutable.ArrayBuffer,
    new IdGen,
  )

  final class Fresh private[Contexts](
    val outer: Context
  ) extends Context(
    new mutable.ArrayBuffer,
    None,
    new mutable.AnyRefMap,
    new mutable.AnyRefMap,
    new mutable.HashMap[Name, mutable.Set[(Name, Type)]] with ConstructorTable,
    new mutable.ArrayBuffer,
    new IdGen,
  )

  object Context {
    def ctx given (ctx: Context) = ctx

    def rootCtx given Context = {
      @tailrec
      def inner(ctx: Context): Context = ctx match {
        case ctx: RootContext => ctx
        case ctx: Fresh       => inner(ctx.outer)
      }
      inner(ctx)
    }

    private def firstTermCtx(name: Name) given Context: Checked[Context] = {
      @tailrec
      def inner(current: Context): Checked[Context] = {
        implied for Context = current
        if isLinearOrInScope(name) then
          ctx
        else ctx match {
          case _: RootContext =>
            CompilerError.IllegalState(s"name not found: ${name.show}")

          case ctx: Fresh => inner(ctx.outer)
        }
      }
      if name == EmptyName then
        CompilerError.IllegalState(s"No defintions for empty name.")
      else
        inner(ctx)
    }

    private def firstTypeCtx(name: Name) given Context: Checked[Context] = {
      @tailrec
      def inner(current: Context): Checked[Context] = {
        implied for Context = current
        if isData(name) then
          ctx
        else ctx match {
          case _: RootContext =>
            CompilerError.IllegalState(s"data definition not found: ${name.show}")

          case ctx: Fresh => inner(ctx.outer)
        }
      }
      if name == EmptyName then
        CompilerError.IllegalState(s"No defintions for empty name.")
      else
        inner(ctx)
    }

    def lookupType(name: Name) given Context: Checked[Type] = {
      firstTypeCtx(name).flatMap { ctx =>
        implied for Context = ctx
        getDataType(name)
      }
    }

    def lookupConstructors(data: Name) given Context: Checked[List[(Name, Type)]] = {
      firstTypeCtx(data).flatMap { ctx =>
        implied for Context = ctx
        if constructorsExistFor(data) then constructorsFor(data)
        else CompilerError.IllegalState(s"${data.show} has no constructors.")
      }
    }

    private def getConstructorType(name: Name) given Context: Checked[Type] =
      if isConstructor(name) then {
        getTermType(name)
      } else {
        CompilerError.UnexpectedType(
          s"${name.show} does not qualify to be a constructor.")
      }

    def lookupConstructorType(ctor: Name) given Context: Checked[Type] = {
      inTermCtx(ctor)(getConstructorType(ctor))
    }

    def inTermCtx[O](name: Name)
                     (onFound: given Context => Checked[O])
                     given Context: Checked[O] = {
      for
        cCtx <- firstTermCtx(name)
        o    <- checked {
          implied for Context = cCtx
          onFound
        }
      yield o
    }

    def isLinear(name: Name) given Context: Boolean =
      ctx.linearScope.filter(_ == name).isDefined

    def isLinearDeep(name: Name) given Context: Boolean = {
      @tailrec
      def inner(current: Context): Boolean = {
        implied for Context = current
        isLinear(name) || {
          ctx match {
            case ctx: Fresh => inner(ctx.outer)
            case _          => false
          }
        }
      }
      inner(ctx)
    }

    def isData(name: Name) given Context: Boolean =
      ctx.dataTypeTable.keySet.contains(name)

    def isDataDeep(name: Name) given Context: Boolean = {
      @tailrec
      def inner(current: Context): Boolean = {
        implied for Context = current
        isData(name) || {
          ctx match {
            case ctx: Fresh => inner(ctx.outer)
            case _          => false
          }
        }
      }
      inner(ctx)
    }

    def inScope(name: Name) given Context: Boolean = {
      ctx.termScope
         .collectFirst { case (Sym(_, `name`), _) => () }
         .isDefined
    }

    def typedLinearVariable given Context: Checked[Name] = {
      val linearScope = ctx.linearScope
      if linearScope.flatMap(ctx.termTypeTable.get).isEmpty then {
        CompilerError.UnexpectedType(
          "Context does not contain a typed linear variable")
      } else {
        linearScope.get
      }
    }

    def termScopeContainsNames given Context: Boolean = {
      ctx.termScope.collectFirst[Checked[Context]] {
        case (Sym(_, name), c) if name.nonEmpty => c
      }.isDefined
    }

    def lookIn(id: Id) given Context: Checked[Context] = {
      ctx.termScope.collectFirst[Checked[Context]] {
        case (Sym(`id`, _), c) => c
      }.getOrElse {
        CompilerError.IllegalState(s"No context found for Id(${id})")
      }
    }

    def lookForInScope(name: Name) given Context: Checked[Unit] = {
      if name != Wildcard && !inScope(name) then {
        CompilerError.IllegalState(
          s"No variable in immediate scope found for name `${name.show}`")
      } else {
        ()
      }
    }

    def lookForInLinear(name: Name) given Context: Checked[Unit] = {
      if name != Wildcard && !isLinear(name) then {
        CompilerError.IllegalState(
          s"No variable in immediate linear scope found for name `${name.show}`")
      } else {
        ()
      }
    }

    def isConstructor(name: Name) given Context: Boolean =
      ctx.constructorNames.contains(name)

    def constructorsExistFor(name: Name) given Context: Boolean =
      ctx.constructorTable.keySet.contains(name)

    def constructorsFor(name: Name)
                       given Context: Checked[List[(Name, Type)]] =
      for
        ctors <- ctx.constructorTable
                    .getOrElse[Checked[mutable.Set[(Name, Type)]]](
                      name, CompilerError.UnexpectedType(
                        s"Could not find constructors for ${name.show}"))
      yield ctors.toList

    def contains(name: Name) given Context = {
      name.nonEmpty && {
        isLinearOrInScope(name)
      }
    }

    def isLinearOrInScope(name: Name) given Context = {
      isLinear(name) || inScope(name)
    }

    def containsDataDeep(name: Name) given Context = {
      name.nonEmpty && isDataDeep(name)
    }

    def containsForLinear(name: Name) given Context = {
      contains(name) || ctx.linearScope.nonEmpty
    }

    def containsDeep(name: Name) given Context = {
      name.nonEmpty && {
        isLinearDeep(name) || inScope(name)
      }
    }

    def enterScope(id: Id, name: Name) given Context: Checked[Context] = {
      guardContainsDeep(name) {
        val newCtx = new Fresh(ctx)
        ctx.termScope += Sym(id, name) -> newCtx
        newCtx
      }
    }

    def enterVariable(name: Name) given Context: Checked[Unit] = {
      name.foldWildcard(()) {
        guardContainsDeep(_) {
          ctx.termScope += Sym(noId, name) -> new Fresh(ctx)
          ()
        }
      }
    }

    def enterData(name: Name) given Context: Checked[Unit] = {
      name.foldWildcard(()) {
        guardContainsData(_) {
          ctx.dataTypeTable += name -> Untyped
          ()
        }
      }
    }

    def enterLinear(name: Name) given Context: Checked[Unit] = {
      guardContainsForLinear(name) {
        name.foldWildcard
          { CompilerError.UnexpectedType(
            "Illegal anonymous variable in linear context.") }
          { name =>
            ctx.linearScope = Some(name)
          }
      }
    }

    private def guardContainsForLinear[O](name: Name)
                                        (f: given Context => Checked[O])
                                        given Context = {
      guardContainsImpl(name)(containsForLinear)(f){ n =>
        s"Illegal attempt to put multiple variables in linear context with name: ${n.show}"
      }
    }

    private def guardContainsDeep[O](name: Name)
                                (f: given Context => Checked[O])
                                given Context = {
      guardContainsImpl(name)(containsDeep)(f) { n =>
        s"Illegal shadowing in scope of variable: ${n.show}"
      }
    }

    private def guardContainsData[O](name: Name)
                                    (f: given Context => Checked[O])
                                    given Context = {
      guardContainsImpl(name)(containsDataDeep)(f) { n =>
        s"Illegal shadowing of imported data type ${n.show}"
      }
    }

    private def guardContainsImpl[O](name: Name)
                                    (check: Name => given Context => Boolean)
                                    (f: given Context => Checked[O])
                                    (msg: Name => String)
                                    given Context: Checked[O] = {
      if check(name) then {
        CompilerError.UnexpectedType(msg(name))
      } else {
        f
      }
    }

    def linkConstructor(name: Name, tpe: Type) given Context: Unit = {
      import NameOps._
      val ret = toCurriedList(tpe).last.unwrapLinearBody
      dataDefinitionName(ret).foldEmptyName(()) { functor =>
        ctx.constructorTable.addBinding(functor, name -> tpe)
        ctx.constructorNames += name
      }
    }

    def putTermType(pair: (Name, Type)) given Context: Unit =
      ctx.termTypeTable += pair

    def getTermType(name: Name) given Context: Checked[Type] = {
      if name == rootName && (ctx `eq` rootCtx) then
        rootPkg
      else
        ctx.termTypeTable.getOrElse[Checked[Type]](
          name,
          CompilerError.UnexpectedType(
            s"no type found for term: ${name.show}")
        )
    }

    def putDataType(pair: (Name, Type)) given Context: Unit =
      ctx.dataTypeTable += pair

    def getDataType(name: Name) given Context: Checked[Type] = {
      ctx.dataTypeTable.getOrElse[Checked[Type]](
        name,
        CompilerError.UnexpectedType(
          s"no data definition found for type: ${name.show}")
      )
    }

    def (tpe: Type) freshVariables given Context: Type = {
      tpe.replaceVariables {
        _.updateDerivedStr(Synthetic(ctx.localIdGen.fresh(), _))
      }
    }

    def enterBootstrapped given Context, IdGen: Checked[Unit] = {
      val root = rootCtx
      if root.termScope.nonEmpty then {
        CompilerError.IllegalState("Non-fresh _root_ context")
      } else if idGen.id != Id.initId then {
        CompilerError.IllegalState("Non-fresh IdGen context")
      } else {
        implied for Context = root
        Types.bootstrapped.foreach { (name, tpe) =>
          for _ <- enterData(name)
          yield {
            putDataType(name -> tpe)
          }
        }
      }
    }

    def declarePackage(parent: Name, name: Name)
                      given Context: Checked[Type] = {
      val parentCtx = ctx match {
        case ctx: RootContext => ctx
        case ctx: Fresh       => ctx.outer
      }
      val parentTpe = checked {
        implied for Context = parentCtx
        getTermType(parent)
      }
      parentTpe.flatMap {
        case pkg: PackageInfo =>
          val tpe = PackageInfo(pkg, name)
          putTermType(name, tpe)
          tpe

        case _ =>
          CompilerError.UnexpectedType(
            s"name `${parent.show}` does not refer to a package")
      }
    }
  }

  object ContextOps {
    import Scoping._

    enum Scoping derives Eql {
      case LinearVariable(name: Scoping, tpe: Scoping)
      case Term(name: Scoping, tpe: Scoping)
      case TermScope(name: Scoping, tpe: Scoping, scope: Seq[Scoping])
      case Data(name: Scoping, tpe: Scoping)
      case AnonScope(scope: Seq[Scoping])
      case ForName(name: String)
      case OfType(tpe: String)
      case EmptyType
      case Empty
    }

    def (ctx: Context) toScoping: Seq[Scoping] = {

      def branch(ctx: Context): Seq[Scoping] = {
        val linearScopeOpt = {
          for
            name <- ctx.linearScope
            tpe  = ctx.termTypeTable.get(name) match {
              case Some(t) => OfType(t.show)
              case _       => EmptyType
            }
          yield LinearVariable(ForName(name.show), tpe)
        }
        val data = {
          for
            (name, tpe) <- ctx.dataTypeTable
          yield Data(ForName(name.show), OfType(tpe.show))
        }
        val defs = {
          for
            pair <- ctx.termScope.filter { pair =>
                      val (Sym(_, name), child) = pair
                      child.termScope.nonEmpty || child.linearScope.nonEmpty || name.nonEmpty
                    }
            (sym, child)  = pair
            tpeOpt        = ctx.termTypeTable.get(sym.name)
            tpe           = tpeOpt.fold(EmptyType)(t => OfType(t.show))
            name          = sym.name
          yield {
            if name.nonEmpty then {
              if child.termScope.isEmpty then
                Term(ForName(name.show), tpe)
              else
                TermScope(ForName(name.show), tpe, child.toScoping)
            } else {
              AnonScope(child.toScoping)
            }
          }
        }
        linearScopeOpt.toList ++ data.toList ++ defs
      }

      def (ctx: Fresh) hasMembers =
        ctx.termScope.nonEmpty || ctx.termTypeTable.nonEmpty || ctx.linearScope.isDefined

      ctx match {
        case ctx: RootContext =>
          List(
            TermScope(ForName(rootSym.name.show),
            OfType(rootPkg.show), branch(ctx))
          )

        case ctx: Fresh if ctx.hasMembers => branch(ctx)
        case _                            => Nil
      }
    }
  }
}