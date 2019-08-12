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
import util.{Show, view}
import core.{ContextErrors => Err}

import delegate NameOps._
import delegate TypeOps._

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

  enum Mode derives Eql {
    case Typing, Term, Pat, PatAlt, LinearPat
  }

  object Mode {
    inline def mode given (mode: Mode) = mode

    def isLPattern given Mode =
      LinearPat == mode

    def isPattern given Mode =
      Pat == mode || PatAlt == mode || LinearPat == mode

    def isType given Mode =
      Typing == mode

    def isTerm given Mode =
      Term == mode
  }

  object ModeOps {
    delegate for Show[Mode] = {
      case LinearPat    => "linear pattern"
      case Pat | PatAlt => "pattern"
      case Term         => "term"
      case Typing       => "typing"
    }
  }

  object opaques {
    opaque type Id = Long
    inline val one: Id = 1l
    inline val zero: Id = 0l
    inline val noId: Id = -1l
    def (x: Id) + (y: Id): Id = x + y
  }

  type Id = opaques.Id

  object Id {
    import opaques._
    private[Contexts] val rootId: Id = zero
    val init: Id = one
    val empty: Id = noId
    inline def (x: Id) succ : Id = x + one
  }

  final class IdGen {
    private[this] var _id: Id = Id.init

    private[Contexts] def reset: Unit = {
      _id = Id.init
    }

    def fresh(): Id = {
      val id = _id
      _id = _id.succ
      id
    }

    def current = _id
  }

  object IdGen {
    def idGen given (gen: IdGen) = gen
  }

  final case class Sym(id: Id, name: Name)

  val rootPkg =
    PackageInfo(BaseType(rootName), rootName)

  val rootSym =
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

    def resetLocalCtx given Context: Unit = {
      ctx.localIdGen.reset
    }

    def removeFromCtx(id: Id) given Context: Unit = {
      for mapping <- symFor(id) if id != Id.empty do
        ctx.termScope -= mapping
    }

    def rootCtx given Context = {
      @tailrec
      def (ctx: Context) root: Context = ctx match {
        case ctx: RootContext => ctx
        case ctx: Fresh       => ctx.outer.root
      }
      ctx.root
    }

    private def firstTermCtx(name: Name) given Context: Lifted[Context] = {
      @tailrec
      def inner(current: Context): Lifted[Context] = {
        delegate for Context = current
        if isLinearOrInScope(name) then
          ctx
        else ctx match {
          case _: RootContext => Err.nameNotFound(name)
          case ctx: Fresh     => inner(ctx.outer)
        }
      }
      if name == EmptyName then Err.emptyNameDefs
      else inner(ctx)
    }

    private def firstTypeCtx(name: Name) given Context: Lifted[Context] = {
      @tailrec
      def inner(current: Context): Lifted[Context] = {
        delegate for Context = current
        if isData(name) then
          ctx
        else ctx match {
          case _: RootContext => Err.dataNotFound(name)
          case ctx: Fresh     => inner(ctx.outer)
        }
      }
      if name == EmptyName then Err.emptyNameDefs
      else inner(ctx)
    }

    def lookupType(name: Name) given Context: Lifted[Type] = {
      firstTypeCtx(name).flatMap { ctx =>
        delegate for Context = ctx
        dataType(name)
      }
    }

    def lookupConstructors(data: Name) given Context: Lifted[List[(Name, Type)]] = {
      firstTypeCtx(data).flatMap { ctx =>
        delegate for Context = ctx
        constructorsFor(data)
      }
    }

    private def constructorType(name: Name) given Context: Lifted[Type] =
      if isConstructor(name) then termType(name)
      else Err.notACtor(name)

    def lookupConstructorType(ctor: Name) given Context: Lifted[Type] =
      inTermCtx(ctor)(constructorType(ctor))

    def inTermCtx[O](name: Name)
                    (onFound: given Context => Lifted[O])
                    given Context: Lifted[O] = {
      for
        ctx1 <- firstTermCtx(name)
        o <- lift {
          delegate for Context = ctx1
          onFound
        }
      yield o
    }

    def isLinear(name: Name) given Context: Boolean =
      ctx.linearScope.exists(_ == name)

    def isData(name: Name) given Context: Boolean =
      ctx.dataTypeTable.keySet.contains(name)

    def isDataDeep(name: Name) given Context: Boolean = {
      @tailrec
      def inner(current: Context): Boolean = {
        delegate for Context = current
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

    def typedLinearVariable given Context: Name = {
      val linearScope = ctx.linearScope
      if linearScope.flatMap(ctx.termTypeTable.get).isEmpty then {
        EmptyName
      } else {
        linearScope.get
      }
    }

    def termScopeContainsNames given Context: Boolean = {
      ctx.termScope.collectFirst[Lifted[Context]] {
        case (Sym(_, name), c) if name.nonEmpty => c
      }.isDefined
    }

    def lookIn(id: Id) given Context: Lifted[Context] = {
      ctx.termScope.collectFirst[Lifted[Context]] {
        case (Sym(`id`, _), c) => c
      }.getOrElse(Err.noCtxForId(id))
    }

    def symFor(id: Id) given Context: Option[(Sym, Context)] = {
      ctx.termScope.find(_._1.id == id)
    }

    def lookForInScope(name: Name) given Context: Lifted[Unit] =
      if name != Wildcard && !inScope(name) then Err.noVarInScope(name)
      else ()

    def lookForInLinear(name: Name) given Context: Lifted[Unit] =
      if name != Wildcard && !isLinear(name) then Err.noVarInLinearScope(name)
      else ()

    def isConstructor(name: Name) given Context: Boolean =
      ctx.constructorNames.contains(name)

    def constructorsFor(name: Name)
                       given Context: Lifted[List[(Name, Type)]] =
      for
        ctors <- ctx.constructorTable
                    .getOrElse[Lifted[mutable.Set[(Name, Type)]]](
                      name, Err.noCtors(name))
      yield ctors.toList

    def contains(name: Name) given Context = {
      name.nonEmpty && {
        isLinearOrInScope(name)
      }
    }

    def containsForLinear(name: Name) given Context = {
      contains(name) || ctx.linearScope.nonEmpty
    }

    def isLinearOrInScope(name: Name) given Context = {
      isLinear(name) || inScope(name)
    }

    def containsDataDeep(name: Name) given Context = {
      name.nonEmpty && isDataDeep(name)
    }

    def enterScope(id: Id, name: Name) given Context: Lifted[Context] = {
      guardContains(name) {
        val newCtx = new Fresh(ctx)
        ctx.termScope += Sym(id, name) -> newCtx
        newCtx
      }
    }

    def enterVariable(name: Name) given Context: Lifted[Unit] = {
      name.foldWildcard(()) {
        guardContains(_) {
          ctx.termScope += Sym(empty, name) -> new Fresh(ctx)
          ()
        }
      }
    }

    def enterData(name: Name) given Context: Lifted[Unit] = {
      name.foldWildcard(()) {
        guardContainsData(_) {
          ctx.dataTypeTable += name -> EmptyType
          ()
        }
      }
    }

    def enterLinear(name: Name) given Context: Lifted[Unit] = {
      guardContainsForLinear(name) {
        name.foldEmptyName(()) { name =>
          ctx.linearScope = Some(name)
        }
      }
    }

    private def guardContainsForLinear[O](name: Name)
                                         (f: given Context => Lifted[O])
                                         given Context =
      guardContainsImpl(name)(containsForLinear)(f)(Err.shadowLinearMulti)

    private def guardContains[O](name: Name)
                                    (f: given Context => Lifted[O])
                                    given Context =
      guardContainsImpl(name)(contains)(f)(Err.shadowVar)

    private def guardContainsData[O](name: Name)
                                    (f: given Context => Lifted[O])
                                    given Context =
      guardContainsImpl(name)(containsDataDeep)(f)(Err.shadowData)

    private def guardContainsImpl[O](name: Name)
                                    (check: Name => given Context => Boolean)
                                    (f: given Context => Lifted[O])
                                    (msg: Name => CompilerError)
                                    given Context: Lifted[O] =
      if check(name) then msg(name)
      else f

    def linkConstructor(name: Name, tpe: Type) given Context: Unit = {
      import NameOps._
      val ret = tpe.toCurriedList.last.unwrapLinearBody
      dataDefinitionName(ret).foldEmptyName(()) { functor =>
        ctx.constructorTable.addBinding(functor, name -> tpe)
        ctx.constructorNames += name
      }
    }

    def putTermType(pair: (Name, Type)) given Context: Unit =
      ctx.termTypeTable += pair

    def termType(name: Name) given Context: Lifted[Type] =
      if name == rootName && (ctx `eq` rootCtx) then rootPkg
      else ctx.termTypeTable.getOrElse[Lifted[Type]](name, Err.noType(name))

    def putDataType(pair: (Name, Type)) given Context: Unit =
      ctx.dataTypeTable += pair

    def dataType(name: Name) given Context: Lifted[Type] =
      ctx.dataTypeTable.getOrElse[Lifted[Type]](name, Err.noData(name))

    def (tpe: Type) freshVariables given Context: Type = {
      tpe.replaceVariables {
        _.updateDerivedStr(Synthetic(ctx.localIdGen.fresh(), _))
      }
    }

    def enterBootstrapped given Context, IdGen: Lifted[Unit] = {
      val root = rootCtx
      if root.termScope.nonEmpty then {
        Err.noFreshScope
      } else if idGen.current != Id.init then {
        Err.noFreshIdGen
      } else {
        delegate for Context = root
        for (name, tpe) <- bootstrapped.view do
          for _ <- enterData(name) do putDataType(name -> tpe)
      }
    }

    def declarePackage(parent: Name, name: Name)
                      given Context: Lifted[Type] = {
      val parentCtx = ctx match {
        case ctx: RootContext => ctx
        case ctx: Fresh       => ctx.outer
      }
      val parentTpe = lift {
        delegate for Context = parentCtx
        termType(parent)
      }
      parentTpe.flatMap {
        case pkg: PackageInfo =>
          val tpe = PackageInfo(pkg, name)
          putTermType(name, tpe)
          tpe

        case _ => Err.noParentPkg(parent)
      }
    }
  }
}