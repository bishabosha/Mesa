package mesa
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

import NameOps.given
import TypeOps.given

object Contexts {
  import Mode._
  import Id._
  import IdGen._
  import Context._

  type Scope            = mutable.Buffer[(Sym, Context)]
  type TypeTable        = mutable.Map[Name, Type]
  type PrimTable        = mutable.Buffer[Name]
  type ConstructorTable = mutable.MultiMap[Name, (Name, Type)]
  type Modal[X]         = (Mode)  ?=> X
  type Contextual[O]    = (Context)  ?=> O
  type IdReader[O]      = (IdGen)  ?=> O

  enum Mode derives CanEqual {
    case Typing, Term, Pat, PatAlt, LinearPat
  }

  object Mode {
    inline def mode(using mode: Mode) = mode

    def isLPattern(using Mode) =
      LinearPat == mode

    def isPattern(using Mode) =
      Pat == mode || PatAlt == mode || LinearPat == mode

    def isType(using Mode) =
      Typing == mode

    def isTerm(using Mode) =
      Term == mode
  }

  object ModeOps {
    given Show[Mode] = {
      case LinearPat    => "linear pattern"
      case Pat | PatAlt => "pattern"
      case Term         => "term"
      case Typing       => "typing"
    }
  }

  object opaques {
    opaque type Id = Long
    val one: Id = 1l
    val zero: Id = 0l
    val noId: Id = -1l
    extension (x: Id) def + (y: Id): Id = x + y
  }

  type Id = opaques.Id

  object Id {
    import opaques._
    private[Contexts] val rootId: Id = zero
    val init: Id = one
    val empty: Id = noId
    extension (x: Id) inline def succ : Id = x + one
  }

  final class IdGen {
    private var _id: Id = Id.init

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
    def idGen(using gen: IdGen) = gen
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
    def ctx(using ctx: Context) = ctx

    def resetLocalCtx(using Context): Unit = {
      ctx.localIdGen.reset
    }

    def removeFromCtx(id: Id)(using Context): Unit = {
      for mapping <- symFor(id) if id != Id.empty do
        ctx.termScope -= mapping
    }

    def rootCtx(using Context) = {
      extension (ctx: Context) @tailrec def root: Context = ctx match {
        case ctx: RootContext => ctx
        case ctx: Fresh       => ctx.outer.root
      }
      ctx.root
    }

    private def firstTermCtx(name: Name)(using Context): Lifted[Context] = {
      @tailrec
      def inner(current: Context): Lifted[Context] = {
        given Context = current
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

    private def firstTypeCtx(name: Name)(using Context): Lifted[Context] = {
      @tailrec
      def inner(current: Context): Lifted[Context] = {
        given Context = current
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

    def lookupType(name: Name)(using Context): Lifted[Type] = {
      firstTypeCtx(name).flatMap { ctx =>
        given Context = ctx
        dataType(name)
      }
    }

    def lookupConstructors(data: Name)(using Context): Lifted[List[(Name, Type)]] = {
      firstTypeCtx(data).flatMap { ctx =>
        given Context = ctx
        constructorsFor(data)
      }
    }

    private def constructorType(name: Name)(using Context): Lifted[Type] =
      if isConstructor(name) then termType(name)
      else Err.notACtor(name)

    def lookupConstructorType(ctor: Name)(using Context): Lifted[Type] =
      inTermCtx(ctor)(constructorType(ctor))

    def inTermCtx[O](name: Name)
                    (onFound: (Context)  ?=> Lifted[O])(using Context): Lifted[O] = {
      for
        ctx1 <- firstTermCtx(name)
        o <- lift {
          given Context = ctx1
          onFound
        }
      yield o
    }

    def isLinear(name: Name)(using Context): Boolean =
      ctx.linearScope.exists(_ == name)

    def isData(name: Name)(using Context): Boolean =
      ctx.dataTypeTable.keySet.contains(name)

    def isDataDeep(name: Name)(using Context): Boolean = {
      @tailrec
      def inner(current: Context): Boolean = {
        given Context = current
        isData(name) || {
          ctx match {
            case ctx: Fresh => inner(ctx.outer)
            case _          => false
          }
        }
      }
      inner(ctx)
    }

    def inScope(name: Name)(using Context): Boolean = {
      ctx.termScope
         .collectFirst { case (Sym(_, `name`), _) => () }
         .isDefined
    }

    def typedLinearVariable(using Context): Name = {
      val linearScope = ctx.linearScope
      if linearScope.flatMap(ctx.termTypeTable.get).isEmpty then {
        EmptyName
      } else {
        linearScope.get
      }
    }

    def termScopeContainsNames(using Context): Boolean = {
      ctx.termScope.collectFirst[Lifted[Context]] {
        case (Sym(_, name), c) if name.nonEmpty => c
      }.isDefined
    }

    def lookIn(id: Id)(using Context): Lifted[Context] = {
      ctx.termScope.collectFirst[Lifted[Context]] {
        case (Sym(`id`, _), c) => c
      }.getOrElse(Err.noCtxForId(id))
    }

    def symFor(id: Id)(using Context): Option[(Sym, Context)] = {
      ctx.termScope.find(_._1.id == id)
    }

    def lookForInScope(name: Name)(using Context): Lifted[Unit] =
      if name != Wildcard && !inScope(name) then Err.noVarInScope(name)
      else ()

    def lookForInLinear(name: Name)(using Context): Lifted[Unit] =
      if name != Wildcard && !isLinear(name) then Err.noVarInLinearScope(name)
      else ()

    def isConstructor(name: Name)(using Context): Boolean =
      ctx.constructorNames.contains(name)

    def constructorsFor(name: Name)(using Context): Lifted[List[(Name, Type)]] =
      for
        ctors <- ctx.constructorTable
                    .getOrElse[Lifted[mutable.Set[(Name, Type)]]](
                      name, Err.noCtors(name))
      yield ctors.toList

    def contains(name: Name)(using Context) = {
      name.nonEmpty && {
        isLinearOrInScope(name)
      }
    }

    def containsForLinear(name: Name)(using Context) = {
      contains(name) || ctx.linearScope.nonEmpty
    }

    def isLinearOrInScope(name: Name)(using Context) = {
      isLinear(name) || inScope(name)
    }

    def containsDataDeep(name: Name)(using Context) = {
      name.nonEmpty && isDataDeep(name)
    }

    def enterScope(id: Id, name: Name)(using Context): Lifted[Context] = {
      guardContains(name) {
        val newCtx = new Fresh(ctx)
        ctx.termScope += Sym(id, name) -> newCtx
        newCtx
      }
    }

    def enterVariable(name: Name)(using Context): Lifted[Unit] = {
      name.foldWildcard(()) {
        guardContains(_) {
          ctx.termScope += Sym(empty, name) -> new Fresh(ctx)
          ()
        }
      }
    }

    def enterData(name: Name)(using Context): Lifted[Unit] = {
      name.foldWildcard(()) {
        guardContainsData(_) {
          ctx.dataTypeTable += name -> EmptyType
          ()
        }
      }
    }

    def enterLinear(name: Name)(using Context): Lifted[Unit] = {
      guardContainsForLinear(name) {
        name.foldEmptyName(()) { name =>
          ctx.linearScope = Some(name)
        }
      }
    }

    private def guardContainsForLinear[O](name: Name)
                                         (f: (Context)  ?=> Lifted[O])(using Context) =
      guardContainsImpl(name)(containsForLinear)(f)(Err.shadowLinearMulti)

    private def guardContains[O](name: Name)
                                    (f: (Context)  ?=> Lifted[O])(using Context) =
      guardContainsImpl(name)(contains)(f)(Err.shadowVar)

    private def guardContainsData[O](name: Name)
                                    (f: (Context)  ?=> Lifted[O])(using Context) =
      guardContainsImpl(name)(containsDataDeep)(f)(Err.shadowData)

    private def guardContainsImpl[O](name: Name)
                                    (check: Name => (Context)  ?=> Boolean)
                                    (f: (Context)  ?=> Lifted[O])
                                    (msg: Name => CompilerError)(using Context): Lifted[O] =
      if check(name) then msg(name)
      else f

    def linkConstructor(name: Name, tpe: Type)(using Context): Unit = {
      import NameOps._
      val ret = tpe.toCurriedList.last.unwrapLinearBody
      dataDefinitionName(ret).foldEmptyName(()) { functor =>
        ctx.constructorTable.addBinding(functor, name -> tpe)
        ctx.constructorNames += name
      }
    }

    def putTermType(pair: (Name, Type))(using Context): Unit =
      ctx.termTypeTable += pair

    def termType(name: Name)(using Context): Lifted[Type] =
      if name == rootName && (ctx `eq` rootCtx) then rootPkg
      else ctx.termTypeTable.getOrElse[Lifted[Type]](name, Err.noType(name))

    def putDataType(pair: (Name, Type))(using Context): Unit =
      ctx.dataTypeTable += pair

    def dataType(name: Name)(using Context): Lifted[Type] =
      ctx.dataTypeTable.getOrElse[Lifted[Type]](name, Err.noData(name))

    extension (tpe: Type) def freshVariables(using Context): Type = {
      tpe.replaceVariables {
        _.updateDerivedStr(Synthetic(ctx.localIdGen.fresh(), _))
      }
    }

    def enterBootstrapped(using Context, IdGen): Lifted[Unit] = {
      val root = rootCtx
      if root.termScope.nonEmpty then {
        Err.noFreshScope
      } else if idGen.current != Id.init then {
        Err.noFreshIdGen
      } else {
        given Context = root
        for (name, tpe) <- bootstrapped.view do
          for _ <- enterData(name) do putDataType(name -> tpe)
      }
    }

    def declarePackage(parent: Name, name: Name)(using Context): Lifted[Type] = {
      val parentCtx = ctx match {
        case ctx: RootContext => ctx
        case ctx: Fresh       => ctx.outer
      }
      val parentTpe = lift {
        given Context = parentCtx
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
