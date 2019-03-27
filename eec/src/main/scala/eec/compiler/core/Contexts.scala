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
import types.Types._
import Type._
import TypeOps._
import util.Showable

import implied NameOps._
import implied TypeOps._

object Contexts {
  import Mode._
  import Id._
  import IdGen._

  type Scope          = mutable.Buffer[(Sym, Context)]
  type TypeTable      = mutable.Map[Name, Type]
  type PrimTable      = mutable.Buffer[Name]
  type Modal[X]       = given Mode => X
  type Contextual[O]  = given Context => O
  type IdReader[O]    = given IdGen => O
  opaque type Id      = Long

  enum Mode derives Eql {
    case PrimitiveType, Typing, Term, Pat, PatAlt
  }

  object Mode {
    inline def mode given (mode: Mode) = mode

    def isPattern given Mode =
      Pat == mode || PatAlt == mode

    def isType given Mode =
      Typing == mode || PrimitiveType == mode

    def isTerm given Mode =
      Term == mode
  }

  object ModeOps {
    implied for Showable[Mode] {
      def (m: Mode) show: String = m match {
        case Pat | PatAlt => "pattern"
        case Term => "term"
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
    PackageInfo(TypeRef(rootString.readAs), rootString.readAs)

  private val rootSym =
    Sym(Id.rootId, rootString.readAs)

  sealed trait Context (
    val scope: Scope,
    val typeTable: TypeTable,
    val primTable: PrimTable,
    val localIdGen: IdGen
  )

  final class RootContext extends Context(
    new mutable.ArrayBuffer,
    new mutable.AnyRefMap,
    new mutable.ArrayBuffer,
    new IdGen
  )

  final class Fresh private[Contexts](
    val outer: Context
  ) extends Context(
    new mutable.ArrayBuffer,
    new mutable.AnyRefMap,
    new mutable.ArrayBuffer,
    new IdGen
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

    def firstCtx(name: Name) given Context: Checked[Context] = {
      @tailrec
      def inner(ctx: Context): Checked[Context] = {
        if ctx.scope.view.map(_._1.name).contains(name) then
          ctx
        else ctx match {
          case _: RootContext =>
            CompilerError.IllegalState(s"name not found: ${name.show}")

          case ctx: Fresh => inner(ctx.outer)
        }
      }
      inner(ctx)
    }

    def lookIn(id: Id) given Context: Checked[Context] = {
      ctx.scope.collectFirst[Checked[Context]] {
        case (Sym(`id`, _), c) => c
      }.getOrElse {
        CompilerError.IllegalState(s"No context found for Id(${id})")
      }
    }

    def contains(name: Name) given Context: Boolean = {
      name != emptyString.readAs && {
        ctx.scope.collectFirst { case (Sym(_, `name`), _) => () }
           .isDefined
      }
    }

    def enterFresh(id: Id, name: Name) given Context: Checked[Context] = {
      if contains(name) then {
        CompilerError.UnexpectedType(
          s"Illegal shadowing in scope of name: ${name.show}")
      } else {
        val newCtx = new Fresh(ctx)
        ctx.scope += Sym(id, name) -> newCtx
        newCtx
      }
    }

    def commitId(id: Id) given Context: Unit = {
      val first = ctx.scope.collectFirst {
        case sym @ (Sym(`id`, name), _) => (sym, name)
      }
      for ((sym, name) <- first) {
        if !ctx.typeTable.contains(name) then {
          ctx.scope -= sym
        }
      }
    }

    def isPrimitive(name: Name) given Context: Boolean =
      ctx.primTable.contains(name)

    def setPrimitive(name: Name) given Context: Unit =
      ctx.primTable += name

    def putType(pair: (Name, Type)) given Context: Unit =
      ctx.typeTable += pair

    def getType(name: Name) given Context: Checked[Type] = {
      if name == rootString.readAs && (ctx `eq` rootCtx) then
        rootPkg
      else
        ctx.typeTable.getOrElse[Checked[Type]](
          name,
          CompilerError.UnexpectedType(
            s"no type found for name: ${name.show}")
        )
    }

    def (tpe: Type) freshVariables given Context: Type = {
      tpe.replaceVariables {
        _.updateDerivedStr(Synthetic(ctx.localIdGen.fresh(), _))
      }
    }

    def enterBootstrapped given Context, IdGen: Checked[Unit] = {
      val root = rootCtx
      if root.scope.nonEmpty then {
        CompilerError.IllegalState("Non-fresh _root_ context")
      } else if idGen.id != Id.initId then {
        CompilerError.IllegalState("Non-fresh IdGen context")
      } else {
        implied for Context = root
        Names.bootstrapped.foreach { (name, tpe) =>
          for (_ <- enterFresh(idGen.fresh(), name))
            yield {
              putType(name -> tpe)
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
        getType(parent)
      }
      parentTpe.flatMap {
        case pkg: PackageInfo =>
            val tpe = PackageInfo(pkg, name)
            putType(name, tpe)
            tpe

        case _ =>
          CompilerError.UnexpectedType(
            s"name `${parent.show}` does not refer to a package")
      }
    }

    def isDefined(tpe: Type): Boolean =
      Names.bootstrapped.map(_._2).contains(tpe)
  }

  object ContextOps {
    import Scoping._

    enum Scoping derives Eql {
      case ScopeDef(name: Scoping, tpe: Scoping, scope: Seq[Scoping])
      case ForName(name: String)
      case TypeDef(tpe: String)
      case EmptyType
      case Empty
    }

    def (ctx: Context) toScoping: Seq[Scoping] = {

      def branch(c: Context): Seq[Scoping] = {
        for {
          pair <- c.scope.filter { pair =>
                    val (Sym(_, name), context) = pair
                    context.scope.nonEmpty || name != emptyString.readAs
                  }
          (sym, context)  = pair
          tpeOpt          = c.typeTable.get(sym.name)
          tpe             = tpeOpt.fold(EmptyType)(t => TypeDef(t.show))
          name            = sym.name.show
        } yield ScopeDef(ForName(name), tpe, context.toScoping)
      }

      def (ctx: Fresh) hasMembers = ctx.scope.nonEmpty || ctx.typeTable.nonEmpty

      ctx match {
        case ctx: RootContext =>
          List(
            ScopeDef(ForName(rootSym.name.show),
            TypeDef(rootPkg.show), branch(ctx))
          )

        case ctx: Fresh if ctx.hasMembers => branch(ctx)
        case _                            => Nil
      }
    }
  }
}