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
    PackageInfo(TypeRef(rootName), rootName)

  private val rootSym =
    Sym(Id.rootId, rootName)

  sealed trait Context (
    val scope: Scope,
    var linearScope: Option[Name],
    val typeTable: TypeTable,
    val constructorTable: ConstructorTable,
    val constructorNames: mutable.Buffer[Name],
    val localIdGen: IdGen,
  )

  final class RootContext() extends Context(
    new mutable.ArrayBuffer,
    None,
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

    def firstCtx(name: Name) given Context: Checked[Context] = {
      @tailrec
      def inner(current: Context): Checked[Context] = {
        implied for Context = current
        if contains(name) then
          ctx
        else ctx match {
          case _: RootContext =>
            CompilerError.IllegalState(s"name not found: ${name.show}")

          case ctx: Fresh => inner(ctx.outer)
        }
      }
      inner(ctx)
    }

    def firstCtxConstructors(name: Name) given Context: Checked[Context] = {
      @tailrec
      def inner(current: Context): Checked[Context] = {
        implied for Context = current
        if containsConstructorsFor(name) then
          ctx
        else ctx match {
          case _: RootContext =>
            CompilerError.IllegalState(s"constructor not found: ${name.show}")

          case ctx: Fresh => inner(ctx.outer)
        }
      }
      inner(ctx)
    }

    def isLinear(name: Name) given Context: Boolean = {
      ctx.linearScope.filter(_ == name).isDefined
    }

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

    def inScope(name: Name) given Context: Boolean = {
      ctx.scope
         .collectFirst { case (Sym(_, `name`), _) => () }
         .isDefined
    }

    def uniqueLinearVariable given Context: Checked[Name] = {
      if scopeContainsNames then {
        CompilerError.UnexpectedType(s"Context contains non linear variables.")
      } else {
        val linearScope = ctx.linearScope
        if linearScope.flatMap(ctx.typeTable.get).isEmpty then {
          CompilerError.UnexpectedType(
            "Context does not contain a typed linear variable")
        } else {
          linearScope.get
        }
      }
    }

    def scopeContainsNames given Context: Boolean = {
      ctx.scope.collectFirst[Checked[Context]] {
        case (Sym(_, name), c) if name.nonEmpty => c
      }.isDefined
    }

    def lookIn(id: Id) given Context: Checked[Context] = {
      ctx.scope.collectFirst[Checked[Context]] {
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

    def containsConstructorsFor(name: Name) given Context: Boolean =
      ctx.constructorTable.keySet.contains(name)

    def constructorsFor(name: Name)
                       given Context: Checked[List[(Name, Type)]] =
      for {
        ctors <- ctx.constructorTable
                    .getOrElse[Checked[mutable.Set[(Name, Type)]]](
                      name, CompilerError.UnexpectedType(
                        s"Could not find constructors for ${name.show}"))
      } yield ctors.toList

    def contains(name: Name) given Context = {
      name.nonEmpty && {
        isLinear(name) || inScope(name)
      }
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
        ctx.scope += Sym(id, name) -> newCtx
        newCtx
      }
    }

    def enterVariable(name: Name) given Context: Checked[Unit] = {
      name.foldWildcard(()) {
        guardContainsDeep(_) {
          ctx.scope += Sym(noId, name) -> new Fresh(ctx)
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
        s"Illegal shadowing in scope of name: ${n.show}"
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

    def commitId(id: Id) given Context: Unit = {
      val first = {
        ctx.scope.collectFirst {
          case mapping @ (Sym(`id`, name), _) => (mapping, name)
        }
      }
      for ((mapping, name) <- first) {
        if !ctx.typeTable.contains(name) then {
          ctx.scope -= mapping
        }
      }
    }

    def linkPrimitive(name: Name, tpe: Type) given Context: Unit = {
      import NameOps._
      val ret = toCurriedList(tpe).last.unwrapLinearBody
      constructorEligableName(ret).foldEmptyName(()) { functor =>
        ctx.constructorTable.addBinding(functor, name -> tpe)
        ctx.constructorNames += name
      }
    }

    def putType(pair: (Name, Type)) given Context: Unit =
      ctx.typeTable += pair

    def getType(name: Name) given Context: Checked[Type] = {
      if name == rootName && (ctx `eq` rootCtx) then
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
        Types.bootstrapped.foreach { (name, tpe) =>
          for (_ <- enterVariable(name))
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
      Types.bootstrapped.map(_._2).contains(tpe)
  }

  object ContextOps {
    import Scoping._

    enum Scoping derives Eql {
      case LinearScope(name: Scoping, tpe: Scoping)
      case NamedScope(name: Scoping, tpe: Scoping, scope: Seq[Scoping])
      case AnonScope(scope: Seq[Scoping])
      case ForName(name: String)
      case TypeDef(tpe: String)
      case EmptyType
      case Empty
    }

    def (ctx: Context) toScoping: Seq[Scoping] = {

      def branch(ctx: Context): Seq[Scoping] = {
        val linearScopeOpt = {
          for {
            name <- ctx.linearScope
            tpe  = ctx.typeTable.get(name) match {
              case Some(t) => TypeDef(t.show)
              case _       => EmptyType
            }
          } yield LinearScope(ForName(name.show), tpe)
        }
        val defs = {
          for {
            pair <- ctx.scope.filter { pair =>
                      val (Sym(_, name), child) = pair
                      child.scope.nonEmpty || child.linearScope.nonEmpty || name.nonEmpty
                    }
            (sym, child)  = pair
            tpeOpt        = ctx.typeTable.get(sym.name)
            tpe           = tpeOpt.fold(EmptyType)(t => TypeDef(t.show))
            name          = sym.name
          } yield {
            if name.nonEmpty then
              NamedScope(ForName(name.show), tpe, child.toScoping)
            else
              AnonScope(child.toScoping)
          }
        }
        linearScopeOpt.toList ++ defs
      }

      def (ctx: Fresh) hasMembers =
        ctx.scope.nonEmpty || ctx.typeTable.nonEmpty || ctx.linearScope.isDefined

      ctx match {
        case ctx: RootContext =>
          List(
            NamedScope(ForName(rootSym.name.show),
            TypeDef(rootPkg.show), branch(ctx))
          )

        case ctx: Fresh if ctx.hasMembers => branch(ctx)
        case _                            => Nil
      }
    }
  }
}