package eec
package compiler
package core

object Contexts {

  import types.Types._
  import Type._
  import core.Names._
  import Name._
  import error.CompilerErrors._
  import collection._
  import annotation._

  type Scope          = mutable.Buffer[(Sym, Context)]
  type TypeTable      = mutable.Map[Name, Type]
  type PrimTable      = mutable.Buffer[Name]
  type Modal[X]       = given Mode => X
  type Contextual[O]  = given Context => O
  type IdMaker[O]     = given IdGen => O
  opaque type Id      = Long

  enum Mode derives Eql {
    case PrimitiveType, Typing, Term, Pat, PatAlt
  }

  object Mode {

    inline def mode given (m: Mode) = m

    def isPattern given Mode =
      Pat     == mode ||
      PatAlt  == mode

    def isType given Mode =
      Typing        == mode ||
      PrimitiveType == mode

    def isTerm given Mode =
      Term == mode
  }

  object ModeOps {

    import Mode._
    import util.Showable

    implied for Showable[Mode] {
      def (m: Mode) userString: String = m match {
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
    import Id._

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

  private val rootPkg = PackageInfo(TypeRef(From(rootString)), From(rootString))
  private val rootSym = Sym(Id.rootId, Name.From(rootString))

  sealed trait Context (
    val scope: Scope,
    val typeTable: TypeTable,
    val primTable: PrimTable
  )

  final class RootContext extends Context(
    new mutable.ArrayBuffer,
    new mutable.AnyRefMap,
    new mutable.ArrayBuffer
  )

  final class Fresh private[Contexts](
    val outer: Context
  ) extends Context(
    new mutable.ArrayBuffer,
    new mutable.AnyRefMap,
    new mutable.ArrayBuffer
  )

  object Context {

    import implied NameOps._

    def ctx given (c: Context) = c

    def rootCtx given Context = {
      @tailrec
      def inner(ctx: Context): Context = ctx match {
        case _: RootContext => ctx
        case f: Fresh       => inner(f.outer)
      }
      inner(ctx)
    }

    def firstCtx(name: Name) given Context: Checked[Context] = {
      lazy val root = rootCtx

      @tailrec
      def inner(ctxIt: Context): Checked[Context] =
        if ctxIt.scope.view.map(_._1.name).contains(name) then
          ctxIt
        else ctxIt match {
          case _: RootContext =>
            CompilerError.IllegalState(s"name not found: ${name.userString}")
          case f: Fresh =>
            inner(f.outer)
        }

      inner(ctx)
    }

    def lookIn(id: Id) given Context: Checked[Context] =
      ctx.scope.collectFirst[Checked[Context]] {
        case (Sym(`id`, _), c) => c
      }.getOrElse {
        CompilerError.IllegalState(s"No context found for Id(${id})")
      }

    def contains(name: Name) given Context: Boolean = {
      name != Name.From(emptyString) && (
        ctx.scope.collectFirst { case (Sym(_, `name`), _) => () }
           .isDefined
      )
    }

    def enterFresh(id: Id, name: Name) given Context: Checked[Context] = {
      import implied NameOps._
      if contains(name) then
        CompilerError.UnexpectedType(
          s"Illegal shadowing in scope of name: ${name.userString}")
      else {
        val newCtx = new Fresh(ctx)
        ctx.scope += Sym(id, name) -> newCtx
        newCtx
      }
    }

    def commitId(id: Id) given Context: Unit = {
      val found = ctx.scope.collectFirst {
        case sym @ (Sym(`id`, name), _) => (sym, name)
      }
      found.foreach { (sym, name) =>
        if !ctx.typeTable.contains(name) then {
          ctx.scope -= sym
        }
      }
    }

    def isPrimitive(name: Name) given Context: Boolean = {
      ctx.primTable.contains(name)
    }

    def setPrimitive(name: Name) given Context: Unit = {
      ctx.primTable += name
    }

    def putType(pair: (Name, Type)) given Context: Unit = {
      ctx.typeTable += pair
    }

    def getType(name: Name) given Context: Checked[Type] = {
      lazy val root = rootCtx
      if name == Name.From(rootString) && (ctx `eq` root) then
        rootPkg
      else
        ctx.typeTable.getOrElse[Checked[Type]](
          name,
          CompilerError.UnexpectedType(
            s"no type found for name: ${name.userString}")
        )
    }

    def enterBootstrapped given Context, IdGen: Checked[Unit] = {
      import types.Types
      import Name._
      import CompilerErrorOps._
      import IdGen._
      lazy val root = rootCtx
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
      import CompilerErrorOps._
      import Type._
      val outer = ctx match {
        case _: RootContext => ctx
        case f: Fresh       => f.outer
      }
      checked {
        implied for Context = outer
        for (parentTpe <- getType(parent)) yield (
          parentTpe match {
            case p @ PackageInfo(_,_) =>
              val tpe = PackageInfo(p, name)
              putType(name, tpe)
              tpe
            case _ =>
              CompilerError.UnexpectedType(
                s"name `${parent.userString}` does not refer to a package")
          }
        )
      }
    }

    def isDefined(tpe: Type): Boolean =
      Names.bootstrapped.map(_._2).contains(tpe)
  }

  object ContextOps {
    import util.Showable
    import Scoping._

    enum Scoping derives Eql {
      case ScopeDef(name: Scoping, tpe: Scoping, scope: Seq[Scoping])
      case ForName(name: String)
      case TypeDef(tpe: String)
      case EmptyType
      case Empty
    }

    def (ctx: Context) toScoping: Seq[Scoping] = {
      import Name._
      import implied NameOps._
      import implied TypeOps._

      def branch(c: Context): Seq[Scoping] = {
        val scopes = c.scope.filter { pair =>
          val (Sym(_, name), context) = pair
          context.scope.nonEmpty || name != Name.From(emptyString)
        }.map { pair =>
          val (sym, context) = pair
          ScopeDef(
            ForName(sym.name.userString),
            c.typeTable.get(sym.name).map { t =>
              TypeDef(t.userString)
            }.getOrElse(EmptyType),
            context.toScoping
          )
        }.toList
        scopes
      }

      ctx match {
        case c: RootContext =>
          List(
            ScopeDef(ForName(rootSym.name.userString),
            TypeDef(rootPkg.userString), branch(c)))
        case f: Fresh if f.scope.nonEmpty || f.typeTable.nonEmpty =>
          branch(f)
        case _ =>
          Nil
      }
    }
  }
}