package eec
package compiler
package core

object Contexts {

  import types.Types._
  import core.Names._
  import error.CompilerErrors._
  import scala.collection._
  import scala.annotation._

  enum Mode derives Eql {

    case PrimitiveType, Type, Term, Pat, PatAlt

    def isPattern = this match {
      case Pat | PatAlt => true
      case _            => false
    }

    def isType = this match {
      case Type | PrimitiveType => true
      case _                    => false
    }

    def isTerm = this match {
      case Term => true
      case _    => false
    }
  }

  object Mode {

    import util.Showable

    def mode given (m: Mode) = m

    implied for Showable[Mode] {
      def (m: Mode) userString: String = m match {
        case Pat | PatAlt => "pattern"
        case Term => "term"
        case PrimitiveType | Type => "type"
      }
    }
  }

  type Modal[X] = given Mode => X

  opaque type Id = Long

  object Id {
    private[Contexts] val rootId: Id = 0l
    val noId: Id = -1l
    val initId: Id = 1l
    def (x: Id) succ : Id = x + 1l
  }

  case class Sym(id: Id, name: Name)

  object RootSym extends Sym(Id.rootId, Name.From(rootString))

  type Scope = mutable.Buffer[(Sym, Context)]

  type TypeTable = mutable.AnyRefMap[Name, Type]

  sealed trait Context {
    import Context._
    import Id._
    import implied NameOps._

    val outer: Context
    val scope: Scope
    private[Contexts] val typeTable: TypeTable

    def putType(pair: (Name, Type)): Unit = {
      typeTable += pair
    }

    def getType(name: Name): Checked[Type] = {
      typeTable.getOrElse[Checked[Type]](
        name,
        CompilerError.UnexpectedType(s"no type found for name: ${name.userString}")
      )
    }

    def rootCtx: RootContext

    def firstCtx(name: Name): Checked[Context] = {
      import NameOps._
      import ContextOps._

      lazy val root = rootCtx

      @tailrec
      def inner(ctxIt: Context): Checked[Context] =
        if ctxIt.scope.view.map(_._1.name).contains(name) then
          ctxIt
        else if ctxIt eq root then
          CompilerError.IllegalState(s"name not found: ${name.userString}")
        else
          inner(ctxIt.outer)

      inner(this)
    }

    def lookIn(id: Id): Checked[Context] = {
      scope.collectFirst[Checked[Context]] {
        case (Sym(`id`, _), c) => c
      }.getOrElse {
        CompilerError.IllegalState(s"No context found for Id(${id})")
      }
    }

    def declarePackage(parent: Name, name: Name): Checked[Type] = {
      import CompilerErrorOps._
      import Type._
      // give package type
      for {
        parentTpe <- outer.getType(parent)
      } yield (
        parentTpe match {
          case p @ PackageInfo(_,_) => PackageInfo(p, name)
          case _ =>
            CompilerError.UnexpectedType(s"name `${parent.userString}` does not refer to a package")
        }
      )
    }
  }

  type Contextual[O] = given Context => O

  object Context {

    def ctx given (c: Context) = c

    def enterFresh(id: Id, name: Name): Contextual[Context] = {
      val newCtx = new Fresh(ctx, new mutable.ArrayBuffer, new mutable.AnyRefMap)
      ctx.scope += Sym(id, name) -> newCtx
      newCtx
    }

    def enterLeaf(id: Id, name: Name): Contextual[Unit] = {
      val newCtx = new Fresh(ctx, new mutable.ArrayBuffer, new mutable.AnyRefMap)
      ctx.scope += Sym(id, name) -> newCtx
    }

    def enterBootstrapped: Contextual[Checked[Unit]] = {
      import types.Types
      import Name._
      if ctx.rootCtx.scope.nonEmpty then {
        CompilerError.IllegalState("Non-fresh _root_ context")
      } else {
        Names.bootstrapped.foreach({ (name, tpe) =>
          enterLeaf(ctx.rootCtx.fresh, name) given ctx.rootCtx
          ctx.rootCtx.putType(name -> tpe)
        })
        ctx.rootCtx.putType(From(rootString), Types.rootPkg)
      }
    }
  }

  class Fresh private[Contexts] (override val outer: Context, override val scope: Scope, override val typeTable: TypeTable) extends Context {
    override def rootCtx = outer.rootCtx
  }

  class RootContext extends Context {
    import Id._

    private[this] val _typeTable: TypeTable = new mutable.AnyRefMap
    private[this] val _scope: Scope = new mutable.ArrayBuffer
    private[this] var _id: Id = Id.initId

    def fresh: Id = {
      val id = _id
      _id = _id.succ
      id
    }

    override val typeTable = _typeTable
    override def rootCtx = this
    override val outer = this
    override val scope = _scope
  }

  object ContextOps {
    import util.Showable
    import Scoping._

    enum Scoping derives Eql {
      case ScopeDef(id: Sym, scope: Scoping)
      case TypeDef(name: Name, tpe: Type)
      case Branch(scopes: Seq[Scoping], tpes: Seq[Scoping])
      case Empty
    }

    def (ctx: Context) toScoping: Scoping = {

      def branch(c: Context): Scoping = {
        val scopes = c.scope.map(p => ScopeDef(p._1, p._2.toScoping)).toList
        val tpes = c.typeTable.toList.map(TypeDef(_,_))
        Branch(scopes, tpes)
      }

      ctx match {
        case c: RootContext =>
          ScopeDef(RootSym, branch(c))
        case f: Fresh if f.scope.nonEmpty || f.typeTable.nonEmpty =>
          branch(f)
        case _ =>
          Empty
      }
    }
  }

}