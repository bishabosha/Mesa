package eec
package compiler
package core

object Contexts {

  import types.Types._
  import core.Names._
  import error.CompilerErrors._
  import scala.collection._

  enum Mode {

    case Type, Term, Pat, PatAlt //, PackageSelect

    def isPattern = this match {
      case Pat | PatAlt => true
      case _            => false
    }
  }

  object Mode {

    import util.Showable

    def mode(implicit m: Mode) = m

    implicit val ModeShowable: Showable[Mode] = new {
      override def (m: Mode) userString: String = m match {
        case Pat | PatAlt => "pattern"
        case Term => "term"
        case Type => "type"
      }
    }
  }

  type Modal[X] = implicit Mode => X

  opaque type Id = Long

  object Id {
    private[Contexts] val rootId: Id = 0l
    val noId: Id = -1l
    val initId: Id = 1l
    private[Contexts] def apply(l: Long): Id = l
    def (x: Id) + (y: Id): Id = x + y
    def (s: Id) toLong: Long = s
  }

  case class Sym(id: Id, name: Name)

  object RootSym extends Sym(Id.rootId, Name.From(rootString))

  type Scope = mutable.Buffer[(Sym, Context)]

  type TypeTable = mutable.LongMap[Type]

  sealed trait Context {
    import Context._
    val outer: Context
    val scope: Scope

    def rootCtx: RootContext
  }

  type Contextual[O] = implicit Context => O

  object Context {

    def ctx(implicit c: Context) = c

    def enterFresh(id: Id, name: Name): Contextual[Context] = {
      val newCtx = Fresh(ctx, new mutable.ArrayBuffer)
      ctx.scope += Sym(id, name) -> newCtx
      newCtx
    }

    def enterLeaf(id: Id, name: Name): Contextual[Unit] = {
      val newCtx = Leaf(ctx, new mutable.ArrayBuffer)
      ctx.scope += Sym(id, name) -> newCtx
    }

    def enterBootstrapped: Contextual[Checked[Unit]] = {
      if ctx.rootCtx.scope.nonEmpty then {
        CompilerError.IllegalState("Non-fresh _root_ context")
      } else {
        Names.bootstrapped.foreach(enterLeaf(ctx.rootCtx.fresh, _)(ctx.rootCtx))
      }
    }
  }

  case class Fresh private[Contexts] (override val outer: Context, override val scope: Scope) extends Context {
    override def rootCtx = outer.rootCtx
  }

  case class Leaf private[Contexts] (override val outer: Context, override val scope: Scope) extends Context {
    override def rootCtx = outer.rootCtx
  }

  case class RootContext() extends Context {
    import Id._

    private[this] val _scope: Scope = new mutable.ArrayBuffer
    private[this] var _typeTable: TypeTable = new mutable.LongMap
    private[this] var _id: Id = Id.initId

    def fresh: Id = {
      val id = _id
      _id += Id(1l)
      id
    }

    def putType(id: Id, tpe: Type): Unit = {
      _typeTable += id.toLong -> tpe
    }

    def getType(id: Id): Type = {
      _typeTable(id.toLong)
    }

    override def rootCtx = this
    override val outer = this
    override val scope = _scope
  }

  object ContextOps {
    import util.Showable
    import Scoping._

    enum Scoping {
      case ScopeDef(id: Sym, scope: Scoping)
      case Branch(scopes: Seq[Scoping])
      case Empty
    }

    def (ctx: Context) toScoping: Scoping = ctx match {
      case c @ RootContext() =>
        val foo = c.scope.map(p => ScopeDef(p._1, p._2.toScoping)).toList
        ScopeDef(RootSym, Branch(foo))
      case Fresh(_,scope) =>
        Branch(scope.map(p => ScopeDef(p._1, p._2.toScoping)).toList)
      case l: Leaf => Empty
    }
  }

}