package eec
package compiler
package core

object Contexts {

  import types.Types._
  import core.Names._
  import error.CompilerErrors._
  import scala.collection._
  import scala.annotation._

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

  type TypeTable = mutable.AnyRefMap[Name, Type]

  sealed trait Context {
    import Context._
    import Id._
    import NameOps._

    val outer: Context
    val scope: Scope
    private[Context] val typeTable: TypeTable

    def putType(name: Name, tpe: Type): Unit = {
      typeTable += name -> tpe
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
  }

  type Contextual[O] = implicit Context => O

  object Context {

    def ctx(implicit c: Context) = c

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
      if ctx.rootCtx.scope.nonEmpty then {
        CompilerError.IllegalState("Non-fresh _root_ context")
      } else {
        Names.bootstrapped.foreach({ (name, tpe) =>
          enterLeaf(ctx.rootCtx.fresh, name)(ctx.rootCtx)
          ctx.rootCtx.putType(name, tpe)
        })
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
      _id += Id(1l)
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

    enum Scoping {
      case ScopeDef(id: Sym, scope: Scoping)
      case Branch(scopes: Seq[Scoping])
      case Empty
    }

    def (ctx: Context) toScoping: Scoping = ctx match {
      case c: RootContext =>
        val foo = c.scope.map(p => ScopeDef(p._1, p._2.toScoping)).toList
        ScopeDef(RootSym, Branch(foo))
      case f: Fresh if f.scope.nonEmpty =>
        Branch(f.scope.map(p => ScopeDef(p._1, p._2.toScoping)).toList)
      case _ =>
        Empty
    }
  }

}