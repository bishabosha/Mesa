package eec
package compiler
package ast

object Trees {

  import Trees._
  import Tree._
  import core.Names._
  import core.Names.Name._
  import core.Constants._
  import core.Modifiers._
  import types.Types._

  object untyped {
    type Tree = Trees.Tree
    val uTpe = Type.Untyped
  }

  object typed {
    type Tree = Trees.Tree
  }

  import core.Contexts._

  enum Tree(val id: Id, val tpe: Type) {
    case Select(tree: Tree, name: Name)(id: Id, tpe: Type) extends Tree(id, tpe)
    case Ident(name: Name)(id: Id, tpe: Type) extends Tree(id, tpe)
    case PackageDef(pid: Tree, stats: List[Tree])(id: Id, tpe: Type) extends Tree(id, tpe)
    case DefDef(modifiers: Set[Modifier], sig: Tree, tpeAs: Tree, body: Tree)(id: Id, tpe: Type) extends Tree(id, tpe)
    case DefSig(name: Name, args: List[Tree])(id: Id, tpe: Type) extends Tree(id, tpe)
    case Apply(f: Tree, args: List[Tree])(id: Id, tpe: Type) extends Tree(id, tpe)
    case Function(args: List[Tree], body: Tree)(id: Id, tpe: Type) extends Tree(id, tpe)
    case Let(name: Tree, value: Tree, continuation: Tree)(id: Id, tpe: Type) extends Tree(id, tpe)
    case If(cond: Tree, thenp: Tree, elsep: Tree)(id: Id, tpe: Type) extends Tree(id, tpe)
    case Literal(constant: Constant)(id: Id, tpe: Type) extends Tree(id, tpe)
    case CaseExpr(selector: Tree, cases: List[Tree])(id: Id, tpe: Type) extends Tree(id, tpe)
    case CaseClause(pat: Tree, guard: Tree, body: Tree)(id: Id, tpe: Type) extends Tree(id, tpe)
    case Alternative(bodys: List[Tree])(id: Id, tpe: Type) extends Tree(id, tpe)
    case Parens(exprs: List[Tree])(id: Id, tpe: Type) extends Tree(id, tpe)
    case Bind(name: Name, body: Tree)(id: Id, tpe: Type) extends Tree(id, tpe)
    case Unapply(f: Tree, args: List[Tree])(id: Id, tpe: Type) extends Tree(id, tpe)
    case Tagged(arg: Name, tpeAs: Tree)(id: Id, tpe: Type) extends Tree(id, tpe)
    case TreeSeq(args: List[Tree])(id: Id, tpe: Type) extends Tree(id, tpe)
    case EmptyTree extends Tree(Id.noId, Type.NoType)
  }

  val emptyIdent = Ident(EmptyName)(Id.noId, Type.NoType)
  val wildcardIdent = Ident(Wildcard)(Id.noId, Type.NoType)

  object TreeOps {

    import scala.annotation._
    import util.Showable

    implicit val TreeShowable: Showable[Tree] = new {
      import core.Printing.untyped.AstOps._
      override def (tree: Tree) userString: String = tree.toAst.toString
    }

    def (tree: Tree) toList: List[Tree] = tree match {
      case EmptyTree          => Nil
      case TreeSeq(args)      => args
      case Parens(args)       => args
      case Alternative(args)  => args
      case t                  => t :: Nil
    }

    def (trees: List[Tree]) toTree: Tree = trees match {
      case Nil      => EmptyTree
      case t :: Nil => t
      case ts       => TreeSeq(ts)(Id.noId, Type.NoType)
    }

    def (tree: Tree) toNames: List[Name] = {
      @tailrec def inner(acc: List[Name], t: Tree): List[Name] = t match {
        case Ident(name)        => name :: acc
        case Select(tree, name) => inner(name :: acc, tree)
        case _                  => acc
      }
      inner(Nil, tree)
    }

    def (tree: Tree) toNamePairs: List[(Id, Name)] = {
      @tailrec def inner(acc: List[(Id, Name)], t: Tree): List[(Id, Name)] = t match {
        case t @ Ident(name)    => (t.id, name) :: acc
        case t @ Select(tree, name) => inner((t.id, name) :: acc, tree)
        case _                  => acc
      }
      inner(Nil, tree)
    }

    def (tree: Tree) addModifiers(mods: Set[Modifier]): Tree = tree match {
      case d: DefDef  => d.copy(modifiers = (d.modifiers ++ mods))(d.id, d.tpe)
      case _          => tree
    }
  }
}