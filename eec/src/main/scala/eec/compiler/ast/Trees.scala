package eec
package compiler
package ast

object Trees {

  import core.Names._
  import core.Constants._
  import core.Contexts._
  import core.Modifiers._
  import types.Types._

  enum Tree(val id: Id, val tpe: Type) derives Eql {
    case Select(tree: Tree, name: Name)(id: Id, tpe: Type) extends Tree(id, tpe)
    case Ident(name: Name)(id: Id, tpe: Type) extends Tree(id, tpe)
    case PackageDef(pid: Tree, stats: List[Tree])(id: Id, tpe: Type) extends Tree(id, tpe)
    case DefDef(modifiers: Set[Modifier], sig: Tree, tpeAs: Tree, body: Tree)(id: Id, tpe: Type) extends Tree(id, tpe)
    case DefSig(name: Name, args: List[Tree])(id: Id, tpe: Type) extends Tree(id, tpe)
    case Apply(f: Tree, args: List[Tree])(id: Id, tpe: Type) extends Tree(id, tpe)
    case Function(args: List[Tree], body: Tree)(id: Id, tpe: Type) extends Tree(id, tpe)
    case Let(name: Tree, value: Tree, continuation: Tree)(id: Id, tpe: Type) extends Tree(id, tpe)
    case Literal(constant: Constant)(id: Id, tpe: Type) extends Tree(id, tpe)
    case CaseExpr(selector: Tree, cases: List[Tree])(id: Id, tpe: Type) extends Tree(id, tpe)
    case CaseClause(pat: Tree, guard: Tree, body: Tree)(id: Id, tpe: Type) extends Tree(id, tpe)
    case Alternative(bodys: List[Tree])(id: Id, tpe: Type) extends Tree(id, tpe)
    case Parens(exprs: List[Tree])(id: Id, tpe: Type) extends Tree(id, tpe)
    case Bind(name: Name, body: Tree)(id: Id, tpe: Type) extends Tree(id, tpe)
    // case Unapply(f: Tree, args: List[Tree])(id: Id, tpe: Type) extends Tree(id, tpe)
    case Tagged(arg: Name, tpeAs: Tree)(id: Id, tpe: Type) extends Tree(id, tpe)
    case TreeSeq(args: List[Tree]) extends Tree(Id.noId, Type.NoType)
    case EmptyTree extends Tree(Id.noId, Type.NoType)
  }

  object TreeOps {

    import scala.annotation._
    import util.{Showable, |>, Convert}
    import Trees.Tree._
    import core.Printing.untyped.Ast
    import implied core.Printing.untyped.AstOps._

    implied for Showable[Tree] {
      def (t: Tree) userString = toAst(t).toString
    }

    implied for (Tree |> List[Tree]) {
      def apply(t: Tree) = t match {
        case EmptyTree          => Nil
        case TreeSeq(args)      => args
        case Parens(args)       => args
        case Alternative(args)  => args
        case t                  => t :: Nil
      }
    }

    implied for (List[Tree] |> Tree) {
      def apply(ts: List[Tree]) = ts match {
        case Nil      => EmptyTree
        case t :: Nil => t
        case ts       => TreeSeq(ts)
      }
    }

    implied toNames for (Tree |> List[Name]) {
      def apply(t: Tree) = toNames(Nil, t)
    }

    implied toNamePairs for (Tree |> List[(Id, Name)]) {
      def apply(t: Tree) = toPairs(Nil, t)
    }

    def (tree: Tree) addModifiers(mods: Set[Modifier]): Tree = tree match {
      case d: DefDef  => d.copy(modifiers = (d.modifiers ++ mods))(d.id, d.tpe)
      case _          => tree
    }

    @tailrec
    private[this] def toNames(acc: List[Name], t: Tree): List[Name] = t match {
      case Ident(name)        => name :: acc
      case Select(tree, name) => toNames(name :: acc, tree)
      case _                  => acc
    }

    @tailrec
    private[this] def toPairs(acc: List[(Id, Name)], t: Tree): List[(Id, Name)] = t match {
      case t @ Ident(name)        => (t.id, name) :: acc
      case t @ Select(tree, name) => toPairs((t.id, name) :: acc, tree)
      case _                      => acc
    }
  }
}