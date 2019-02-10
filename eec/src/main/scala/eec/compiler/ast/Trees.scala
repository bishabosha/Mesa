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

  enum Tree {
    case Select(tpe: Type, tree: Tree, name: Name)
    case Ident(tpe: Type, name: Name)
    case PackageDef(tpe: Type, pid: Tree, stats: List[Tree])
    case DefDef(tpe: Type, modifiers: Set[Modifier], sig: Tree, tpeAs: Tree, body: Tree)
    case DefSig(tpe: Type, name: Name, args: List[Name])
    case Apply(tpe: Type, id: Tree, args: List[Tree])
    case Function(tpe: Type, args: List[Tree], body: Tree)
    case Let(tpe: Type, name: Name, value: Tree, continuation: Tree)
    case If(tpe: Type, cond: Tree, thenp: Tree, elsep: Tree)
    case Literal(tpe: Type, constant: Constant)
    case CaseExpr(tpe: Type, selector: Tree, cases: List[Tree])
    case CaseClause(tpe: Type, pat: Tree, guard: Tree, body: Tree)
    case Alternative(tpe: Type, bodys: List[Tree])
    case Parens(tpe: Type, exprs: List[Tree])
    case Bind(tpe: Type, name: Name, body: Tree)
    case Unapply(tpe: Type, id: Tree, args: List[Tree])
    case Tagged(tpe: Type, arg: Name, tpeAs: Tree)
    case TreeSeq(args: List[Tree])
    case EmptyTree
  }

  val emptyIdent = Ident(untyped.uTpe, EmptyName)
  val wildcardIdent = Ident(untyped.uTpe, Wildcard)

  object TreeOps {

    import scala.annotation._

    def (tree: Tree) toList: List[Tree] = tree match {
      case EmptyTree            => Nil
      case TreeSeq(args)        => args
      case Parens(_, args)      => args
      case Alternative(_, args) => args
      case t                    => t :: Nil
    }

    def (trees: List[Tree]) toTree: Tree = trees match {
      case Nil      => EmptyTree
      case t :: Nil => t
      case ts       => TreeSeq(ts)
    }

    def (tree: Tree) toNames: List[Name] = {
      @tailrec def inner(acc: List[Name], t: Tree): List[Name] = t match {
        case Ident(_, name)         => name :: acc
        case Select(_, tree, name)  => inner(name :: acc, tree)
        case _                      => acc
      }
      inner(Nil, tree).reverse
    }

    def (tree: Tree) addModifiers(mods: Set[Modifier]): Tree = tree match {
      case d: DefDef  => d.copy(modifiers = (d.modifiers ++ mods))
      case _          => tree
    }
  }
}