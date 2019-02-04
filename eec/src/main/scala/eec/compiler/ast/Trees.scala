package eec
package compiler
package ast

import Trees._
import Tree._
import core.Names._
import core.Names.Name._
import core.Constants._
import core.Modifiers._

object Trees {

  enum Tree {
    case Select(tree: Tree, name: Name)
    case Ident(name: Name)
    case PackageDef(pid: Tree, stats: List[Tree])
    case Def(modifiers: Set[Modifier], sig: Tree, typ: Tree, value: Tree)
    case DefSig(name: Name, args: List[Name])
    case FunctionType(arg: Tree, body: Tree)
    case Apply(id: Tree, args: List[Tree])
    case Function(args: List[Tree], body: Tree)
    case Let(name: Name, value: Tree, continuation: Tree)
    case IfThenElse(cond: Tree, ifTrue: Tree, orElse: Tree)
    case Literal(constant: Constant)
    case CaseExpr(value: Tree, cases: List[Tree])
    case CaseClause(pat: Tree, guard: Tree, body: Tree)
    case Alternative(bodys: List[Tree])
    case Bind(name: Name, body: Tree)
    case Unapply(id: Tree, args: List[Tree])
    case Tagged(arg: Name, tpe: Tree)
    case TreeSeq(args: List[Tree])
    case EmptyTree
  }

  val emptyIdent = Ident(EmptyName)
  val wildcardIdent = Ident(Wildcard)

  object TreeOps {
    import scala.annotation._

    def (tree: Tree) toList: List[Tree] = tree match {
      case EmptyTree => Nil
      case PackageDef(`emptyIdent`, stats) => stats
      case TreeSeq(args) => args
      case t => t :: Nil
    }

    def (trees: List[Tree]) toTree: Tree = trees match {
      case Nil => EmptyTree
      case t :: Nil => t
      case ts => PackageDef(emptyIdent, ts)
    }

    def (tree: Tree) toNames: List[Name] = {
      @tailrec def inner(acc: List[Name], t: Tree): List[Name] = t match {
        case Ident(name) => name :: acc
        case Select(tree, name) => inner(name :: acc, tree)
        case _ => acc
      }
      inner(Nil, tree).reverse
    }

    def (tree: Tree) addModifiers(mods: Set[Modifier]): Tree = tree match {
      case d: Def => d.copy(modifiers = (d.modifiers ++ mods))
      case otherwise => otherwise
    }
  }
}