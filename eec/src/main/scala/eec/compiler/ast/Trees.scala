package eec
package compiler
package ast

import Trees._
import Tree._
import Names._
import Constants._
import Modifiers._

object Trees {

  sealed trait ValDefTree extends Tree
  sealed trait RefTree extends ExprTree with TypeTree
  sealed trait TypeTree extends Tree
  sealed trait ExprTree extends Tree
  sealed trait SigTree extends Tree
  sealed trait MemberDefTree extends Tree
  sealed trait PatTree extends Tree

  enum Tree {
    case Select(tree: Tree, name: Name) extends RefTree
    case Ident(name: Name) extends RefTree with PatTree
    case PackageDef(pid: RefTree, stats: List[Tree])
    case Def(modifiers: Set[Modifier], sig: SigTree, typ: TypeTree, value: ExprTree) extends MemberDefTree
    case DefSig(name: Name, args: List[Name]) extends SigTree
    case FunctionType(arg: TypeTree, body: TypeTree) extends TypeTree
    case TypeApply(id: RefTree, args: List[TypeTree]) extends TypeTree
    case Function(args: List[ValDefTree], body: ExprTree) extends ExprTree
    case Let(name: Name, value: ExprTree, continuation: ExprTree) extends ExprTree
    case IfThenElse(cond: ExprTree, ifTrue: ExprTree, orElse: ExprTree) extends ExprTree
    case Apply(lhs: ExprTree, arg: ExprTree) extends ExprTree
    case Literal(constant: Constant) extends ExprTree with PatTree
    case CaseExpr(value: ExprTree, cases: List[CaseClause]) extends ExprTree
    case CaseClauses(cases: List[CaseClause])
    case CaseClause(pat: PatTree, guard: ExprTree, body: ExprTree)
    case Alternative(bodys: List[PatTree]) extends PatTree
    case Bind(name: Name, body: PatTree) extends PatTree
    case Unapply(id: RefTree, args: List[PatTree]) extends PatTree
    case Bindings(args: List[ValDefTree]) extends ValDefTree
    case Tagged(arg: Name, tpe: TypeTree) extends ValDefTree
    case EmptyTree extends TypeTree with ExprTree with MemberDefTree with SigTree with RefTree with ValDefTree with PatTree
  }

  val emptyIdent = new Ident(Name.Empty)
  val wildcardIdent = new Ident(Name.Wildcard)

  object ValDefTreeOps {
    def (tree: ValDefTree) toList: List[ValDefTree] = tree match {
      case EmptyTree => Nil
      case Bindings(args) => args
      case t: Tagged => t :: Nil
    }

    def (trees: List[ValDefTree]) toTree: ValDefTree = trees match {
      case Nil => EmptyTree
      case (t: Tagged) :: Nil => t
      case ts => new Bindings(ts)
    }
  }

  object TreeOps {

    def (tree: Tree) toList: List[Tree] = tree match {
      case EmptyTree => Nil
      case PackageDef(`emptyIdent`, stats) => stats
      case t => t :: Nil
    }

    def (trees: List[Tree]) toTree: Tree = trees match {
      case Nil => EmptyTree
      case t => PackageDef(emptyIdent, t)
    }

    def (tree: MemberDefTree) addModifiers(mods: Set[Modifier]): MemberDefTree =
      tree match {
        case d: Def => d.copy(modifiers = (d.modifiers ++ mods))
      }
  }
}