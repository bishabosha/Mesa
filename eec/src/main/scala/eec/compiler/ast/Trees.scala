package eec
package compiler
package ast

import core._
import Names._
import Name._
import Constants._
import Contexts._
import Modifiers._
import types.Types._
import annotation._
import util.{Showable, |>, Convert}
import Convert._

import implied Printing.untyped.AstOps._
import implied NameOps._

object Trees {
  import Tree._

  trait Unique(val id: Id)

  enum Tree(val tpe: Type) derives Eql {
    case Select(tree: Tree, name: Name)(id: Id, tpe: Type) extends Tree(tpe) with Unique(id)
    case Ident(name: Name)(id: Id, tpe: Type) extends Tree(tpe) with Unique(id)
    case PackageDef(pid: Tree, stats: List[Tree])(tpe: Type) extends Tree(tpe)
    case DefDef(modifiers: Set[Modifier], sig: Tree, tpeAs: Tree, body: Tree)(tpe: Type) extends Tree(tpe)
    case DefSig(name: Name, args: List[Name])(id: Id, tpe: Type) extends Tree(tpe) with Unique(id)
    case LinearSig(name: Name, args: List[Name], linear: Name)(id: Id, tpe: Type) extends Tree(tpe) with Unique(id)
    case Apply(f: Tree, args: List[Tree])(tpe: Type) extends Tree(tpe)
    case InfixApply(f: Tree, left: Tree, right: Tree)(tpe: Type) extends Tree(tpe)
    case Eval(f: Tree, arg: Tree)(tpe: Type) extends Tree(tpe)
    case Tensor(value: Tree, computation: Tree)(tpe: Type) extends Tree(tpe)
    case Function(args: List[Tree], body: Tree)(id: Id, tpe: Type) extends Tree(tpe) with Unique(id)
    case LinearFunction(arg: Tree, body: Tree)(id: Id, tpe: Type) extends Tree(tpe) with Unique(id)
    case Let(x: Name, value: Tree, continuation: Tree)(id: Id, tpe: Type) extends Tree(tpe) with Unique(id)
    case LetTensor(x: Name, z: Name, s: Tree, continuation: Tree)(id: Id, tpe: Type) extends Tree(tpe) with Unique(id)
    case Literal(constant: Constant)(tpe: Type) extends Tree(tpe)
    case CaseExpr(selector: Tree, cases: List[Tree])(tpe: Type) extends Tree(tpe)
    case CaseClause(pat: Tree, guard: Tree, body: Tree)(id: Id, tpe: Type) extends Tree(tpe) with Unique(id)
    case LinearCaseExpr(selector: Tree, cases: List[Tree])(tpe: Type) extends Tree(tpe)
    case LinearCaseClause(pat: Tree, body: Tree)(id: Id, tpe: Type) extends Tree(tpe) with Unique(id)
    case Alternative(bodys: List[Tree])(tpe: Type) extends Tree(tpe)
    case Parens(exprs: List[Tree])(tpe: Type) extends Tree(tpe)
    case Bind(name: Name, body: Tree)(tpe: Type) extends Tree(tpe)
    case Unapply(name: Name, args: List[Tree])(tpe: Type) extends Tree(tpe)
    case Tagged(arg: Name, tpeAs: Tree)(tpe: Type) extends Tree(tpe)
    case TreeSeq(args: List[Tree]) extends Tree(Type.EmptyType)
    case EmptyTree extends Tree(Type.EmptyType)
  }

  object TreeOps {

    implied for Showable[Tree] {
      def (t: Tree) show = toAst(t).toString
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

    implied uniqName for (Tree |> Name) {
      def apply(tree: Tree) = tree match {
        case DefSig(name, _)        => name
        case LinearSig(name, _, _)  => name
        case DefDef(_, sig, _, _)   => apply(sig)
        case Tagged(name, _)        => name
        case Bind(name, _)          => name
        case Ident(name)            => name
        case _                      => EmptyName
      }
    }

    def (tree: Tree) linearArg: Name = tree match {
      case LinearSig(_, _, linearArg) => linearArg
      case LinearFunction(arg,_)      => arg.convert
      case _                          => EmptyName
    }

    def (t: Tree) toNames: List[Name] = {
      @tailrec
      def toNames(acc: List[Name], t: Tree): List[Name] = t match {
        case Ident(name)        => name :: acc
        case Select(tree, name) => toNames(name :: acc, tree)
        case _                  => acc
      }
      toNames(Nil, t)
    }

    def (t: Tree) toNamePairs: List[(Id, Name)] = {
      @tailrec
      def toPairs(acc: List[(Id, Name)], t: Tree): List[(Id, Name)] = t match {
        case t @ Ident(name)        => (t.id, name) :: acc
        case t @ Select(tree, name) => toPairs((t.id, name) :: acc, tree)
        case _                      => acc
      }
      toPairs(Nil, t)
    }

    def (tree: Tree) addModifiers(mods: Set[Modifier]): Tree = tree match {
      case tree: DefDef =>
        tree.copy(modifiers = (tree.modifiers ++ mods))(tree.tpe)

      case _ => tree
    }

    def (tree: Tree) withTpe(tpe: Type): Tree = tree match {
      case tree: Select => tree.copy()(tree.id, tpe)
      case tree: Ident => tree.copy()(tree.id, tpe)
      case tree: PackageDef => tree.copy()(tpe)
      case tree: DefDef => tree.copy()(tpe)
      case tree: DefSig => tree.copy()(tree.id, tpe)
      case tree: LinearSig => tree.copy()(tree.id, tpe)
      case tree: Apply => tree.copy()(tpe)
      case tree: InfixApply => tree.copy()(tpe)
      case tree: Eval => tree.copy()(tpe)
      case tree: Tensor => tree.copy()(tpe)
      case tree: Function => tree.copy()(tree.id, tpe)
      case tree: LinearFunction => tree.copy()(tree.id, tpe)
      case tree: Let => tree.copy()(tree.id, tpe)
      case tree: LetTensor => tree.copy()(tree.id, tpe)
      case tree: Literal => tree.copy()(tpe)
      case tree: CaseExpr => tree.copy()(tpe)
      case tree: CaseClause => tree.copy()(tree.id, tpe)
      case tree: LinearCaseExpr => tree.copy()(tpe)
      case tree: LinearCaseClause => tree.copy()(tree.id, tpe)
      case tree: Alternative => tree.copy()(tpe)
      case tree: Parens => tree.copy()(tpe)
      case tree: Bind => tree.copy()(tpe)
      case tree: Unapply => tree.copy()(tpe)
      case tree: Tagged => tree.copy()(tpe)

      case _ => tree
    }
  }
}