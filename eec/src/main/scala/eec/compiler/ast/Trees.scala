package eec
package compiler
package ast

import core._
import Names._
import Constants._
import Contexts._
import Modifiers._
import types.Types._
import annotation._
import util.{Showable, |>, Convert}

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
    case PrimSig(name: Name, args: List[Name], linear: Name)(id: Id, tpe: Type) extends Tree(tpe) with Unique(id)
    case Apply(f: Tree, args: List[Tree])(tpe: Type) extends Tree(tpe)
    case InfixApplyType(f: Tree, left: Tree, right: Tree)(tpe: Type) extends Tree(tpe)
    case Eval(f: Tree, arg: Tree)(tpe: Type) extends Tree(tpe)
    case Tensor(value: Tree, computation: Tree)(tpe: Type) extends Tree(tpe)
    case Function(args: List[Tree], body: Tree)(id: Id, tpe: Type) extends Tree(tpe) with Unique(id)
    case LinearFunction(arg: Tree, body: Tree)(id: Id, tpe: Type) extends Tree(tpe) with Unique(id)
    case Let(x: Name, value: Tree, continuation: Tree)(id: Id, tpe: Type) extends Tree(tpe) with Unique(id)
    case LetTensor(x: Name, z: Name, value: Tree, continuation: Tree)(id: Id, tpe: Type) extends Tree(tpe) with Unique(id)
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
        case DefSig(name, _)      => name
        case DefDef(_, sig, _, _) => apply(sig)
        case Tagged(name, _)      => name
        case Bind(name, _)        => name
        case Ident(name)          => name
        case _                    => emptyString.readAs
      }
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
  }
}