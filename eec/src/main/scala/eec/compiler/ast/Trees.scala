package eec
package compiler
package ast

import scala.language.implicitConversions

import core._
import Names._
import Name._
import Constants._
import Constant._
import Contexts.Id
import Modifiers._
import types.Types._
import untyped.nt
import annotation._
import util.{Show, eval, foldMap}

import delegate Meta.TreeOps._
import delegate Names.NameOps._
import delegate TypeOps._

object Trees {
  import Tree._

  trait Unique(val id: Id) { self: Tree => }

  enum Tree(val tpe: Type) derives Eql {
    case Select(tree: Tree, name: Name)(id: Id, tpe: Type) extends Tree(tpe) with Unique(id)
    case Ident(name: Name)(id: Id, tpe: Type) extends Tree(tpe) with Unique(id)
    case PackageDef(pid: Tree, stats: List[Tree])(tpe: Type) extends Tree(tpe)
    case DataDcl(name: Name, args: List[Name], ctors: List[Tree])(tpe: Type) extends Tree(tpe)
    case InfixDataDcl(name: Name, left: Name, right: Name, ctors: List[Tree])(tpe: Type) extends Tree(tpe)
    case CtorSig(name: Name, tpeArgs: List[Tree])(tpe: Type) extends Tree(tpe)
    case LinearCtorSig(name: Name, tpeArg: Option[Tree])(tpe: Type) extends Tree(tpe)
    case DefDef(modifiers: Set[Modifier], sig: Tree, tpeAs: Tree, body: Tree)(tpe: Type) extends Tree(tpe)
    case DefSig(name: Name, args: List[Name])(id: Id, tpe: Type) extends Tree(tpe) with Unique(id)
    case LinearSig(name: Name, args: List[Name], linear: Name)(id: Id, tpe: Type) extends Tree(tpe) with Unique(id)
    case Apply(f: Tree, args: List[Tree])(tpe: Type) extends Tree(tpe)
    case InfixApply(f: Tree, left: Tree, right: Tree)(tpe: Type) extends Tree(tpe)
    case Eval(f: Tree, arg: Tree)(tpe: Type) extends Tree(tpe)
    case WhyNot(value: Tree)(tpe: Type) extends Tree(tpe)
    case Bang(value: Tree)(tpe: Type) extends Tree(tpe)
    case Tensor(value: Tree, computation: Tree)(tpe: Type) extends Tree(tpe)
    case Function(args: List[Tree], body: Tree)(id: Id, tpe: Type) extends Tree(tpe) with Unique(id)
    case LinearFunction(arg: Tree, body: Tree)(id: Id, tpe: Type) extends Tree(tpe) with Unique(id)
    case Let(patt: Tree, value: Tree, continuation: Tree)(id: Id, tpe: Type) extends Tree(tpe) with Unique(id)
    case LetTensor(x: Tree, z: Tree, s: Tree, continuation: Tree)(id: Id, tpe: Type) extends Tree(tpe) with Unique(id)
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
    case TreeSeq(args: List[Tree]) extends Tree(nt)
    case EmptyTree extends Tree(nt)
  }

  object TreeOps {

    def showPatternTemplate(tree: Tree): String = {
      type StackT = List[String]
      type StatT = StackT => StackT
      type ProgT = List[StatT]

      @tailrec
      def inner(program: ProgT, patts: List[Tree]): ProgT = patts match {
        case Nil => program

        case patt::patts => patt match {

          case Unapply(name, args) =>
            val statement = { stack: StackT =>
              val (args1, rest) = stack.splitAt(args.length)
              val args2 = args1.view.zip(args).map { (str, arg) =>
                arg match {
                  case (_:(Ident | Parens | Literal) | Unapply(_,Nil)) => str
                  case _ => s"($str)"
                }
              }
              val argsStr = args2.foldMap("")(_.mkString(" ", " ", ""))
              s"${name.show}$argsStr"::rest
            }
            inner(statement::program, args:::patts)

          case Parens(args) =>
            val statement = { stack: StackT =>
              val (args1, rest) = stack.splitAt(args.length)
              val argsStr = args1.mkString("(", ", ", ")")
              argsStr::rest
            }
            inner(statement::program, args:::patts)

          case Ident(name) => inner((name.show::_)::program, patts)

          case Literal(b @ (True | False)) =>
            val str = if b == True then "True" else "False"
            inner((str::_)::program, patts)

          case Alternative(patts1) =>
            val statement = { stack: StackT =>
              val (args1, stack1) = stack.splitAt(patts1.length)
              val argsStr = args1.mkString(" | ")
              argsStr::stack1
            }
            inner(statement::program, patts1:::patts)

          case Bind(x,patt) =>
            val statement = { stack: StackT =>
              val patt1::stack1 = stack
              val pattStr = patt match {
                case (_:(Ident | Parens | Literal) | Unapply(_,Nil)) => patt1
                case _ => s"($patt1)"
              }
              s"${x.show} @ $pattStr"::stack1
            }
            inner(statement::program, patt::patts)

          case _ => inner(("<???>"::_)::program, patts)
        }
      }

      inner(Nil, tree :: Nil)
        .foldLeft(List.empty[String])(eval)
        .head
    }

    @tailrec
    def (tree: Tree) uniqId: Id = tree match {
      case Select(t, _)     => t.uniqId
      case PackageDef(t, _) => t.uniqId
      case DefDef(_,s,_,_)  => s.uniqId
      case u: Unique        => u.id
      case _                => Id.empty
    }

    delegate for Show[Tree] = t => pprint.apply(
      x = (t: Meta.Tree),
      width = 80,
      height = Int.MaxValue
    ).render

    delegate for Conversion[Tree, List[Tree]] = {
      case EmptyTree          => Nil
      case TreeSeq(args)      => args
      case Parens(args)       => args
      case Alternative(args)  => args
      case t                  => t :: Nil
    }

    delegate for Conversion[List[Tree], Tree] = {
      case Nil      => EmptyTree
      case t :: Nil => t
      case ts       => TreeSeq(ts)
    }

    delegate uniqName for Conversion[Tree, Name] = {
      case DefSig(name, _)              => name
      case LinearSig(name, _, _)        => name
      case DefDef(_, sig, _, _)         => sig
      case Tagged(name, _)              => name
      case Bind(name, _)                => name
      case Ident(name)                  => name
      case DataDcl(name, _, _)          => name
      case InfixDataDcl(name, _, _, _)  => name
      case CtorSig(name, _)             => name
      case LinearCtorSig(name, _)       => name
      case _                            => EmptyName
    }

    def (tree: Tree) linearArg: Name = tree match {
      case LinearSig(_, _, linearArg) => linearArg
      case LinearFunction(arg,_)      => arg
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
      case tree: Select           => tree.copy()(tree.id, tpe)
      case tree: Ident            => tree.copy()(tree.id, tpe)
      case tree: PackageDef       => tree.copy()(tpe)
      case tree: DataDcl          => tree.copy()(tpe)
      case tree: InfixDataDcl     => tree.copy()(tpe)
      case tree: CtorSig          => tree.copy()(tpe)
      case tree: LinearCtorSig    => tree.copy()(tpe)
      case tree: DefDef           => tree.copy()(tpe)
      case tree: DefSig           => tree.copy()(tree.id, tpe)
      case tree: LinearSig        => tree.copy()(tree.id, tpe)
      case tree: Apply            => tree.copy()(tpe)
      case tree: InfixApply       => tree.copy()(tpe)
      case tree: Eval             => tree.copy()(tpe)
      case tree: WhyNot           => tree.copy()(tpe)
      case tree: Bang             => tree.copy()(tpe)
      case tree: Tensor           => tree.copy()(tpe)
      case tree: Function         => tree.copy()(tree.id, tpe)
      case tree: LinearFunction   => tree.copy()(tree.id, tpe)
      case tree: Let              => tree.copy()(tree.id, tpe)
      case tree: LetTensor        => tree.copy()(tree.id, tpe)
      case tree: Literal          => tree.copy()(tpe)
      case tree: CaseExpr         => tree.copy()(tpe)
      case tree: CaseClause       => tree.copy()(tree.id, tpe)
      case tree: LinearCaseExpr   => tree.copy()(tpe)
      case tree: LinearCaseClause => tree.copy()(tree.id, tpe)
      case tree: Alternative      => tree.copy()(tpe)
      case tree: Parens           => tree.copy()(tpe)
      case tree: Bind             => tree.copy()(tpe)
      case tree: Unapply          => tree.copy()(tpe)
      case tree: Tagged           => tree.copy()(tpe)
      case _:TreeSeq | EmptyTree  => tree
    }
  }
}