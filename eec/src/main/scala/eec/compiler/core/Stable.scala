package eec
package compiler
package core

import scala.language.implicitConversions

import ast.Trees
import core.{Names, Modifiers, Constants, Contexts}
import Names._
import NameOps._
import Modifiers._
import Constants._
import Contexts._
import types.Types._

import implied TypeOps._
import implied NameOps._

object Stable {
  import Tree._
  import Context._

  enum Tree derives Eql {
    case Select(tree: Tree, name: Name)
    case Ident(name: Name)
    case PackageDef(pid: Tree, stats: List[Tree])
    case DataDcl(name: Name, args: List[Name], ctors: List[Tree])
    case InfixDataDcl(name: Name, left: Name, right: Name, ctors: List[Tree])
    case CtorSig(name: Name, tpeArgs: List[Tree])
    case LinearCtorSig(name: Name, tpeArg: Option[Tree])
    case DefDef(modifiers: Set[Modifier], sig: Tree, tpeAs: Tree, body: Tree)
    case DefSig(name: Name, args: List[Name])
    case LinearSig(name: Name, args: List[Name], linear: Name)
    case Apply(id: Tree, args: List[Tree])
    case InfixApply(f: Tree, left: Tree, right: Tree)
    case Eval(f: Tree, arg: Tree)
    case Bang(value: Tree)
    case WhyNot(value: Tree)
    case Tensor(value: Tree, computation: Tree)
    case Function(args: List[Tree], body: Tree)
    case LinearFunction(arg: Tree, body: Tree)
    case Let(patt: Tree, value: Tree, continuation: Tree)
    case LetTensor(x: Tree, z: Tree, s: Tree, t: Tree)
    case Literal(constant: Constant)
    case CaseExpr(selector: Tree, cases: List[Tree])
    case CaseClause(pat: Tree, guard: Tree, body: Tree)
    case LinearCaseExpr(selector: Tree, cases: List[Tree])
    case LinearCaseClause(pat: Tree, body: Tree)
    case Alternative(bodys: List[Tree])
    case Parens(exprs: List[Tree])
    case Bind(name: Name, body: Tree)
    case Unapply(name: Name, args: List[Tree])
    case Tagged(arg: Name, tpeAs: Tree)
    case TreeSeq(args: List[Tree])
    case EmptyTree
  }

  enum Context derives Eql {
    case LinearVariable(name: String, tpe: String)
    case Term(name: String, tpe: String)
    case TermScope(name: String, tpe: String, scope: Seq[Context])
    case Data(name: String, tpe: String)
    case AnonScope(scope: Seq[Context])
  }

  object ContextOps {

    implied for Conversion[Contexts.Context, Seq[Context]] {

      def branch(ctx: Contexts.Context): Seq[Context] = {
        val linearScopeOpt = {
          for
            name <- ctx.linearScope
            tpe  = ctx.termTypeTable.get(name) match {
              case Some(t) => t.show
              case _       => "<notype>"
            }
          yield LinearVariable(name.show, tpe)
        }
        val data = {
          for
            (name, tpe) <- ctx.dataTypeTable
          yield Data(name.show, tpe.show)
        }
        val defs = {
          for
            pair <- ctx.termScope.filter { pair =>
                      val (Sym(_, name), child) = pair
                      child.termScope.nonEmpty ||
                      child.linearScope.nonEmpty ||
                      name.nonEmpty
                    }
            (sym, child)  = pair
            tpeOpt        = ctx.termTypeTable.get(sym.name)
            tpe           = tpeOpt.fold("<notype>")(t => t.show)
            name          = sym.name
          yield {
            if name.nonEmpty then {
              if child.termScope.isEmpty then
                Term(name.show, tpe)
              else
                TermScope(name.show, tpe, child)
            } else {
              AnonScope(child)
            }
          }
        }
        linearScopeOpt.toList ++ data.toList ++ defs
      }

      def (ctx: Fresh) hasMembers = {
        ctx.termScope.nonEmpty ||
        ctx.termTypeTable.nonEmpty ||
        ctx.linearScope.isDefined
      }

      def apply(ctx: Contexts.Context): Seq[Context] = {
        ctx match {
          case ctx: RootContext =>
            List(
              TermScope(
                rootSym.name.show,
                rootPkg.show,
                branch(ctx)
              )
            )

          case ctx: Fresh if ctx.hasMembers => branch(ctx)
          case _                            => Nil
        }
      }
    }
  }

  object TreeOps {
    implied toTree for Conversion[Trees.Tree, Tree] = {
      case Trees.Tree.Select(tree, name)  => Select(toTree(tree), name)
      case Trees.Tree.Ident(name)         => Ident(name)

      case Trees.Tree.PackageDef(pid, stats) =>
        PackageDef(toTree(pid), stats.map(toTree))

      case Trees.Tree.DataDcl(name, args, ctors) =>
        DataDcl(name, args, ctors.map(toTree))

      case Trees.Tree.InfixDataDcl(name, left, right, ctors) =>
        InfixDataDcl(name, left, right, ctors.map(toTree))

      case Trees.Tree.CtorSig(name, tpeArgs) =>
        CtorSig(name, tpeArgs.map(toTree))

      case Trees.Tree.LinearCtorSig(name, tpeArg) =>
        LinearCtorSig(name, tpeArg.map(toTree))

      case Trees.Tree.DefDef(modifiers, sig, tpeAs, body) =>
        DefDef(modifiers, toTree(sig), toTree(tpeAs), toTree(body))

      case Trees.Tree.DefSig(name, args)        => DefSig(name, args)
      case Trees.Tree.LinearSig(name, arg, lin) => LinearSig(name, arg, lin)

      case Trees.Tree.Apply(id, args) =>
        Apply(toTree(id), args.map(toTree))

      case Trees.Tree.InfixApply(f, left, right) =>
        InfixApply(toTree(f), toTree(left), toTree(right))

      case Trees.Tree.Eval(f, arg)  => Eval(toTree(f), toTree(arg))
      case Trees.Tree.WhyNot(value) => WhyNot(toTree(value))
      case Trees.Tree.Bang(value)   => Bang(toTree(value))

      case Trees.Tree.Tensor(value, computation) =>
        Tensor(toTree(value), toTree(computation))

      case Trees.Tree.Function(args, body) =>
        Function(args.map(toTree), toTree(body))

      case Trees.Tree.LinearFunction(arg, body) =>
        LinearFunction(toTree(arg), toTree(body))

      case Trees.Tree.Let(patt, value, continuation) =>
        Let(toTree(patt), toTree(value), toTree(continuation))

      case Trees.Tree.LetTensor(x, z, s, t) =>
        LetTensor(toTree(x), toTree(z), toTree(s), toTree(t))

      case Trees.Tree.Literal(constant) => Literal(constant)

      case Trees.Tree.CaseExpr(selector, cases) =>
        CaseExpr(toTree(selector), cases.map(toTree))

      case Trees.Tree.CaseClause(pat, guard, body) =>
        CaseClause(toTree(pat), toTree(guard), toTree(body))

      case Trees.Tree.LinearCaseExpr(selector, cases) =>
        LinearCaseExpr(toTree(selector), cases.map(toTree))

      case Trees.Tree.LinearCaseClause(pat, body) =>
        LinearCaseClause(toTree(pat), toTree(body))

      case Trees.Tree.Alternative(bodys)  => Alternative(bodys.map(toTree))
      case Trees.Tree.Parens(exprs)       => Parens(exprs.map(toTree))
      case Trees.Tree.Bind(name, body)    => Bind(name, toTree(body))
      case Trees.Tree.Unapply(name, args) => Unapply(name, args.map(toTree))
      case Trees.Tree.Tagged(arg, tpeAs)  => Tagged(arg, toTree(tpeAs))
      case Trees.Tree.TreeSeq(args)       => TreeSeq(args.map(toTree))
      case Trees.Tree.EmptyTree           => EmptyTree
    }
  }
}