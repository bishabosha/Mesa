package eec
package compiler
package core

import ast.Trees._
import core.{Names, Modifiers, Constants}
import Names._
import Modifiers._
import Constants._
import util.|>

object Printing {

  object untyped {
    import Ast._

    enum Ast derives Eql {
      case Select(tree: Ast, name: Name)
      case Ident(name: Name)
      case PackageDef(pid: Ast, stats: List[Ast])
      case DefDef(modifiers: Set[Modifier], sig: Ast, tpeAs: Ast, body: Ast)
      case DefSig(name: Name, args: List[Name])
      case LinearSig(name: Name, args: List[Name], linear: Name)
      case Apply(id: Ast, args: List[Ast])
      case InfixApply(f: Ast, left: Ast, right: Ast)
      case Eval(f: Ast, arg: Ast)
      case Bang(value: Ast)
      case WhyNot(value: Ast)
      case Tensor(value: Ast, computation: Ast)
      case Function(args: List[Ast], body: Ast)
      case LinearFunction(arg: Ast, body: Ast)
      case Let(x: Name, value: Ast, continuation: Ast)
      case LetTensor(x: Name, z: Name, s: Ast, t: Ast)
      case Literal(constant: Constant)
      case CaseExpr(selector: Ast, cases: List[Ast])
      case CaseClause(pat: Ast, guard: Ast, body: Ast)
      case LinearCaseExpr(selector: Ast, cases: List[Ast])
      case LinearCaseClause(pat: Ast, body: Ast)
      case Alternative(bodys: List[Ast])
      case Parens(exprs: List[Ast])
      case Bind(name: Name, body: Ast)
      case Unapply(name: Name, args: List[Ast])
      case Tagged(arg: Name, tpeAs: Ast)
      case TreeSeq(args: List[Ast])
      case EmptyAst
    }

    object AstOps {
      implied toAst for (Tree |> Ast) = {
        case Tree.Select(tree, name)  => Select(toAst(tree), name)
        case Tree.Ident(name)         => Ident(name)

        case Tree.PackageDef(pid, stats) =>
          PackageDef(toAst(pid), stats.map(toAst))

        case Tree.DefDef(modifiers, sig, tpeAs, body) =>
          DefDef(modifiers, toAst(sig), toAst(tpeAs), toAst(body))

        case Tree.DefSig(name, args)        => DefSig(name, args)
        case Tree.LinearSig(name, arg, lin) => LinearSig(name, arg, lin)
        case Tree.Apply(id, args)           => Apply(toAst(id), args.map(toAst))

        case Tree.InfixApply(f, left, right) =>
          InfixApply(toAst(f), toAst(left), toAst(right))

        case Tree.Eval(f, arg)  => Eval(toAst(f), toAst(arg))
        case Tree.WhyNot(value) => WhyNot(toAst(value))
        case Tree.Bang(value)   => Bang(toAst(value))

        case Tree.Tensor(value, computation) =>
          Tensor(toAst(value), toAst(computation))

        case Tree.Function(args, body) =>
          Function(args.map(toAst), toAst(body))

        case Tree.LinearFunction(arg, body) =>
          LinearFunction(toAst(arg), toAst(body))

        case Tree.Let(name, value, continuation) =>
          Let(name, toAst(value), toAst(continuation))

        case Tree.LetTensor(x, z, s, t) =>
          LetTensor(x, z, toAst(s), toAst(t))

        case Tree.Literal(constant) => Literal(constant)

        case Tree.CaseExpr(selector, cases) =>
          CaseExpr(toAst(selector), cases.map(toAst))

        case Tree.CaseClause(pat, guard, body) =>
          CaseClause(toAst(pat), toAst(guard), toAst(body))

        case Tree.LinearCaseExpr(selector, cases) =>
          LinearCaseExpr(toAst(selector), cases.map(toAst))

        case Tree.LinearCaseClause(pat, body) =>
          LinearCaseClause(toAst(pat), toAst(body))

        case Tree.Alternative(bodys)  => Alternative(bodys.map(toAst))
        case Tree.Parens(exprs)       => Parens(exprs.map(toAst))
        case Tree.Bind(name, body)    => Bind(name, toAst(body))
        case Tree.Unapply(name, args) => Unapply(name, args.map(toAst))
        case Tree.Tagged(arg, tpeAs)  => Tagged(arg, toAst(tpeAs))
        case Tree.TreeSeq(args)       => TreeSeq(args.map(toAst))
        case Tree.EmptyTree           => EmptyAst
      }
    }
  }
}