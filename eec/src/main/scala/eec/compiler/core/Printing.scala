package eec
package compiler
package core

object Printing {

  import compiler.ast.Trees._
  import compiler.core.Names._
  import compiler.core.Modifiers._
  import compiler.core.Constants._

  object untyped {

    enum Ast {
      case Select(tree: Ast, name: Name)
      case Ident(name: Name)
      case PackageDef(pid: Ast, stats: List[Ast])
      case DefDef(modifiers: Set[Modifier], sig: Ast, tpeAs: Ast, body: Ast)
      case DefSig(name: Name, args: List[Tree])
      case Apply(id: Ast, args: List[Ast])
      case Function(args: List[Ast], body: Ast)
      case Let(name: Tree, value: Ast, continuation: Ast)
      case Literal(constant: Constant)
      case CaseExpr(selector: Ast, cases: List[Ast])
      case CaseClause(pat: Ast, guard: Ast, body: Ast)
      case Alternative(bodys: List[Ast])
      case Parens(exprs: List[Ast])
      case Bind(name: Name, body: Ast)
      case Unapply(id: Ast, args: List[Ast])
      case Tagged(arg: Name, tpeAs: Ast)
      case TreeSeq(args: List[Ast])
      case EmptyAst
    }

    object AstOps {
      import Ast._
      def (tree: Tree) toAst: Ast = tree match {
        case Tree.Select(tree, name) => Select(tree.toAst, name)
        case Tree.Ident(name) => Ident(name)
        case Tree.PackageDef(pid, stats) => PackageDef(pid.toAst, stats.map(_.toAst))
        case Tree.DefDef(modifiers, sig, tpeAs, body) => DefDef(modifiers, sig.toAst, tpeAs.toAst, body.toAst)
        case Tree.DefSig(name, args) => DefSig(name, args)
        case Tree.Apply(id, args) => Apply(id.toAst, args.map(_.toAst))
        case Tree.Function(args, body) => Function(args.map(_.toAst), body.toAst)
        case Tree.Let(name, value, continuation) => Let(name, value.toAst, continuation.toAst)
        case Tree.Literal(constant) => Literal(constant)
        case Tree.CaseExpr(selector, cases) => CaseExpr(selector.toAst, cases.map(_.toAst))
        case Tree.CaseClause(pat, guard, body) => CaseClause(pat.toAst, guard.toAst, body.toAst)
        case Tree.Alternative(bodys) => Alternative(bodys.map(_.toAst))
        case Tree.Parens(exprs) => Parens(exprs.map(_.toAst))
        case Tree.Bind(name, body) => Bind(name, body.toAst)
        case Tree.Unapply(id, args) => Unapply(id.toAst, args.map(_.toAst))
        case Tree.Tagged(arg, tpeAs) => Tagged(arg, tpeAs.toAst)
        case Tree.TreeSeq(args) => TreeSeq(args.map(_.toAst))
        case Tree.EmptyTree => EmptyAst
      }
    }

  }
}