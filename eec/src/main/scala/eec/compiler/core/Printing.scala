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
      case DefSig(name: Name, args: List[Name])
      case Apply(id: Ast, args: List[Ast])
      case Function(args: List[Ast], body: Ast)
      case Let(name: Name, value: Ast, continuation: Ast)
      case If(cond: Ast, thenp: Ast, elsep: Ast)
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
        case Tree.Select(_, tree, name) => Select(tree.toAst, name)
        case Tree.Ident(_, name) => Ident(name)
        case Tree.PackageDef(_, pid, stats) => PackageDef(pid.toAst, stats.map(_.toAst))
        case Tree.DefDef(_, modifiers, sig, tpeAs, body) => DefDef(modifiers, sig.toAst, tpeAs.toAst, body.toAst)
        case Tree.DefSig(_, name, args) => DefSig(name, args)
        case Tree.Apply(_, id, args) => Apply(id.toAst, args.map(_.toAst))
        case Tree.Function(_, args, body) => Function(args.map(_.toAst), body.toAst)
        case Tree.Let(_, name, value, continuation) => Let(name, value.toAst, continuation.toAst)
        case Tree.If(_, cond, thenp, elsep) => If(cond.toAst, thenp.toAst, elsep.toAst)
        case Tree.Literal(_, constant) => Literal(constant)
        case Tree.CaseExpr(_, selector, cases) => CaseExpr(selector.toAst, cases.map(_.toAst))
        case Tree.CaseClause(_, pat, guard, body) => CaseClause(pat.toAst, guard.toAst, body.toAst)
        case Tree.Alternative(_, bodys) => Alternative(bodys.map(_.toAst))
        case Tree.Parens(_, exprs) => Parens(exprs.map(_.toAst))
        case Tree.Bind(_, name, body) => Bind(name, body.toAst)
        case Tree.Unapply(_, id, args) => Unapply(id.toAst, args.map(_.toAst))
        case Tree.Tagged(_, arg, tpeAs) => Tagged(arg, tpeAs.toAst)
        case Tree.TreeSeq(args) => TreeSeq(args.map(_.toAst))
        case Tree.EmptyTree => EmptyAst
      }
    }

  }
}