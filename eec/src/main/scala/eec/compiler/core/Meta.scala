package eec
package compiler
package core

import scala.language.implicitConversions

import ast.Trees
import core.{Names, Modifiers, Constants, Contexts}
import Modifiers._
import Contexts._
import types.Types._

import implied TypeOps._

object Meta {
  import Tree._
  import Name._
  import Context._
  import Constant._

  import implied NameOps._

  enum Name derives Eql {
    case From(str: String)
    case BangTag, TensorTag, IntegerTag, DecimalTag, VoidTag, VoidCompTag,
      BooleanTag, StringTag, CharTag
    case Wildcard
    case EmptyName
  }

  enum Constant derives Eql {
    case StringConstant(str: String)
    case CharConstant(chr: Char)
    case IntegerConstant(bi: BigInt)
    case DecimalConstant(bd: BigDecimal)
    case True
    case False
  }

  enum Tree derives Eql {
    case PackageDef(pid: Tree, stats: List[Tree])
    case Select(tree: Tree, name: Name)
    case Ident(name: Name)
    case DataDcl(name: Name, args: List[Name], ctors: List[Tree])
    case CtorSig(name: Name, tpeArgs: List[Tree], lin: Tree)
    case DefDcl(modifiers: List[Modifier], sig: Tree, tpeAs: Tree, body: Tree)
    case DefSig(name: Name, args: List[Name], linear: Name)
    case Apply(id: Tree, args: List[Tree])
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
    case Alternative(bodys: List[Tree])
    case Parens(exprs: List[Tree])
    case Bind(name: Name, body: Tree)
    case Unapply(name: Name, args: List[Tree])
    case Tagged(arg: Name, tpeAs: Tree)
    case EmptyTree
  }

  enum Context derives Eql {
    case LinearVariable(name: Name, tpe: String)
    case Term(name: Name, tpe: String)
    case TermScope(name: Name, tpe: String, scope: Seq[Context])
    case Data(name: Name, tpe: String)
    case AnonScope(scope: Seq[Context])
  }

  object NameOps {
    import implied Names.NameOps._
    implied for Conversion[Names.Name, Name] = {
      case f: (Names.Name.From | Names.Name.Comp) => From(f.show)
      case Names.Name.BangTag                     => BangTag
      case Names.Name.TensorTag                   => TensorTag
      case Names.Name.IntegerTag                  => IntegerTag
      case Names.Name.StringTag                   => StringTag
      case Names.Name.CharTag                     => CharTag
      case Names.Name.BooleanTag                  => BooleanTag
      case Names.Name.DecimalTag                  => DecimalTag
      case Names.Name.VoidCompTag                 => VoidCompTag
      case Names.Name.VoidTag                     => VoidTag
      case Names.Name.Wildcard                    => Wildcard
      case Names.Name.EmptyName                   => EmptyName
    }
  }

  object ContextOps {

    implied for Conversion[Contexts.Context, Seq[Context]] {

      def branch(ctx: Contexts.Context): Seq[Context] = {
        val linearScopeOpt = {
          for
            name <- ctx.linearScope
            tpe  = ctx.termTypeTable.get(name) match {
              case Some(t) => t.define
              case _       => "<notype>"
            }
          yield LinearVariable(name, tpe)
        }
        val data = {
          for
            (name, tpe) <- ctx.dataTypeTable
          yield Data(name, tpe.define)
        }
        val defs = {
          for
            pair <- ctx.termScope.filter { pair =>
                      val (Sym(_, name), child) = pair
                      child.termScope.nonEmpty ||
                      child.linearScope.nonEmpty ||
                      name != Names.Name.EmptyName
                    }
            (sym, child)  = pair
            tpeOpt        = ctx.termTypeTable.get(sym.name)
            tpe           = tpeOpt.fold("<notype>")(t => t.define)
            name          = sym.name
          yield {
            if name != Names.Name.EmptyName then {
              if child.termScope.isEmpty then
                Term(name, tpe)
              else
                TermScope(name, tpe, child)
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
                rootSym.name,
                rootPkg.define,
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
        DataDcl(name, args.map(n => n: Name), ctors.map(toTree))

      case Trees.Tree.InfixDataDcl(name, left, right, ctors) =>
        DataDcl(name, List(left, right), ctors.map(toTree))

      case Trees.Tree.CtorSig(name, tpeArgs) =>
        CtorSig(name, tpeArgs.map(toTree), EmptyTree)

      case Trees.Tree.LinearCtorSig(name, tpeArg) =>
        CtorSig(name, Nil, tpeArg.map(toTree).getOrElse{EmptyTree})

      case Trees.Tree.DefDef(modifiers, sig, tpeAs, body) =>
        DefDcl(modifiers.toList, toTree(sig), toTree(tpeAs), toTree(body))

      case Trees.Tree.DefSig(name, args) =>
        DefSig(name, args.map(n => n: Name), EmptyName)

      case Trees.Tree.LinearSig(name, arg, lin) =>
        DefSig(name, arg.map(n => n: Name), lin)

      case Trees.Tree.Apply(id, args) =>
        Apply(toTree(id), args.map(toTree))

      case Trees.Tree.InfixApply(f, left, right) =>
        Apply(toTree(f), List(toTree(left), toTree(right)))

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

      case Trees.Tree.Literal(constant) =>
        Constants.Constant.asScala(constant) match {
          case c: Boolean     => Literal(if c then True else False)
          case c: Char        => Literal(CharConstant(c))
          case c: String      => Literal(StringConstant(c))
          case c: BigDecimal  => Literal(DecimalConstant(c))
          case c: BigInt      => Literal(IntegerConstant(c))
        }

      case Trees.Tree.CaseExpr(selector, cases) =>
        CaseExpr(toTree(selector), cases.map(toTree))

      case Trees.Tree.CaseClause(pat, guard, body) =>
        CaseClause(toTree(pat), toTree(guard), toTree(body))

      case Trees.Tree.LinearCaseExpr(selector, cases) =>
        LinearCaseExpr(toTree(selector), cases.map(toTree))

      case Trees.Tree.LinearCaseClause(pat, body) =>
        CaseClause(toTree(pat), EmptyTree, toTree(body))

      case Trees.Tree.Alternative(bodys)  => Alternative(bodys.map(toTree))
      case Trees.Tree.Parens(exprs)       => Parens(exprs.map(toTree))
      case Trees.Tree.Bind(name, body)    => Bind(name, toTree(body))
      case Trees.Tree.Unapply(name, args) => Unapply(name, args.map(toTree))
      case Trees.Tree.Tagged(arg, tpeAs)  => Tagged(arg, toTree(tpeAs))
      case Trees.Tree.EmptyTree           => EmptyTree

      case s: Trees.Tree.TreeSeq =>
        throw new AssertionError(
          s"Illegal ${s.productPrefix} in user facing API.")
    }
  }
}