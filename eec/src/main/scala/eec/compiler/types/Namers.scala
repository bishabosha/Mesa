package eec
package compiler
package types

import ast.Trees._
import Tree._
import TreeOps._
import error.CompilerErrors._
import CompilerErrorOps._
import core.Contexts._
import core.Names._
import core.Modifiers._
import Modifier.Primitive
import Name._
import NameOps._
import Context._
import Mode._
import util.Convert
import Convert._

import implied NameOps._
import implied TreeOps._

object Namers {

  private val anon = EmptyName

  def namedDataDcl(name: Name, ctors: List[Tree])
                  given Context, Mode: Checked[Unit] = {
    for {
      _ <- enterVariable(name)
      _ <- ctors.foldLeftE(())((_, ctor) => index(ctor))
    } yield ()
  }

  def namedCtorSig(name: Name)
                  given Context, Mode: Checked[Unit] = {
    for {
      _ <- enterVariable(name)
    } yield ()
  }

  def namedDefDef(modifiers: Set[Modifier], sig: DefSig | LinearSig)
                 (tpeAs: Tree, body: Tree)
                 given Context, Mode: Checked[Unit] = {
    val (name, args) = sig match {
      case LinearSig(name, args, _) => (name, args)
      case DefSig(name, args)     => (name, args)
    }
    enterScope(sig.id, name).flatMap { ctx1 =>
      implied for Context = ctx1
      for {
        _ <- args.foldLeftE(())((_, n) => enterVariable(n))
        _ <- sig.linearArg.foldEmptyName(())(enterLinearArg(modifiers))
        _ <- index(body)
      } yield ()
    }
  }

  def enterLinearArg(mods: Set[Modifier])(linearArg: Name) given Context: Checked[Unit] = {
    val enterArg = {
      val isPrimitive = mods.contains(Primitive)
      val isWildcard  = linearArg == Wildcard
      (isPrimitive && !isWildcard) || !isPrimitive
    }
    if enterArg then
      enterLinear(linearArg)
    else
      ()
  }

  def namedFunctionTerm(args: List[Tree], body: Tree)
                       (id: Id)
                       given Context, Mode: Checked[Unit] = {
    enterScope(id, anon).flatMap { ctx1 =>
      implied for Context = ctx1
      for {
        _ <-  args.foldLeftE(())((_, n) => enterVariable(n.convert))
        _ <-  index(body)
      } yield ()
    }
  }

  def namedLinearFunctionTerm(arg: Tree, body: Tree)
                             (id: Id)
                             given Context, Mode: Checked[Unit] = {
    enterScope(id, anon).flatMap { ctx1 =>
      implied for Context = ctx1
      for {
        _ <-  enterLinear(arg.convert)
        _ <-  index(body)
      } yield ()
    }
  }

  def namedPackageDef(pid: Tree, stats: List[Tree])
                     given Context, Mode: Checked[Unit] = {
    val cPkgCtx = pid.toNamePairs.foldLeftE(ctx) { (pkgCtx, pair) =>
      val (id, pkgName)   = pair
      implied for Context = pkgCtx
      enterScope(id, pkgName)
    }
    cPkgCtx.flatMap { pkgCtx =>
      implied for Context = pkgCtx
      stats.foldLeftE(())((_, stat) => index(stat))
    }
  }

  def namedApplyTerm(fun: Tree, args: List[Tree])
                    given Context, Mode: Checked[Unit] = {
    for {
      _ <- index(fun)
      _ <- args.foldLeftE(())((_, arg) => index(arg))
    } yield ()
  }

  def namedEvalTerm(fun: Tree, arg: Tree)
                   given Context, Mode: Checked[Unit] = {
    for {
      _ <- index(fun)
      _ <- index(arg)
    } yield ()
  }

  def namedLet(letName: Name, value: Tree, continuation: Tree)
              (id: Id)
              given Context, Mode: Checked[Unit] = {
    for {
      _ <-  index(value)
      _ <-  enterScope(id, anon).flatMap { ctx1 =>
              implied for Context = ctx1
              for {
                _ <- enterVariable(letName)
                _ <- index(continuation)
              } yield ()
            }
    } yield ()
  }

  def namedLetTensor(x: Name, z: Name, s: Tree, t: Tree)
                    (id: Id)
                    given Context, Mode: Checked[Unit] = {
    for {
      _ <-  index(s)
      _ <-  enterScope(id, anon).flatMap { ctx1 =>
              implied for Context = ctx1
              for {
                _ <- enterVariable(x)
                _ <- enterLinear(z)
                _ <- index(t)
              } yield ()
            }
    } yield ()
  }

  def namedCaseExpr(selector: Tree, cases: List[Tree])
                   given Context, Mode: Checked[Unit] = {
    for {
      _ <-  index(selector)
      _ <-  cases.foldLeftE(()) { (_, tree) =>
              tree match {
                case tree: (CaseClause | LinearCaseClause) =>
                  enterScope(tree.id, anon).flatMap { ctx1 =>
                    implied for Context = ctx1
                    index(tree)
                  }
                case _ =>
              }
            }
    } yield ()
  }

  def namedCaseClause(pat: Tree, guard: Tree, body: Tree)
                     given Context, Mode: Checked[Unit] = {
    for {
      _ <- indexAsPattern(pat)
      _ <- index(guard) // idents here are normal refs to variables in this scope
      _ <- index(body)
    } yield ()
  }

  def namedLinearCaseClause(pat: Tree, body: Tree)
                           given Context, Mode: Checked[Unit] = {
    for {
      _ <- indexAsLinearPattern(pat)
      _ <- index(body)
    } yield ()
  }

  def namedAlternative(alts: List[Tree]) given Context, Mode: Checked[Unit] = {
    implied for Mode = Mode.PatAlt
    alts.foldLeftE(())((_, t) => index(t))
  }

  def namedUnapply(args: List[Tree]) given Context, Mode: Checked[Unit] = {
    args.foldLeftE(())((_, t) => index(t))
  }

  def namedBind(name: Name, pat: Tree)
               given Context, Mode: Checked[Unit] = {
    for {
      _ <- enterVariable(name)
      _ <- index(pat)
    } yield ()
  }

  def namedParens(args: List[Tree]) given Context, Mode: Checked[Unit] =
    args.foldLeftE(())((_, t) => index(t))

  def namedIdentPat(name: Name) given Context, Mode: Checked[Unit] =
    enterVariable(name)

  def namedIdentLinearPat(name: Name) given Context, Mode: Checked[Unit] =
    name.foldWildcard(())(enterLinear)

  def indexAsPattern(tree: Tree) given Context: Checked[Unit] = {
    implied for Mode = Mode.Pat
    index(tree)
  }

  def indexAsLinearPattern(tree: Tree) given Context: Checked[Unit] = {
    implied for Mode = Mode.LinearPat
    index(tree)
  }

  def indexAsExpr(tree: Tree) given Context: Checked[Unit] = {
    implied for Mode = Mode.Term
    index(tree)
  }

  def index(tree: Tree) given Context, Mode: Checked[Unit] = tree match {
    /* Linear Pattern Trees */
    case Ident(n)                 if isLPattern => namedIdentLinearPat(n)
    case Unapply(_,ts)            if isLPattern => namedUnapply(ts)
    /* Pattern Trees */
    case Ident(n)                 if isPattern  => namedIdentPat(n)
    case Bind(n,t)                if isPattern  => namedBind(n,t)
    case Alternative(ts)          if isPattern  => namedAlternative(ts)
    case Unapply(_,ts)            if isPattern  => namedUnapply(ts)
    /* Term Trees */
    case PackageDef(t,ts)         if isTerm     => namedPackageDef(t,ts)
    case Apply(t,ts)              if isTerm     => namedApplyTerm(t,ts)
    case Eval(t,c)                if isTerm     => namedEvalTerm(t,c)
    case DataDcl(n,_,c)           if isTerm     => namedDataDcl(n,c)
    case InfixDataDcl(n,_,_,c)    if isTerm     => namedDataDcl(n,c)
    case CtorSig(n,_)             if isTerm     => namedCtorSig(n)
    case LinearCtorSig(n,_)       if isTerm     => namedCtorSig(n)
    case DefDef(                  // DefDef
      m,                          // DefDef
      s: (DefSig | LinearSig),    // DefDef
      t,                          // DefDef
      b)                          if isTerm     => namedDefDef(m,s)(t,b)
    case u @ Let(n,v,c)           if isTerm     => namedLet(n,v,c)(u.id)
    case u @ LetTensor(x,z,s,t)   if isTerm     => namedLetTensor(x,z,s,t)(u.id)
    case u @ Function(ts,t)       if isTerm     => namedFunctionTerm(ts,t)(u.id)
    case u @ LinearFunction(a,b)  if isTerm     => namedLinearFunctionTerm(a,b)(u.id)
    case LinearCaseExpr(t,ts)     if isTerm     => namedCaseExpr(t,ts)
    case CaseExpr(t,ts)           if isTerm     => namedCaseExpr(t,ts)
    case CaseClause(p,g,b)        if isTerm     => namedCaseClause(p,g,b)
    case LinearCaseClause(p,b)    if isTerm     => namedLinearCaseClause(p,b)
    /* any mode */
    case Parens(ts)                             => namedParens(ts)
    case _: Literal
       | _: Ident
       | _: Select
       | _: Tensor
       | _: Bang
       | _: WhyNot
       | EmptyTree                              => // atomic
    /* error case */
    case _                                      => NamerErrors.namingMissing(tree)
  }
}