package eec
package compiler
package types

import scala.language.implicitConversions

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
import types.{NamerErrors => Err}

import implied NameOps._
import implied TreeOps._

object Namers {

  private val anon = EmptyName

  def indexed(tree: Tree) given Context: Lifted[Unit] = {
    implied for Mode = Mode.Term
    index(tree)
  }

  private def indexAsPattern(tree: Tree) given Context: Lifted[Unit] = {
    implied for Mode = Mode.Pat
    index(tree)
  }

  private def indexAsLinearPattern(tree: Tree) given Context: Lifted[Unit] = {
    implied for Mode = Mode.LinearPat
    index(tree)
  }

  private def namedDataDcl(name: Name, ctors: List[Tree])
                          given Context, Mode: Lifted[Unit] = {
    for
      _ <- enterData(name)
      _ <- ctors.foldLeftE(())((_, ctor) => index(ctor))
    yield ()
  }

  private def namedCtorSig(name: Name)
                          given Context, Mode: Lifted[Unit] = {
    for _ <- enterVariable(name)
    yield ()
  }

  private def namedDefDef(modifiers: Set[Modifier], sig: DefSig | LinearSig)
                         (tpeAs: Tree, body: Tree)
                         given Context, Mode: Lifted[Unit] = {
    val (name, args) = sig match {
      case LinearSig(name, args, _) => (name, args)
      case DefSig(name, args)       => (name, args)
    }
    enterScope(sig.id, name).flatMap { ctx1 =>
      implied for Context = ctx1
      for
        _ <- args.foldLeftE(())((_, n) => enterVariable(n))
        _ <- sig.linearArg.foldEmptyName(())(enterLinearArg(modifiers))
        _ <- index(body)
      yield ()
    }
  }

  private def enterLinearArg(mods: Set[Modifier])(linearArg: Name)
                            given Context: Lifted[Unit] = {
    val enterArg = {
      val isPrimitive = mods.contains(Primitive)
      val isWildcard  = linearArg == Wildcard
      (isPrimitive && !isWildcard) || !isPrimitive
    }
    if enterArg then enterLinear(linearArg)
    else ()
  }

  private def namedFunctionTerm(args: List[Tree], body: Tree)
                               (id: Id)
                               given Context, Mode: Lifted[Unit] = {
    enterScope(id, anon).flatMap { ctx1 =>
      implied for Context = ctx1
      for
        _ <- args.foldLeftE(())((_, n) => enterVariable(n))
        _ <- index(body)
      yield ()
    }
  }

  private def namedLinearFunctionTerm(arg: Tree, body: Tree)
                                     (id: Id)
                                     given Context, Mode: Lifted[Unit] = {
    enterScope(id, anon).flatMap { ctx1 =>
      implied for Context = ctx1
      for
        _ <- enterLinear(arg)
        _ <- index(body)
      yield ()
    }
  }

  private def namedPackageDef(pid: Tree, stats: List[Tree])
                             given Context, Mode: Lifted[Unit] = {
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

  private def namedApplyTerm(fun: Tree, args: List[Tree])
                            given Context, Mode: Lifted[Unit] = {
    for
      _ <- index(fun)
      _ <- args.foldLeftE(())((_, arg) => index(arg))
    yield ()
  }

  private def namedEvalTerm(fun: Tree, arg: Tree)
                           given Context, Mode: Lifted[Unit] = {
    for
      _ <- index(fun)
      _ <- index(arg)
    yield ()
  }

  private def namedLet(patt: Tree, value: Tree, continuation: Tree)
                      (id: Id)
                      given Context, Mode: Lifted[Unit] = {
    for
      _ <- index(value)
      _ <- enterScope(id, anon).flatMap { ctx1 =>
        implied for Context = ctx1
        for
          _ <- indexAsPattern(patt)
          _ <- index(continuation)
        yield ()
      }
    yield ()
  }

  private def namedLetTensor(x: Tree, z: Tree, s: Tree, t: Tree)
                            (id: Id)
                            given Context, Mode: Lifted[Unit] = {
    for
      _ <- index(s)
      _ <- enterScope(id, anon).flatMap { ctx1 =>
        implied for Context = ctx1
        for
          _ <- indexAsPattern(x)
          _ <- indexAsLinearPattern(z)
          _ <- index(t)
        yield ()
      }
    yield ()
  }

  private def namedCaseExpr(selector: Tree, cases: List[Tree])
                           given Context, Mode: Lifted[Unit] = {
    for
      _ <- index(selector)
      _ <- cases.foldLeftE(()) { (_, tree) =>
        tree match {
          case tree: (CaseClause | LinearCaseClause) =>
            enterScope(tree.id, anon).flatMap { ctx1 =>
              implied for Context = ctx1
              index(tree)
            }

          case _ =>
        }
      }
    yield ()
  }

  private def namedCaseClause(pat: Tree, guard: Tree, body: Tree)
                             given Context, Mode: Lifted[Unit] = {
    for
      _ <- indexAsPattern(pat)
      _ <- index(guard) // idents here are normal refs to variables in this scope
      _ <- index(body)
    yield ()
  }

  private def namedLinearCaseClause(pat: Tree, body: Tree)
                                   given Context, Mode: Lifted[Unit] = {
    for
      _ <- indexAsLinearPattern(pat)
      _ <- index(body)
    yield ()
  }

  private def namedAlternative(alts: List[Tree])
                              given Context, Mode: Lifted[Unit] = {
    implied for Mode = Mode.PatAlt
    alts.foldLeftE(())((_, t) => index(t))
  }

  private def namedUnapply(args: List[Tree])
                          given Context, Mode: Lifted[Unit] =
    args.foldLeftE(())((_, t) => index(t))

  private def namedBind(name: Name, pat: Tree)
                       given Context, Mode: Lifted[Unit] = {
    for
      _ <- enterVariable(name)
      _ <- index(pat)
    yield ()
  }

  private def namedParens(args: List[Tree]) given Context, Mode: Lifted[Unit] =
    args.foldLeftE(())((_, t) => index(t))

  private def namedIdentPat(name: Name) given Context, Mode: Lifted[Unit] =
    enterVariable(name)

  private def namedIdentLinearPat(name: Name) given Context, Mode: Lifted[Unit] =
    name.foldWildcard(())(enterLinear)

  private def index(tree: Tree) given Context, Mode: Lifted[Unit] = tree match {
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
    case u @ Let(p,v,c)           if isTerm     => namedLet(p,v,c)(u.id)
    case u @ LetTensor(x,z,s,t)   if isTerm     => namedLetTensor(x,z,s,t)(u.id)
    case u @ Function(ts,t)       if isTerm     => namedFunctionTerm(ts,t)(u.id)
    case u @ LinearFunction(a,b)  if isTerm     => namedLinearFunctionTerm(a,b)(u.id)
    case LinearCaseExpr(t,ts)     if isTerm     => namedCaseExpr(t,ts)
    case CaseExpr(t,ts)           if isTerm     => namedCaseExpr(t,ts)
    case CaseClause(p,g,b)        if isTerm     => namedCaseClause(p,g,b)
    case LinearCaseClause(p,b)    if isTerm     => namedLinearCaseClause(p,b)
    /* any mode */
    case Parens(ts)                             => namedParens(ts)
    case _: (Literal | Ident | Select | Tensor
            | Bang | WhyNot)
       | EmptyTree                              => // atomic
    /* error case */
    case _                                      => Err.namingMissing(tree)
  }
}