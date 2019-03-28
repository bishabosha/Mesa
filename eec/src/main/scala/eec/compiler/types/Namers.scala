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
import Name._
import Context._
import Mode._
import util.Convert
import Convert._

import implied NameOps._
import implied TreeOps._

object Namers {
  private[this] val anon: Name  = emptyString.readAs

  def namedDefDef(name: Name, args: List[Name], sigId: Id)
                 (tpeAs: Tree, body: Tree)
                 given Context, Mode: Checked[Unit] = {
    enterScope(sigId, name).flatMap { ctx1 =>
      implied for Context = ctx1
      for {
        _ <- args.mapE(enterVariable)
        _ <- index(body)
      } yield ()
    }
  }

  def namedFunctionTerm(args: List[Tree], body: Tree)
                       (id: Id)
                       given Context, Mode: Checked[Unit] = {
    enterScope(id, anon).flatMap { ctx1 =>
      implied for Context = ctx1
      for {
        _ <-  args.map(_.convert: Name).mapE(enterVariable)
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
      for (_ <- stats.mapE(index(_)))
        yield ()
    }
  }

  def namedApplyTerm(fun: Tree, args: List[Tree])
                    given Context, Mode: Checked[Unit] = {
    for {
      _ <- index(fun)
      _ <- args.mapE(index)
    } yield ()
  }

  def namedLet(letName: Name)
              (value: Tree, continuation: Tree)
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

  def namedCaseExpr(selector: Tree, cases: List[Tree])
                   given Context, Mode: Checked[Unit] = {
    for {
      _ <-  index(selector)
      _ <-  cases.mapE { tree =>
              tree match {
                case tree: CaseClause =>
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

  def namedAlternative(alts: List[Tree]) given Context, Mode: Checked[Unit] = {
    implied for Mode = Mode.PatAlt
    for (_ <- alts.mapE(index))
    yield ()
  }

  def namedUnapply(args: List[Tree]) given Context, Mode: Checked[Unit] = {
    for (_ <- args.mapE(index))
    yield ()
  }

  def namedBind(name: Name, pat: Tree)
               given Context, Mode: Checked[Unit] = {
    for {
      _ <- enterVariable(name)
      _ <- index(pat)
    } yield ()
  }

  def namedParens(args: List[Tree]) given Context, Mode: Checked[Unit] = {
    for (_ <- args.mapE(index))
    yield ()
  }

  def namedIdentPat(name: Name) given Context, Mode: Checked[Unit] = {
    if name != Wildcard then {
      enterVariable(name)
    }
  }

  def indexAsPattern(tree: Tree) given Context: Checked[Unit] = {
    implied for Mode = Mode.Pat
    index(tree)
  }

  def indexAsExpr(tree: Tree) given Context: Checked[Unit] = {
    implied for Mode = Mode.Term
    index(tree)
  }

  def index(tree: Tree) given Context, Mode: Checked[Unit] = tree match {
    /* Pattern Trees */
    case Ident(n)           if isPattern  => namedIdentPat(n)
    case Bind(n,t)          if isPattern  => namedBind(n,t)
    case Alternative(ts)    if isPattern  => namedAlternative(ts)
    case Unapply(_,ts)      if isPattern  => namedUnapply(ts)
    /* Term Trees */
    case PackageDef(t,ts)   if isTerm     => namedPackageDef(t,ts)
    case Apply(t,ts)        if isTerm     => namedApplyTerm(t,ts)
    case DefDef(            // DefDef
      _,                    // DefDef
      s @ DefSig(n, ns),    // DefDef
      t,                    // DefDef
      b)                    if isTerm     => namedDefDef(n,ns,s.id)(t,b)
    case u @ Let(n,v,c)     if isTerm     => namedLet(n)(v,c)(u.id)
    case u @ Function(ts,t) if isTerm     => namedFunctionTerm(ts,t)(u.id)
    case CaseExpr(t,ts)     if isTerm     => namedCaseExpr(t,ts)
    case CaseClause(p,g,b)  if isTerm     => namedCaseClause(p,g,b)
    /* any mode */
    case Parens(ts)                       => namedParens(ts)
    case _: Literal
       | _: Ident
       | _: Select
       | EmptyTree                        => // atomic
    /* error case */
    case _                                => NamerErrors.namingMissing(tree)
  }
}