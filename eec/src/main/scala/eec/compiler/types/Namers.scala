package eec
package compiler
package types

object Namers {

  import ast.Trees._
  import Tree._
  import error.CompilerErrors._
  import CompilerErrorOps._
  import core.Contexts._
  import core.Names._
  import Name._
  import Context._
  import Mode._
  import util.Convert

  private[this] val anon = From(emptyString)

  def namedDefDef(name: Name, args: List[Tree], sigId: Id)
                 (tpeAs: Tree, body: Tree) given Context, Mode: Checked[Unit] = {
    enterFresh(sigId, name).map { ctx1 =>
      implied for Context = ctx1
      for {
        _ <-  args.mapE { t =>
                val Ident(name) = t
                enterFresh(t.id, name)
              }
        _ <-  index(body)
      } yield ()
    }
  }

  def namedFunctionTerm(args: List[Tree], body: Tree)
                       (id: Id) given Context, Mode: Checked[Unit] =
    enterFresh(id, anon).map { ctx1 =>
      implied for Context = ctx1
      for {
        _ <-  args.mapE { t =>
                val Tagged(name, _) = t
                enterFresh(t.id, name)
              }
        _ <-  index(body)
      } yield ()
    }

  def namedPackageDef
      (pid: Tree, stats: List[Tree]) given Context, Mode: Checked[Unit] = {
    import TreeOps._
    val cPkgCtx = pid.toNamePairs.foldLeftE(ctx) { (pkgCtx, pair) =>
      implied for Context = pkgCtx
      val (id, pkgName) = pair
      enterFresh(id, pkgName)
    }
    cPkgCtx.flatMap { pkgCtx =>
      implied for Context = pkgCtx
      for (_ <- stats.mapE(index(_)))
        yield ()
    }
  }

  def namedApplyTerm
      (fun: Tree, args: List[Tree]) given Context, Mode: Checked[Unit] =
    for {
      _ <- index(fun)
      _ <- args.mapE(index)
    } yield ()

  def namedLet(letId: Tree, value: Tree, continuation: Tree)
              (id: Id) given Context, Mode: Checked[Unit] =
    for {
      _ <-  index(value)
      _ <-  enterFresh(id, anon).map { ctx1 =>
              implied for Context = ctx1
              val Ident(name) = letId
              for {
                _ <- enterFresh(letId.id, name)
                _ <- index(continuation)
              } yield ()
            }
    } yield ()

  def namedCaseExpr
      (selector: Tree, cases: List[Tree]) given Context, Mode: Checked[Unit] =
    for {
      _ <-  index(selector)
      _ <-  cases.mapE { caseClause =>
              enterFresh(caseClause.id, anon).map { ctx1 =>
                implied for Context = ctx1
                index(caseClause)
              }
            }
    } yield ()

  def namedCaseClause
      (pat: Tree, guard: Tree, body: Tree) given Context, Mode: Checked[Unit] =
    for {
      _ <- indexAsPattern(pat)
      _ <- index(guard) // idents here are normal refs to variables in this scope
      _ <- index(body)
    } yield ()

  def namedAlternative(alts: List[Tree]) given Context, Mode: Checked[Unit] = {
    implied for Mode = Mode.PatAlt
    for (_ <- alts.mapE(index))
      yield ()
  }

  def namedBind(name: Name, pat: Tree)
               (id: Id) given Context, Mode: Checked[Unit] =
    for {
      _ <- enterFresh(id, name)
      _ <- index(pat)
    } yield ()

  def namedParens(args: List[Tree]) given Context, Mode: Checked[Unit] =
    for (_ <- args.mapE(index))
      yield ()

  def namedIdentPat(name: Name)(id: Id) given Context, Mode: Checked[Unit] =
    if name != Wildcard then
      for (_ <- enterFresh(id, name))
        yield ()
    else
      ()

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
    case Ident(n)           if mode.isPattern => namedIdentPat(n)(tree.id)
    case Bind(n,t)          if mode.isPattern => namedBind(n,t)(tree.id)
    case Alternative(ts)    if mode.isPattern => namedAlternative(ts)
    /* Term Trees */
    case PackageDef(t,ts)   if mode.isTerm    => namedPackageDef(t,ts)
    case Apply(t,ts)        if mode.isTerm    => namedApplyTerm(t,ts)
    case DefDef(
      _,
      s @ DefSig(n, ns),
      t,
      b)                    if mode.isTerm    => namedDefDef(n,ns,s.id)(t,b)
    case Let(n,v,c)         if mode.isTerm    => namedLet(n,v,c)(tree.id)
    case Function(ts,t)     if mode.isTerm    => namedFunctionTerm(ts,t)(tree.id)
    case CaseExpr(t,ts)     if mode.isTerm    => namedCaseExpr(t,ts)
    case CaseClause(p,g,b)  if mode.isTerm    => namedCaseClause(p,g,b)
    /* any mode */
    case Parens(ts)                           => namedParens(ts)
    case Literal(_)
       | Ident(_)
       | Select(_,_)
       | EmptyTree                            => // atomic
    /* error case */
    case _ =>
      import implied TreeOps._
      CompilerError.IllegalState(
        s"Namer implementation missing for `${tree.userString}`")
  }
}