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

  private[this] val toPairs = {
    import implied TreeOps._
    Convert[Tree, List[(Id, Name)]]
  }

  def namedDefDef(sig: Tree, tpeAs: Tree, body: Tree): (
    Contextual[Modal[Checked[Unit]]]) = {
      val DefSig(name, names) = sig
      enterFresh(sig.id, name).map { ctx1 =>
        implied for Context = ctx1
        for {
          _ <-  names.mapE { t =>
                  val Ident(name) = t
                  enterFresh(t.id, name)
                }
          _ <-  index(body)
        } yield ()
      }
    }

  def namedFunctionTerm(args: List[Tree], body: Tree)(id: Id): (
    Contextual[Modal[Checked[Unit]]]) =
      for {
        ctx1  <-  enterFresh(id, anon)
        _     <-  checked {
                    implied for Context = ctx1
                    for {
                      _ <-  args.mapE { t =>
                              val Tagged(name, _) = t
                              enterFresh(t.id, name)
                            }
                      _ <-  index(body)
                    } yield ()
                  }
      } yield ()

  def namedPackageDef(pid: Tree, stats: List[Tree]): (
    Contextual[Modal[Checked[Unit]]]) =
      for {
        ctx1  <-  toPairs(pid).foldLeftE(ctx) { (ctx1, pair) =>
                    implied for Context = ctx1
                    val (id, n) = pair
                    enterFresh(id, n)
                  }
        _     <-  checked {
                    implied for Context = ctx1
                    stats.mapE(index(_))
                  }
      } yield ()

  def namedApplyTerm(fun: Tree, args: List[Tree]): (
    Contextual[Modal[Checked[Unit]]]) =
      for {
        _ <- index(fun)
        _ <- args.mapE(index)
      } yield ()

  def namedLet(letId: Tree, value: Tree, continuation: Tree)(id: Id): (
    Contextual[Modal[Checked[Unit]]]) =
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

  def namedCaseExpr(selector: Tree, cases: List[Tree]): (
    Contextual[Modal[Checked[Unit]]]) =
      for {
        _ <-  index(selector)
        _ <-  cases.mapE { caseClause =>
                enterFresh(caseClause.id, anon).map { ctx1 =>
                  implied for Context = ctx1
                  index(caseClause)
                }
              }
      } yield ()

  def namedCaseClause(pat: Tree, guard: Tree, body: Tree): (
    Contextual[Modal[Checked[Unit]]]) =
      for {
        _ <- indexAsPattern(pat)
        _ <- index(guard) // idents here are normal refs to variables in this scope
        _ <- index(body)
      } yield ()

  // def namedUnapply(functor: Tree, args: List[Tree]): Contextual[Modal[Checked[Unit]]] = {
  //   args.foreach(index)
  // }

  def namedAlternative(alts: List[Tree]): Contextual[Modal[Checked[Unit]]] = {
    implied for Mode = Mode.PatAlt
    for (_ <- alts.mapE(index))
      yield ()
  }

  def namedBind(name: Name, pat: Tree)(id: Id): (
    Contextual[Modal[Checked[Unit]]]) =
      for {
        _ <- enterFresh(id, name)
        _ <- index(pat)
      } yield ()

  def namedParens(args: List[Tree]): Contextual[Modal[Checked[Unit]]] =
    for (_ <- args.mapE(index))
      yield ()

  def namedIdentPat(name: Name)(id: Id): Contextual[Modal[Checked[Unit]]] =
    if name != Wildcard then
      for (_ <- enterFresh(id, name))
        yield ()
    else
      ()

  def indexAsPattern(tree: Tree): Contextual[Checked[Unit]] = {
    implied for Mode = Mode.Pat
    index(tree)
  }

  def indexAsExpr(tree: Tree): Contextual[Checked[Unit]] = {
    implied for Mode = Mode.Term
    index(tree)
  }

  def index(tree: Tree): Contextual[Modal[Checked[Unit]]] = tree match {
    /* Pattern Trees */
    case Ident(n)           if mode.isPattern => namedIdentPat(n)(tree.id)
    // case Unapply(t,ts)      if mode.isPattern => namedUnapply(t,ts)
    case Bind(n,t)          if mode.isPattern => namedBind(n,t)(tree.id)
    case Alternative(ts)    if mode.isPattern => namedAlternative(ts)
    /* Term Trees */
    case PackageDef(t,ts)   if mode.isTerm    => namedPackageDef(t,ts)
    case Apply(t,ts)        if mode.isTerm    => namedApplyTerm(t,ts)
    case DefDef(_,s,t,b)    if mode.isTerm    => namedDefDef(s,t,b)
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