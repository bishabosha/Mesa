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
  import Context._
  import Mode._
  import util.Convert

  private[this] val anon = Name.From(emptyString)

  private[this] val toPairs = {
    import implied TreeOps._
    Convert[Tree, List[(Id, Name)]]
  }

  def namedDefDef(sig: Tree, tpeAs: Tree, body: Tree): (
    Contextual[Modal[Checked[Unit]]]) = {
      val DefSig(name, names) = sig
      for {
        ctx1  <-  enterFresh(sig.id, name)
        _     <-  names.mapE { t =>
                    val Ident(name) = t
                    for (_ <- enterFresh(t.id, name) given ctx1)
                      yield ()
                  }
        _     <-  index(body) given ctx1
      } yield ()
    }

  def namedFunctionTerm(args: List[Tree], body: Tree)(id: Id): (
    Contextual[Modal[Checked[Unit]]]) =
      for {
        ctx1  <-  enterFresh(id, anon)
        _     <-  args.mapE { t =>
                    val Tagged(name, _) = t
                    for (_ <- enterFresh(t.id, name) given ctx1)
                      yield ()
                  }
        _     <-  index(body) given ctx1
      } yield ()

  def namedPackageDef(pid: Tree, stats: List[Tree]): Contextual[Modal[Checked[Unit]]] =
    for {
      ctx1  <-  toPairs(pid).foldLeftE(ctx) { (ctx1, pair) =>
                  val (id, n) = pair
                  for (ctx <- enterFresh(id, n) given ctx1)
                    yield ctx
                }
      _     <-  stats.mapE(index(_) given ctx1)
    } yield ()

  def namedApplyTerm(fun: Tree, args: List[Tree]): Contextual[Modal[Checked[Unit]]] =
    for {
      _ <- index(fun)
      _ <- args.mapE(index)
    } yield ()

  def namedLet(letId: Tree, value: Tree, continuation: Tree)(id: Id): Contextual[Modal[Checked[Unit]]] =
    for {
      _     <-  index(value)
      ctx1  <-  enterFresh(id, anon)
      _     <-  failable {
                  val Ident(name) = letId
                  enterFresh(letId.id, name) given ctx1
                }
      _     <-  index(continuation) given ctx1
    } yield ()

  def namedCaseExpr(selector: Tree, cases: List[Tree]): Contextual[Modal[Checked[Unit]]] =
    for {
      _ <-  index(selector)
      _ <-  cases.mapE { caseClause =>
              for (ctx1 <- enterFresh(caseClause.id, anon))
                yield index(caseClause) given ctx1
            }
    } yield ()

  def namedCaseClause(pat: Tree, guard: Tree, body: Tree): Contextual[Modal[Checked[Unit]]] =
    for {
      _ <- indexAsPattern(pat) // all variables in pattern are declared in this current scope
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

  def namedBind(name: Name, pat: Tree)(id: Id): Contextual[Modal[Checked[Unit]]] =
    for {
      _ <- enterFresh(id, name)
      _ <- index(pat)
    } yield ()

  def namedParens(args: List[Tree]): Contextual[Modal[Checked[Unit]]] = {
    for (_ <- args.mapE(index))
      yield ()
  }

  def namedIdentPat(name: Name)(id: Id): Contextual[Modal[Checked[Unit]]] = {
    import CompilerErrorOps._
    if name != Name.Wildcard then
      for (_ <- enterFresh(id, name))
        yield ()
    else
      ()
  }

  def indexAsPattern(tree: Tree): Contextual[Checked[Unit]] = {
    implied for Mode = Mode.Pat
    index(tree)
  }

  def indexAsExpr(tree: Tree): Contextual[Checked[Unit]] = {
    implied for Mode = Mode.Term
    index(tree)
  }

  def index(tree: Tree): Contextual[Modal[Checked[Unit]]] = tree match {
    /* Type Trees */
    /* Pattern Trees */
    case Ident(n)           if mode.isPattern => namedIdentPat(n)(tree.id)
    // case Unapply(t,ts)      if mode.isPattern => namedUnapply(t,ts)
    case Bind(n,t)          if mode.isPattern => namedBind(n,t)(tree.id)
    case Alternative(ts)    if mode.isPattern => namedAlternative(ts)
    /* Term Trees */
    case PackageDef(t,ts)   if mode == Term   => namedPackageDef(t,ts)
    case Apply(t,ts)        if mode == Term   => namedApplyTerm(t,ts)
    case DefDef(_,s,t,b)    if mode == Term   => namedDefDef(s,t,b)
    case Let(n,v,c)         if mode == Term   => namedLet(n,v,c)(tree.id)
    case Function(ts,t)     if mode == Term   => namedFunctionTerm(ts,t)(tree.id)
    case CaseExpr(t,ts)     if mode == Term   => namedCaseExpr(t,ts)
    case CaseClause(p,g,b)  if mode == Term   => namedCaseClause(p,g,b)
    /* any mode */
    case Parens(ts)                           => namedParens(ts)
    case Literal(_)
       | Ident(_)
       | Select(_,_)
       | EmptyTree                            => // atomic
    /* error case */
    case _ =>
      import implied TreeOps._
      throw new IllegalStateException(s"Please implement Name pass for `${tree.userString}`")
  }
}