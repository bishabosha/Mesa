package eec
package compiler
package types

object Namers {

  import ast.Trees._
  import Tree._
  import error.CompilerErrors._
  import core.Contexts._
  import core.Names._
  import Context._
  import Mode._

  private[this] val anon = Name.From(emptyString)

  def namedDefDef(sig: Tree, tpeAs: Tree, body: Tree): Contextual[Modal[Unit]] = {
    val DefSig(name, names) = sig
    val newCtx = enterFresh(sig.id, name)
    locally {
      implied for Context = newCtx
      names.foreach {
        case t @ Ident(name) => enterLeaf(t.id, name)
        case _ =>
      }
      index(body)
    }
  }

  def namedFunctionTerm(args: List[Tree], body: Tree)(id: Id): Contextual[Modal[Unit]] = {
    val newCtx = enterFresh(id, anon)
    locally {
      implied for Context = newCtx
      args.foreach {
        case t @ Tagged(name, _) => enterLeaf(t.id, name)
        case _ =>
      }
      index(body)
    }
  }

  def namedPackageDef(pid: Tree, stats: List[Tree]): Contextual[Modal[Unit]] = {
    var ctxNew = ctx
    import implied TreeOps._
    toNamePairs(pid).foreach { (id, n) => ctxNew = enterFresh(id, n) given ctxNew }
    stats.foreach(index(_) given ctxNew)
  }

  def namedApplyTerm(fun: Tree, args: List[Tree]): Contextual[Modal[Unit]] = {
    index(fun)
    args.foreach(index)
  }

  def namedLet(letId: Tree, value: Tree, continuation: Tree)(id: Id): Contextual[Modal[Unit]] = {
    index(value)
    val newCtx = enterFresh(id, anon)
    locally {
      implied for Context = newCtx
      val Ident(name) = letId
      enterLeaf(letId.id, name)
      index(continuation)
    }
  }

  def namedCaseExpr(selector: Tree, cases: List[Tree]): Contextual[Modal[Unit]] = {
    index(selector)
    cases.foreach(c => index(c) given enterFresh(c.id, anon))
  }

  def namedCaseClause(pat: Tree, guard: Tree, body: Tree): Contextual[Modal[Unit]] = {
    indexAsPattern(pat) // all variables in pattern are declared in this current scope
    index(guard) // idents here are normal refs to variables in this scope
    index(body) 
  }

  // def namedUnapply(functor: Tree, args: List[Tree]): Contextual[Modal[Unit]] = {
  //   args.foreach(index)
  // }

  def namedAlternative(alts: List[Tree]): Contextual[Modal[Unit]] = {
    implied for Mode = Mode.PatAlt
    alts.foreach(index)
  }

  def namedBind(name: Name, pat: Tree)(id: Id): Contextual[Modal[Unit]] = {
    enterLeaf(id, name)
    index(pat)
  }

  def namedParens(args: List[Tree]): Contextual[Modal[Unit]] = {
    args.foreach(index)
  }

  def namedIdentPat(name: Name)(id: Id): Contextual[Modal[Unit]] = {
    if name != Name.Wildcard then {
      enterLeaf(id, name)
    }
  }

  def indexAsPattern(tree: Tree): Contextual[Unit] = {
    implied for Mode = Mode.Pat
    index(tree)
  }

  def indexAsExpr(tree: Tree): Contextual[Unit] = {
    implied for Mode = Mode.Term
    index(tree)
  }

  def index(tree: Tree): Contextual[Modal[Unit]] = tree match {
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