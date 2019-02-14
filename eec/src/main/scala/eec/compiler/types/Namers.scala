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
    val oldCtx = ctx
    def namedDef(name: Name, args: List[Name]): Unit = {
      implicit val newCtx: Context = enterFresh(name)(oldCtx)
      args.foreach(enterLeaf)
      index(body)
    }
    val DefSig(_, name, names) = sig
    namedDef(name, names)
  }

  def namedFunctionTerm(args: List[Tree], body: Tree): Contextual[Modal[Unit]] = {
    val oldCtx = ctx
    def inner: Unit = {
      implicit val newCtx: Context = enterFresh(anon)(oldCtx)
      args.foreach {
        case Tagged(_, name, _) => enterLeaf(name)
        case _ =>
      }
      index(body)
    }
    inner
  }

  def namedTagged(name: Name): Contextual[Modal[Unit]] = {
    enterLeaf(name)
  }

  def namedPackageDef(pid: Tree, stats: List[Tree]): Contextual[Modal[Unit]] = {
    import TreeOps._
    var ctxNew = ctx
    pid.toNames.foreach { n => ctxNew = enterFresh(n)(ctxNew) }
    stats.foreach(index(_)(ctxNew))
  }

  def namedApplyTerm(fun: Tree, args: List[Tree]): Contextual[Modal[Unit]] = {
    index(fun)
    args.foreach(index)
  }

  def namedLet(name: Name, value: Tree, continuation: Tree): Contextual[Modal[Unit]] = {
    val oldCtx = ctx
    def inner: Unit = {
      implicit val newCtx = enterFresh(anon)(oldCtx)
      enterLeaf(name)
      index(continuation)
    }
    index(value)
    inner
  }

  def namedIf(cond: Tree, thenp: Tree, elsep: Tree): Contextual[Modal[Unit]] = {
    index(cond)
    index(thenp)(enterFresh(anon))
    index(elsep)(enterFresh(anon))
  }

  def namedCaseExpr(selector: Tree, cases: List[Tree]): Contextual[Modal[Unit]] = {
    index(selector)
    cases.foreach(index(_)(enterFresh(anon)))
  }

  def namedCaseClause(pat: Tree, guard: Tree, body: Tree): Contextual[Modal[Unit]] = {
    indexAsPattern(pat) // all variables in pattern are declared in this current scope
    index(guard) // idents here are normal refs to variables in this scope
    index(body) 
  }

  def namedUnapply(functor: Tree, args: List[Tree]): Contextual[Modal[Unit]] = {
    args.foreach(index)
  }

  def namedAlternative(alts: List[Tree]): Contextual[Modal[Unit]] = {
    implicit val newMode = Mode.PatAlt
    alts.foreach(index)
  }

  def namedBind(name: Name, pat: Tree): Contextual[Modal[Unit]] = {
    enterLeaf(name)
    index(pat)
  }

  def namedParens(args: List[Tree]): Contextual[Modal[Unit]] = {
    args.foreach(index)
  }

  def namedIdentPat(name: Name): Contextual[Modal[Unit]] = {
    if name != Name.Wildcard then {
      enterLeaf(name)
    }
  }

  def indexAsPattern(tree: Tree): Contextual[Unit] = {
    implicit val mode: Mode = Mode.Pat
    index(tree)
  }

  def indexAsExpr(tree: Tree): Contextual[Unit] = {
    implicit val mode: Mode = Mode.Term
    index(tree)
  }

  def index(tree: Tree): Contextual[Modal[Unit]] = tree match {
    /* Type Trees */
    /* Pattern Trees */
    case Ident(_,n)           if mode.isPattern => namedIdentPat(n)
    case Unapply(_,t,ts)      if mode.isPattern => namedUnapply(t,ts)
    case Bind(_,n,t)          if mode.isPattern => namedBind(n,t)
    case Alternative(_,ts)    if mode.isPattern => namedAlternative(ts)
    /* Term Trees */
    case PackageDef(_,t,ts)   if mode == Term   => namedPackageDef(t,ts)
    case Apply(_,t,ts)        if mode == Term   => namedApplyTerm(t,ts)
    case DefDef(_,_,s,t,b)    if mode == Term   => namedDefDef(s,t,b)
    case If(_,c,t,e)          if mode == Term   => namedIf(c,t,e)
    case Let(_,n,v,c)         if mode == Term   => namedLet(n,v,c)
    case Function(_,ts,t)     if mode == Term   => namedFunctionTerm(ts,t)
    case CaseExpr(_,t,ts)     if mode == Term   => namedCaseExpr(t,ts)
    case CaseClause(_,p,g,b)  if mode == Term   => namedCaseClause(p,g,b)
    /* any mode */
    case Parens(_,ts)                           => namedParens(ts)
    case Literal(_,_) | Ident(_,_) | EmptyTree  => // atomic
    /* error case */
    case _ =>
      import TreeOps._
      throw new IllegalStateException(s"Please implement Name pass for `${tree.userString}`")
  }
}