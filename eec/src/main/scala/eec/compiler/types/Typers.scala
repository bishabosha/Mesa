package eec
package compiler
package types

object Typers {

  import Types._
  import Types.Type._
  import core.Names._
  import core.Constants._
  import core.Constants.Constant._
  import core.Modifiers._
  import ast.Trees._
  import ast.Trees.Tree._
  import ast.Trees.untyped._
  import error.CompilerErrors._
  import CompilerErrorOps._
  import core.Contexts._
  import Context._

  private[Typers] val any = Type.WildcardType

  def (tpe: Type) isComputationType: Boolean = {
    import Name._
    tpe match {
      case AppliedType(TypeRef(ComputationTag), List(_)) => true
      case FunctionType(_, tpe) => tpe.isComputationType
      case Product(ts) => ts.forall(isComputationType)
      case _ => false
    }
  }

  def (tpe: Type) =!= (other: Type): Boolean = tpe == other ||
    tpe == Type.WildcardType ||
    other == Type.WildcardType

  def (ts: List[Tree]) unifiedTpe: Checked[Type] =
    if ts.isEmpty then {
      NoType
    } else {
      import TypeOps._
      val tpe = ts.head.tpe
      val unified = ts.tail.foldLeft(true)((acc, t) => acc && t.tpe == tpe)
      if unified then
        tpe
      else
        CompilerError.UnexpectedType(s"Types do not unify to ${tpe.userString}")
    }

  def checkFunWithProto(funTyp: Type, proto: Type, pt: Type)(tree: => Tree): (
    Checked[Type]) = {
      (funTyp, proto) match {
        case (FunctionType(arg, ret), FunctionType(arg1, ret1)) =>
          if arg =!= arg1 then
            if ret =!= ret1 then
              pt match {
                case `any` =>
                  (arg, ret) match {
                    // very temporary unification mechanism for ! types
                    case (WildcardType, AppliedType(tpe, List(WildcardType))) =>
                      AppliedType(tpe, List(arg1))
                    case _ =>
                      ret
                  }
                case _ =>
                  import TreeOps._
                  val retStr  = ret.userString
                  val ptStr   = pt.userString
                  val treeStr     = tree.userString
                  CompilerError.UnexpectedType(
                    s"$retStr != $ptStr in application expr:\n$treeStr")
              }
            else
              CompilerError.UnexpectedType("Function Definition does not match return type")
          else {
            import TreeOps._
            val argStr  = arg.userString
            val retStr  = ret.userString
            val arg1Str = arg1.userString
            val ret1Str = ret1.userString
            val treeStr     = tree.userString
            CompilerError.UnexpectedType(
              s"Function definition `$argStr -> $retStr` does not match args. Expected `$argStr` but was `$arg1Str` in application expr:\n$treeStr")
          }
        case _ =>
          CompilerError.IllegalState("Can not apply to non function type")
      }
    }

  def (tree: Tree) typedAsExpr(pt: Type): Contextual[Checked[Tree]] = {
    implicit val newMode = Mode.Term
    tree.typed(pt)
  }

  def (tree: Tree) typedAsType(pt: Type): Contextual[Checked[Tree]] = {
    implicit val newMode = Mode.Type
    tree.typed(pt)
  }

  def (tree: Tree) typedAsPattern(pt: Type): Contextual[Checked[Tree]] = {
    implicit val newMode = Mode.Pat
    tree.typed(pt)
  }

  // def (tree: Tree) typedAsPackageSelection(id: Id, pt: Type): Contextual[Checked[Tree]] = {
  //   implicit val newMode = Mode.PackageSelect
  //   tree.typed(pt)
  // }

  def constantTpe: Constant => Type = {
    case BooleanConstant(_) => Bootstraps.BooleanType
    case BigDecConstant(_)  => Bootstraps.DecimalType
    case BigIntConstant(_)  => Bootstraps.IntegerType
    case CharConstant(_)    => Bootstraps.CharType
    case StringConstant(_)  => Bootstraps.StringType
  }

  def typedFunctionTerm(args: List[Tree], body: Tree)(id: Id, pt: Type): (
    Contextual[Modal[Checked[Tree]]]) = {

      def function(args1: List[Tree], body1: Tree): Checked[Tree] = {
        import TypeOps._
        val tpes  = args1.map(_.tpe)
        val fType = tpes.foldRight(body1.tpe) { FunctionType(_,_) }
        if !fType.isComputationType then
          CompilerError.UnexpectedType("Function does not have computational co-domain")
        else
          Function(args1, body1)(id, fType)
      }

      for {
        fCtx  <- ctx.lookIn(id)
        args1 <- args.flatMapM(_.typed(pt)(fCtx))
        body1 <- body.typed(pt)(fCtx)
        f     <- function(args1, body1)
      } yield f
    }

  def typedFunctionType(args: List[Tree], body: Tree)(id: Id, pt: Type): (
    Contextual[Modal[Checked[Tree]]]) = {
      def function(args1: List[Tree], body1: Tree): Checked[Tree] = {
        import TypeOps._
        val tpe = args1.map(_.tpe).toType
        val fType = FunctionType(tpe, body1.tpe)
        if !fType.isComputationType then
          CompilerError.UnexpectedType("Function does not have computational co-domain")
        else
          Function(args1, body1)(id, fType)
      }

      for {
        args1 <- args.flatMapM(_.typed(pt))
        body1 <- body.typed(pt)
        f     <- function(args1, body1)
      } yield f
    }

  def typedTagged(arg: Name, tpeTree: Tree)(id: Id, pt: Type): (
    Contextual[Modal[Checked[Tree]]]) = {

      import TypeOps._

      for (tpeTree1 <- tpeTree.typedAsType(pt))
        yield {
          ctx.putType(arg, tpeTree1.tpe)
          Tagged(arg, tpeTree1)(id, tpeTree1.tpe)
        }
    }

  def typedParens(tpeTrees: List[Tree])(id: Id, pt: Type): Contextual[Modal[Checked[Tree]]] = {
    if tpeTrees.isEmpty then {
      val tpe = Product(List())
      Parens(List())(id, tpe)
    } else {

      import TypeOps._

      for {
        tpes1 <- tpeTrees.flatMapM(_.typed(any))
      } yield {
        val tpes      = tpes1.map(_.tpe)
        val tupleTyp  = tpes.toType
        if tupleTyp =!= pt then {
          Parens(tpes1)(id, tupleTyp)
        } else {
          import TreeOps._
          val tupleTypeStr  = tupleTyp.userString
          val expectedStr   = pt.userString
          val treeStr       = Parens(tpeTrees)(Id.noId, Type.NoType).userString
          CompilerError.UnexpectedType(
            s"expected `$expectedStr` but was `$tupleTypeStr` in expr:\n$treeStr")
        }
      }
    }
  }

  def typedApply(functor: Tree, args: List[Tree])(id: Id, pt: Type): (
    Contextual[Modal[Checked[Tree]]]) = {
      /*TODO: type application for generic functors
        form type prototype by typing args and then forming typeTpe:
          [...args1] => pt

        then type functor and retrieve its symbol
        check tpe matches prototype
      */
      for {
        functor1 <- functor.typed(pt)
        args1 <- args.flatMapM(_.typed(pt))
      } yield {
        import TypeOps._
        val selTpe = functor1.tpe
        val argTpes = args1.map(_.tpe)
        val tpe = AppliedType(selTpe, argTpes)
        Apply(functor1, args1)(id, tpe)
      }
    }

  def typedApplyTerm(fun: Tree, args: List[Tree])(id: Id, pt: Type): (
    Contextual[Modal[Checked[Tree]]]) = {

      import TypeOps._

      for {
        args1     <- args.flatMapM(_.typed(any))
        funProto  <- FunctionType(args1.map(_.tpe).toType, pt)
        fun1      <- fun.typed(any)
        tpe       <- checkFunWithProto(fun1.tpe, funProto, pt)(Apply(fun, args)(Id.noId, Type.NoType))
      } yield Apply(fun1, args1)(id, tpe)
    }

  def typedIf(cond: Tree, thenp: Tree, elsep: Tree)(id: Id, pt: Type): (
    Contextual[Modal[Checked[Tree]]]) = {

      import TypeOps._

      for {
        cond1     <- cond.typed(Bootstraps.BooleanType)
        thenp1    <- thenp.typed(pt)
        elsep1    <- elsep.typed(pt)
        strictEq  <- thenp1.tpe =!= elsep1.tpe
      } yield {
        if !strictEq then {
          import TreeOps._
          val thenpTpeStr = thenp1.tpe.userString
          val elsepTpeStr = elsep1.tpe.userString
          val treeStr     = If(cond,thenp,elsep)(Id.noId, Type.NoType).userString
          CompilerError.UnexpectedType(
            s"$thenpTpeStr != $elsepTpeStr in `if` expr:\n$treeStr")
        } else {
          If(cond1, thenp1, elsep1)(id, thenp1.tpe)
        }
      }
  }

  def typedLet(letId: Tree, value: Tree, continuation: Tree)(id: Id, pt: Type): (
    Contextual[Modal[Checked[Tree]]]) = {

      import TreeOps._
      import core.Names.NameOps._
      import TypeOps._

      val Ident(name) = letId

      def unwrapCompApply(tpe: Type): Checked[Type] = tpe match {
        case AppliedType(TypeRef(Name.ComputationTag), List(t)) =>
          t
        case _ =>
          CompilerError.UnexpectedType(s"Can not infer type of `! ${name.userString}` = `${value.userString}` as of computation type.")
      }

      for {
        value1        <- value.typed(pt) // need to typecheck value1 as being comp type
        value1Inner   <- unwrapCompApply(value1.tpe)
        letId1        <- Ident(name)(letId.id, value1Inner)
        continuation1 <- continuation.typed(pt)
      } yield Let(letId1, value1, continuation1)(id, continuation1.tpe)
  }

  def typedCaseExpr(selector: Tree, cases: List[Tree])(id: Id, pt: Type): (
    Contextual[Modal[Checked[Tree]]]) = {

      import TypeOps._

      def (ts: List[Tree]) mapAsCaseClauses(selTpe: Type)(pt: Type): (
        Contextual[Modal[Checked[List[Tree]]]]) = {
          ts.flatMapM({
            case t @ CaseClause(p,g,b) => typedCaseClause(p, g, b, selTpe)(t.id, pt)
            case unknown => CompilerError.IllegalState(
                s"$unknown is not Tree.CaseClause(_,_,_,_)")
          })
        }

      for {
        selector1 <- selector.typed(pt)
        cases1    <- cases.mapAsCaseClauses(selector1.tpe)(pt)
        tpe       <- cases1.unifiedTpe
      } yield CaseExpr(selector1, cases1)(id, tpe)
    }

  def typedCaseClause(pat: Tree, guard: Tree, body: Tree, selTpe: Type)(
    id: Id, pt: Type): Contextual[Modal[Checked[Tree]]] = {
      import TypeOps._
      for {
        pat1    <- pat.typedAsPattern(selTpe)
        // TODO: need to set any variables in context
        guard1  <- guard.typedAsExpr(Bootstraps.BooleanType)
        body1   <- body.typedAsExpr(pt)
      } yield CaseClause(pat1, guard1, body1)(id, body1.tpe)
    }

  def typedUnapply(f: Tree, args: List[Tree])(id: Id, pt: Type): (
    Contextual[Modal[Checked[Tree]]]) = {
      import TypeOps._
      for {
        id1 <- f.typed(pt)
        args1 <- args.flatMapM(_.typed(pt))
      } yield Unapply(id1, args1)(id, id1.tpe) // need to lookup id1 to get idArgs, if idArgs types match args1 then ok
    }

  def typedBind(name: Name, body: Tree)(id: Id, pt: Type): Contextual[Modal[Checked[Tree]]] = {
    import Mode._
    for {
      body1 <- body.typed(pt)
    } yield {
      import core.Names._
      import TypeOps._
      if name == Name.Wildcard then
        body1
      else if mode == PatAlt then
        CompilerError.IllegalState(
          s"Illegal variable ${name} in pattern alternative")
      else
        // TODO: add name to context with type of body1
        Bind(name, body1)(id, body1.tpe)
    }
  }

  def typedAlternative(patterns: List[Tree])(id: Id, pt: Type): Contextual[Modal[Checked[Tree]]] = {
      implicit val newMode = Mode.PatAlt
      for {
        patterns1 <- patterns.flatMapM(_.typed(pt))
        tpe       <- patterns1.unifiedTpe
      } yield Alternative(patterns1)(id, tpe)
    }

  def typedPackageDef(pid: Tree, stats: List[Tree])(id: Id, pt: Type): (
    Contextual[Modal[Checked[Tree]]]) = {
      import TypeOps._
      for {
        pid1    <- pid.typedAsType(any)
        // symbol  <- pid1.defineSymbol
        stats1  <- typedStats(stats /*, symbol */)
      } yield PackageDef(pid1, stats1)(id, pid1.tpe)
    }

  def typedStats(stats: List[Tree] /*, symbol: ??? */): (
    Contextual[Modal[Checked[List[Tree]]]]) =
      stats.flatMapM(_.typed(any))

  def typedDefDef(
    modifiers: Set[Modifier],
    sig: Tree,
    tpeAs: Tree,
    body: Tree)(
    id: Id,
    pt: Type): Contextual[Modal[Checked[Tree]]] = {
      import TypeOps._
      for {
        tpeAs1  <- tpeAs.typedAsType(any)
        tpe     <- tpeAs1.tpe
        sig1    <- sig.typed(tpe)
        body1   <- body.typed(tpe)
      } yield DefDef(modifiers, sig1, tpeAs1, body1)(id, tpe)
    }

  def typedDefSig(name: Name, args: List[Tree])(id: Id, pt: Type): (
    Contextual[Modal[Checked[Tree]]]) =
      CompilerError.IllegalState("Function declarations unimplemented")

  def typedSelectType(from: Tree, name: Name)(id: Id, pt: Type): (
    Contextual[Modal[Checked[Tree]]]) =
      CompilerError.IllegalState("Typed member selections unimplemented")

  def typedSelectTerm(from: Tree, name: Name)(id: Id, pt: Type): (
    Contextual[Modal[Checked[Tree]]]) =
      CompilerError.SyntaxError("Member selections do not exist for terms.")

  def typedIdentType(name: Name)(id: Id, pt: Type): Contextual[Modal[Checked[Tree]]] =
    Ident(name)(id, TypeRef(name))

  def typedIdentPat(name: Name)(id: Id, pt: Type): Contextual[Modal[Checked[Tree]]] = {
    import Mode._
    if mode == PatAlt then
      CompilerError.SyntaxError("Illegal variable in pattern alternative")
    else
      Ident(name)(id, pt) // TODO: need to add name to context
  }

  def typedIdentTerm(name: Name)(id: Id, pt: Type): Contextual[Modal[Checked[Tree]]] = {
    for {
      fstCtx    <- ctx.firstCtx(name)
      idRefTpe  <- fstCtx.getType(name)
    } yield {
      if idRefTpe =!= pt then {
        Ident(name)(id, idRefTpe)
      } else {
        import NameOps._
        val nameStr     = name.userString
        val idRefTpeStr = idRefTpe.userString
        val ptStr       = pt.userString
        CompilerError.UnexpectedType(s"Expected type for identifier $nameStr of `$ptStr` but was `$idRefTpe`")
      }
    }
  }

  def typedLiteral(constant: Constant)(id: Id): Contextual[Modal[Checked[Tree]]] =
    Literal(constant)(id, constantTpe(constant))

  def check(typed: Tree)(pt: Type): Contextual[Modal[Checked[Tree]]] = {
      import TypeOps._
      import Mode._

      lazy val typedTpe = typed.tpe

      def ignoreType(tree: Tree) = tree match {
        case EmptyTree | TreeSeq(_) => true
        case _ => false
      }

      val safe = pt == Types.Type.WildcardType ||
          ignoreType(typed) ||
          pt =!= typedTpe

      if safe then {
        typed
      } else {
        import TreeOps._
        val treeStr = typed.userString
        val modeStr = mode.userString
        val ptStr   = pt.userString
        val tpeStr  = typedTpe.userString
        CompilerError.UnexpectedType(
              s"type $ptStr != $tpeStr in $modeStr:\n$treeStr")
      }
    }

  def (tree: Tree) typed(pt: Type): Contextual[Modal[Checked[Tree]]] = {
    import Mode._
    def inner(tree: Tree, pt: Type): Contextual[Modal[Checked[Tree]]] = tree match {
      /* Type Trees */
      case Select(t,n)      if mode == Type   => typedSelectType(t,n)(tree.id, pt)
      case Ident(n)         if mode == Type   => typedIdentType(n)(tree.id, pt)
      case Apply(t,ts)      if mode == Type   => typedApply(t,ts)(tree.id, pt)
      case Function(ts,t)   if mode == Type   => typedFunctionType(ts,t)(tree.id, pt)
      /* Pattern Trees */
      case Ident(n)         if mode.isPattern => typedIdentPat(n)(tree.id, pt)
      case Unapply(t,ts)    if mode.isPattern => typedUnapply(t,ts)(tree.id, pt)
      case Bind(n,t)        if mode.isPattern => typedBind(n,t)(tree.id, pt)
      case Alternative(ts)  if mode.isPattern => typedAlternative(ts)(tree.id, pt)
      /* Term Trees */
      case PackageDef(t,ts) if mode == Term   => typedPackageDef(t,ts)(tree.id, pt)
      case Apply(t,ts)      if mode == Term   => typedApplyTerm(t,ts)(tree.id, pt)
      case DefDef(m,s,t,b)  if mode == Term   => typedDefDef(m,s,t,b)(tree.id, pt)
      case DefSig(n,ns)     if mode == Term   => typedDefSig(n,ns)(tree.id, pt)
      case If(c,t,e)        if mode == Term   => typedIf(c,t,e)(tree.id, pt)
      case Let(n,v,c)       if mode == Term   => typedLet(n,v,c)(tree.id, pt)
      case Function(ts,t)   if mode == Term   => typedFunctionTerm(ts,t)(tree.id, pt)
      case Tagged(n,t)      if mode == Term   => typedTagged(n,t)(tree.id, pt)
      case CaseExpr(t,ts)   if mode == Term   => typedCaseExpr(t,ts)(tree.id, pt)
      case Select(t,n)      if mode == Term   => typedSelectTerm(t,n)(tree.id, pt)
      case Ident(n)         if mode == Term   => typedIdentTerm(n)(tree.id, pt)
      /* any mode */
      case Literal(c)                         => typedLiteral(c)(tree.id)
      case Parens(ts)                         => typedParens(ts)(tree.id, pt)
      case t @ ( TreeSeq(_)
                | CaseClause(_,_,_)
                | EmptyTree)                  => t
      /* error case */
      case _ =>
        import TreeOps._
        CompilerError.IllegalState(s"Typing not implemented for <${mode.userString}, ${tree.userString}>")
    }
    inner(tree, pt).map(check(_)(pt))
  }
}