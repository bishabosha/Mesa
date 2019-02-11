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

  enum Mode {

    case Type, Term, Pat, PatAlt //, PackageSelect

    def isPattern = this match {
      case Pat | PatAlt => true
      case _            => false
    }
  }

  type Modal[X] = implicit Mode => X

  object Mode {

    import util.Showable

    def mode(implicit m: Mode) = m

    implicit val ModeShowable: Showable[Mode] = new {
      override def (m: Mode) userString: String = m match {
        case Pat | PatAlt => "pattern"
        case Term => "term"
        case Type => "type"
      }
    }
  }

  private[Typers] val any = Type.WildcardType

  def (tpe: Type) =!= (other: Type): Boolean = tpe == other || other == any

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
                case `any` => ret
                case _ =>
                  import core.Printing.untyped.AstOps._
                  val retStr  = ret.userString
                  val ptStr   = pt.userString
                  val ast     = tree.toAst
                  CompilerError.UnexpectedType(
                    s"$retStr != $ptStr in `application` expr:\n$ast")
              }
            else
              CompilerError.UnexpectedType("Function Definition does not match return type")
          else {
            import core.Printing.untyped.AstOps._
            val argStr  = arg.userString
            val arg1Str = arg1.userString
            val retStr  = ret.userString
            val ret1Str = ret1.userString
            val ast     = tree.toAst
            CompilerError.UnexpectedType(
              s"Function definition does not match args. Expected `$argStr -> $retStr` but was `$arg1Str -> $ret1Str` in `application` expr:\n$ast")
          }
        case _ =>
          CompilerError.IllegalState("Can not apply to non function type")
      }
    }

  def (tree: Tree) typedAsExpr(pt: Type): Checked[Tree] = {
    implicit val newMode = Mode.Term
    tree.typed(pt)
  }

  def (tree: Tree) typedAsType(pt: Type): Checked[Tree] = {
    implicit val newMode = Mode.Type
    tree.typed(pt)
  }

  def (tree: Tree) typedAsPattern(pt: Type): Checked[Tree] = {
    implicit val newMode = Mode.Pat
    tree.typed(pt)
  }

  // def (tree: Tree) typedAsPackageSelection(pt: Type): Checked[Tree] = {
  //   implicit val newMode = Mode.PackageSelect
  //   tree.typed(pt)
  // }

  def (tree: Tree) typed(pt: Type): Modal[Checked[Tree]] = {

    def constantTpe: Constant => Type = {
      case BooleanConstant(_) => Bootstraps.BooleanType
      case BigDecConstant(_)  => Bootstraps.DecimalType
      case BigIntConstant(_)  => Bootstraps.IntegerType
      case CharConstant(_)    => Bootstraps.CharType
      case StringConstant(_)  => Bootstraps.StringType
    }

    def typedFunctionTerm(args: List[Tree], body: Tree)(pt: Type): (
      Modal[Checked[Tree]]) = {

        def function(args1: List[Tree], body1: Tree) = {
          import TypeOps._
          val tpes  = args1.map(_.tpe)
          val fType = tpes.foldRight(body1.tpe) { FunctionType(_,_) }
          Function(fType, args1, body1)
        }

        for {
          args1 <- args.flatMapM(_.typed(pt))
          body1 <- body.typed(pt)
        } yield function(args1, body1)
      }

    def typedFunctionType(args: List[Tree], body: Tree)(pt: Type): Modal[Checked[Tree]] = {

      def function(args1: List[Tree], body1: Tree) = {
        import TypeOps._
        val tpe = args1.map(_.tpe).toType
        val fType = FunctionType(tpe, body1.tpe)
        Function(fType, args1, body1)
      }

      for {
        args1 <- args.flatMapM(_.typed(pt))
        body1 <- body.typed(pt)
      } yield function(args1, body1)
    }

    def typedTagged(arg: Name, tpeTree: Tree)(pt: Type): (
      Modal[Checked[Tree]]) = {

        import TypeOps._

        for (tpeTree1 <- tpeTree.typedAsType(pt))
          yield Tagged(tpeTree1.tpe, arg, tpeTree1)
      }

    def typedParens(tpeTrees: List[Tree])(pt: Type): Modal[Checked[Tree]] = {
      if tpeTrees.isEmpty then {
        val tpe = Product(List())
        Parens(tpe, List())
      } else {

        import TypeOps._

        for {
          tpes1 <- tpeTrees.flatMapM(_.typed(any))
        } yield {
          val tpes      = tpes1.map(_.tpe)
          val tupleTyp  = tpes.toType
          if tupleTyp =!= pt then {
            Parens(tupleTyp, tpes1)
          } else {
            import core.Printing.untyped.AstOps._
            val tupleTypeStr  = tupleTyp.userString
            val expectedStr   = pt.userString
            val ast           = Parens(any, tpeTrees).toAst
            CompilerError.UnexpectedType(
              s"expected `$expectedStr` but was `$tupleTypeStr` in expr:\n$ast")
          }
        }
      }
    }

    def typedApply(functor: Tree, args: List[Tree])(pt: Type): (
      Modal[Checked[Tree]]) = {
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
          Apply(tpe, functor1, args1)
        }
      }

    def typedApplyTerm(fun: Tree, args: List[Tree])(pt: Type): (
      Modal[Checked[Tree]]) = {

        import TypeOps._
        import core.Printing.untyped.AstOps._

        for {
          args1     <- args.flatMapM(_.typed(any))
          funProto  <- FunctionType(args1.map(_.tpe).toType, pt)
          fun1      <- fun.typed(any)
          tpe       <- checkFunWithProto(fun1.tpe, funProto, pt)(Apply(any, fun, args))
        } yield Apply(tpe, fun1, args1)
      }

    def typedIf(cond: Tree, thenp: Tree, elsep: Tree)(pt: Type): (
      Modal[Checked[Tree]]) = {

        import TypeOps._

        for {
          cond1     <- cond.typed(Bootstraps.BooleanType)
          thenp1    <- thenp.typed(pt)
          elsep1    <- elsep.typed(pt)
          strictEq  <- thenp1.tpe =!= elsep1.tpe
        } yield {
          if !strictEq then {
            import core.Printing.untyped.AstOps._
            val thenpTpeStr = thenp1.tpe.userString
            val elsepTpeStr = elsep1.tpe.userString
            val ast         = If(any,cond,thenp,elsep).toAst
            CompilerError.UnexpectedType(
              s"$thenpTpeStr != $elsepTpeStr in `if` expr:\n$ast")
          } else {
            If(thenp1.tpe, cond1, thenp1, elsep1)
          }
        }
    }

    def typedLet(name: Name, value: Tree, continuation: Tree)(pt: Type): (
      Modal[Checked[Tree]]) = {

        import core.Printing.untyped.AstOps._
        import core.Names.NameOps._
        import TypeOps._

        for {
          value1        <- value.typed(pt) // need to typecheck value1 as being comp type
          continuation1 <- continuation.typed(pt)
          catchAll      <- CompilerError.UnexpectedType(
            s"Can not infer type of `! ${name.userString}` = `${value1.toAst}` as of computation type.")
        } yield Let(continuation1.tpe, name, value1, continuation1)
    }

    def typedCaseExpr(selector: Tree, cases: List[Tree])(pt: Type): (
      Modal[Checked[Tree]]) = {

        import TypeOps._

        def (ts: List[Tree]) mapAsCaseClauses(selTpe: Type, pt: Type): (
          Modal[Checked[List[Tree]]]) = {
            ts.flatMapM({
              case t @ CaseClause(_,_,_,_) => typedCaseClause(t, selTpe, pt)
              case unknown => CompilerError.IllegalState(
                  s"$unknown is not Tree.CaseClause(_,_,_,_)")
            })
          }

        for {
          selector1 <- selector.typed(pt)
          cases1    <- cases.mapAsCaseClauses(selector1.tpe, pt)
          tpe       <- cases1.unifiedTpe
        } yield CaseExpr(tpe, selector1, cases1)
      }

    def typedCaseClause(t: CaseClause, selTpe: Type, pt: Type): (
      Modal[Checked[Tree]]) = t match {
        case CaseClause(_, pat, guard, body) =>

          import TypeOps._

          for {
            pat1    <- pat.typedAsPattern(selTpe)
            // TODO: need to set any variables in context
            guard1  <- guard.typedAsExpr(Bootstraps.BooleanType)
            body1   <- body.typedAsExpr(pt)
          } yield CaseClause(body1.tpe, pat1, guard1, body1)
      }

    def typedUnapply(id: Tree, args: List[Tree])(pt: Type): (
      Modal[Checked[Tree]]) = {

        for {
          id1 <- id.typed(pt)
          args1 <- args.flatMapM(_.typed(pt))
        } yield {
          // need to lookup id1 to get idArgs, if idArgs types match args1 then ok
          import TypeOps._
          Unapply(id1.tpe, id1, args1)
        }
      }

    def typedBind(name: Name, body: Tree)(pt: Type): Modal[Checked[Tree]] = {

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
          Bind(body1.tpe, name, body1)
      }
    }

    def typedAlternative(patterns: List[Tree])(pt: Type): (
      Modal[Checked[Tree]]) = {
        implicit val newMode = Mode.PatAlt
        for {
          patterns1 <- patterns.flatMapM(_.typed(pt))
          tpe       <- patterns1.unifiedTpe
        } yield Alternative(tpe, patterns1)
      }

    def typedPackageDef(pid: Tree, stats: List[Tree])(pt: Type): (
      Modal[Checked[Tree]]) = {
        import TypeOps._
        for {
          pid1    <- pid.typedAsType(any)
          // symbol  <- pid1.defineSymbol
          stats1  <- typedStats(stats /*, symbol */)
        } yield PackageDef(pid1.tpe, pid1, stats1)
      }

    def typedStats(stats: List[Tree] /*, symbol: ??? */): (
      Modal[Checked[List[Tree]]]) =
        stats.flatMapM(_.typed(any))

    def typedDefDef(
      modifiers: Set[Modifier],
      sig: Tree,
      tpeAs: Tree,
      body: Tree)(
      pt: Type): Modal[Checked[Tree]] = {
        import TypeOps._
        for {
          tpeAs1  <- tpeAs.typedAsType(any)
          tpe     <- tpeAs1.tpe
          sig1    <- sig.typed(tpe)
          body1   <- body.typed(tpe)
        } yield DefDef(tpe, modifiers, sig1, tpeAs1, body1)
      }

    def typedDefSig(name: Name, args: List[Name])(pt: Type): (
      Modal[Checked[Tree]]) =
        CompilerError.IllegalState("Function declarations unimplemented")

    def typedSelectType(from: Tree, name: Name)(pt: Type): (
      Modal[Checked[Tree]]) =
        CompilerError.IllegalState("Typed member selections unimplemented")

    def typedSelectTerm(from: Tree, name: Name)(pt: Type): (
      Modal[Checked[Tree]]) =
        CompilerError.SyntaxError("Member selections do not exist for terms.")

    def typedIdentType(name: Name)(pt: Type): Modal[Checked[Tree]] =
      Ident(TypeRef(name), name)

    def typedIdentPat(name: Name)(pt: Type): Modal[Checked[Tree]] =
      Ident(pt, name) // TODO: need to add name to context

    def typedLiteral(constant: Constant): Modal[Checked[Tree]] =
      Literal(constantTpe(constant), constant)
 
    def inner(tree: Tree, pt: Type): Modal[Checked[Tree]] = {
      import Mode._
      tree match {
        /* Type Trees */
        case Select(_,t,n)      if mode == Type   => typedSelectType(t,n)(pt)
        case Ident(_,n)         if mode == Type   => typedIdentType(n)(pt)
        case Apply(_,t,ts)      if mode == Type   => typedApply(t,ts)(pt)
        case Function(_,ts,t)   if mode == Type   => typedFunctionType(ts,t)(pt)
        /* Pattern Trees */
        case Ident(_,n)         if mode.isPattern => typedIdentPat(n)(pt)
        case Unapply(_,t,ts)    if mode.isPattern => typedUnapply(t,ts)(pt)
        case Bind(_,n,t)        if mode.isPattern => typedBind(n,t)(pt)
        case Alternative(_,ts)  if mode.isPattern => typedAlternative(ts)(pt)
        /* Term Trees */
        case PackageDef(_,t,ts) if mode == Term   => typedPackageDef(t,ts)(pt)
        case Apply(_,t,ts)      if mode == Term   => typedApplyTerm(t,ts)(pt)
        case DefDef(_,m,s,t,b)  if mode == Term   => typedDefDef(m,s,t,b)(pt)
        case DefSig(_,n,ns)     if mode == Term   => typedDefSig(n,ns)(pt)
        case If(_,c,t,e)        if mode == Term   => typedIf(c,t,e)(pt)
        case Let(_,n,v,c)       if mode == Term   => typedLet(n,v,c)(pt)
        case Function(_,ts,t)   if mode == Term   => typedFunctionTerm(ts,t)(pt)
        case Tagged(_,n,t)      if mode == Term   => typedTagged(n,t)(pt)
        case CaseExpr(_,t,ts)   if mode == Term   => typedCaseExpr(t,ts)(pt)
        case Select(_,t,n)      if mode == Term   => typedSelectTerm(t,n)(pt)
        /* any mode */
        case Literal(_,c)                         => typedLiteral(c)
        case Parens(_,ts)                         => typedParens(ts)(pt)
        case t @ ( TreeSeq(_)
                 | CaseClause(_,_,_,_)
                 | EmptyTree)                     => t
        /* error case */
        case _ =>
          import core.Printing.untyped.AstOps._
          CompilerError.IllegalState(s"Typing not implemented for <${mode.userString}, ${tree.toAst}>")
      }
    }

    def ignoreType(tree: Tree) = tree match {
      case EmptyTree | TreeSeq(_) => true
      case _ => false
    }

    inner(tree, pt).map {
      typed =>

        import TypeOps._
        import Mode._

        lazy val typedTpe = typed.tpe

        val safe = pt == Types.Type.WildcardType ||
            ignoreType(typed) ||
            pt =!= typedTpe

        if safe then {
          typed
        } else {
          import core.Printing.untyped.AstOps._
          val ast = typed.toAst
          val modeStr = mode.userString
          val ptStr = pt.userString
          val tpeStr = typedTpe.userString
          CompilerError.UnexpectedType(
                s"type $ptStr != $tpeStr in $modeStr:\n$ast")
        }
    }
  }
}