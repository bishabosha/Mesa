package eec
package compiler
package types

object Typers {

  import Types._
  import Types.Type._
  import core.Names.Name._
  import core.Constants._
  import core.Constants.Constant._
  import ast.Trees._
  import ast.Trees.Tree._
  import ast.Trees.untyped._
  import error.CompilerErrors._

  enum Mode {

    case Type, Term, Pat, PatAlt

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

  def (tpe: Type) =!= (other: Type): Boolean = tpe == other

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

  def (tree: Tree) typed(pt: Type): Modal[Checked[Tree]] = {

    def constantTpe: Constant => Type = {
      case BooleanConstant(_) => Bootstraps.BooleanType
      case BigDecConstant(_)  => Bootstraps.DecimalType
      case BigIntConstant(_)  => Bootstraps.IntegerType
      case CharConstant(_)    => Bootstraps.CharType
      case StringConstant(_)  => Bootstraps.StringType
    }

    def typedFunctionTerm(f: Function, pt: Type): Modal[Checked[Tree]] = {
      def function(args1: List[Tree], body1: Tree) = {
        import TypeOps._
        val tpes  = args1.map(_.tpe)
        val fType = tpes.foldRight(body1.tpe) { FunctionType(_,_) }
        Function(fType, args1, body1)
      }
      f match {
        case Function(_, args, body) =>
          import CompilerError._
          for {
            args1 <- args.flatMapM(_.typed(pt))
            body1 <- body.typed(pt)
          } yield function(args1, body1)
      }
    }

    def typedFunctionType(f: Function, pt: Type): Modal[Checked[Tree]] = {
      def function(args1: List[Tree], body1: Tree) = {
        import TypeOps._
        val tpe =
          if args1.length == 1 then
            args1.head.tpe
          else
            Product(args1.map(_.tpe))
        val fType = FunctionType(tpe, body1.tpe)
        Function(fType, args1, body1)
      }
      f match {
        case Function(_, args, body) =>
          import CompilerError._
          for {
            args1 <- args.flatMapM(_.typed(pt))
            body1 <- body.typed(pt)
          } yield function(args1, body1)
      }
    }

    def typedTagged(t: Tagged, pt: Type): Modal[Checked[Tree]] = t match {
      case Tagged(_, arg, tpeTree) =>
        import TypeOps._
        import CompilerError._
        tpeTree.typedAsType(pt).map { tpeTree1 =>
          Tagged(tpeTree1.tpe, arg, tpeTree1)
        }
    }

    def typedParens(t: Parens, pt: Type): Modal[Checked[Tree]] = t match {
      case Parens(_, List()) =>
        val tpe = Product(List())
        Parens(tpe, List())
      case Parens(_, es) =>
        import CompilerError._
        import TypeOps._
        es.flatMapM(_.typed(pt)).map { es1 =>
          val tpes = es1.map(_.tpe)
          val tupleTyp = Product(tpes)
          Parens(tupleTyp, es1)
        }
    }

    def typedApply(t: Apply, pt: Type): Modal[Checked[Tree]] = t match {
      case Apply(_, e, es) =>
        import CompilerError._
        for {
          e1  <- e.typed(pt)
          es1 <- es.flatMapM(_.typed(pt))
        } yield {
          import TypeOps._
          val selTpe = e1.tpe
          val argTpes = es1.map(_.tpe)
          val tpe = AppliedType(selTpe, argTpes)
          Apply(tpe, e1, es1)
        }
    }

    def typedIf(t: If, pt: Type): Modal[Checked[Tree]] = t match {
      case If(_, cond, thenp, elsep) =>
        import CompilerError._
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
            val ast         = t.toAst
            CompilerError.UnexpectedType(
              s"$thenpTpeStr != $elsepTpeStr in `if` expr:\n$ast")
          } else {
            If(thenp1.tpe, cond1, thenp1, elsep1)
          }
        }
    }

    def typedLet(t: Let, pt: Type): Modal[Checked[Tree]] = t match {
      case Let(_, name, value, continuation) =>
        import CompilerError._
        import core.Printing.untyped.AstOps._
        import core.Names.NameOps._
        for {
          value1 <- value.typed(pt) // need to typecheck value1 as being comp type
          continuation1 <- continuation.typed(pt)
          catchAll <- CompilerError.UnexpectedType(
            s"Can not infer type of `! ${name.userString}` = `${value1.toAst}` as of computation type.")
        } yield ??? // Let(continuation1.tpe, name, value1, continuation1)
    }

    def typedCaseExpr(t: CaseExpr, pt: Type): Modal[Checked[Tree]] = {
      import CompilerError._
      def (ts: List[Tree]) mapAsCaseClauses(selTpe: Type, pt: Type): (
        Modal[Checked[List[Tree]]]) = {
          ts.flatMapM({
            case t @ CaseClause(_,_,_,_) => typedCaseClause(t, selTpe, pt)
            case unknown => CompilerError.IllegalState(
                s"$unknown is not Tree.CaseClause(_,_,_,_)")
          })
        }
      t match {
        case CaseExpr(_, selector, cases) =>
          import TypeOps._
          for {
            selector1 <- selector.typed(pt)
            cases1    <- cases.mapAsCaseClauses(selector1.tpe, pt)
            tpe       <- cases1.unifiedTpe
          } yield CaseExpr(tpe, selector1, cases1)
      }
    }

    def typedCaseClause(t: CaseClause, selTpe: Type, pt: Type): (
      Modal[Checked[Tree]]) = t match {
        case CaseClause(_, pat, guard, body) =>
          import CompilerError._
          import TypeOps._
          for {
            pat1    <- pat.typedAsPattern(selTpe)
            // TODO: need to set any variables in context
            guard1  <- guard.typedAsExpr(Bootstraps.BooleanType)
            body1   <- body.typedAsExpr(pt)
          } yield CaseClause(body1.tpe, pat1, guard1, body1)
      }

    def typedUnapply(t: Unapply, pt: Type): Modal[Checked[Tree]] = t match {
      case Unapply(_, id, args) =>
        import CompilerError._
        for {
          id1 <- id.typed(pt)
          args1 <- args.flatMapM(_.typed(pt))
        } yield {
          import TypeOps._
          // need to lookup id1 to get idArgs, if idArgs types match args1 then ok
          Unapply(id1.tpe, id1, args1)
        }
    }

    def typedBind(t: Bind, pt: Type): Modal[Checked[Tree]] = t match {
      case Bind(_, name, body) =>
        import CompilerError._
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

    def typedAlternative(t: Alternative, pt: Type): Modal[Checked[Tree]] =
      t match {
        case Alternative(_, patterns) =>
          import CompilerError._
          implicit val newMode = Mode.PatAlt
          for {
            patterns1 <- patterns.flatMapM(_.typed(pt))
            tpe       <- patterns1.unifiedTpe
          } yield Alternative(tpe, patterns1)
      }
 
    def inner(tree: Tree, pt: Type): Modal[Checked[Tree]] = {
      import Mode._
      val any: core.Names.Name = Wildcard
      tree match {
        /* Type Trees */
        case _ @ Ident(_, name)   if mode == Type     => Ident(TypeRef(name), name)
        case t @ Apply(_,_,_)     if mode == Type     => typedApply(t, pt)
        case t @ Function(_,_,_)  if mode == Type     => typedFunctionType(t, pt)
        /* Pattern Trees */
        case _ @ Ident(_, name)   if mode.isPattern   => Ident(pt, name) // TODO: need to add name to context
        case t @ Unapply(_,_,_)   if mode.isPattern   => typedUnapply(t, pt)
        case t @ Bind(_,_,_)      if mode.isPattern   => typedBind(t, pt)
        case t @ Alternative(_,_) if mode.isPattern   => typedAlternative(t, pt)
        /* Term Trees */
        case t @ If(_,_,_,_)      if mode == Term     => typedIf(t, pt)
        case t @ Let(_,_,_,_)     if mode == Term     => typedLet(t, pt)
        case t @ Function(_,_,_)  if mode == Term     => typedFunctionTerm(t, pt)
        case t @ Tagged(_,_,_)    if mode == Term     => typedTagged(t, pt)
        case t @ CaseExpr(_,_,_)  if mode == Term     => typedCaseExpr(t, pt)
        /* any mode */
        case _ @ Literal(_,c)                         => Literal(constantTpe(c), c)
        case t @ Parens(_,_)                          => typedParens(t, pt)
        case t @ ( TreeSeq(_)
                 | CaseClause(_,_,_,_)
                 | EmptyTree)                         => t
        /* error case */
        case _ =>
          import core.Printing.untyped.AstOps._
          CompilerError.UnexpectedType(s"Type not implemented for <${mode.userString}, ${tree.toAst}>")
      }
    }

    def ignoreType(tree: Tree) = tree match {
      case EmptyTree | TreeSeq(_) => true
      case _ => false
    }

    import CompilerError._
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