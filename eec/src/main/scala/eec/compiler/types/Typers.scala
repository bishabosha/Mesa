package eec
package compiler
package types

object Typers {

  import Types._
  import Type._
  import core.Names._
  import core.Constants._
  import core.Constants.Constant._
  import core.Modifiers._
  import ast._
  import ast.Trees._
  import Tree._
  import untyped._
  import error.CompilerErrors._
  import CompilerErrorOps._
  import core.Contexts._
  import Context._
  import util.Convert

  private val toType = {
    import implied TypeOps._
    Convert[List[Type], Type]
  }
  
  private val typeToList = {
    import implied TypeOps._
    Convert[Type, List[Type]]
  }

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

  def (tpe: Type) =!= (other: Type): Boolean =
    tpe == other ||
    tpe == Type.WildcardType ||
    other == Type.WildcardType

  def (ts: List[Tree]) unifiedTpe: Checked[Type] =
    if ts.isEmpty then {
      NoType
    } else {
      import implied TypeOps._
      val tpe = ts.head.tpe
      val unified = ts.tail.foldLeft(true)((acc, t) => acc && t.tpe == tpe)
      if unified then
        tpe
      else
        CompilerError.UnexpectedType(s"Types do not unify to ${tpe.userString}")
    }

  def checkFunWithProto(funTyp: Type, proto: Type, pt: Type)
                       (tree: => Tree): Checked[Type] = {

    def getSubstitutions(arg: Type, app: Type): List[(Name, Type)] = arg match {
      case AppliedType(f, args1) => app match {
        case AppliedType(g, args2) if args1.size == args2.size =>
          getSubstitutions(f,g) ::: args1.zip(args2).flatMap(getSubstitutions)
        case _ =>
          Nil
      }
      case FunctionType(a1, b1) => app match {
        case FunctionType(a2, b2) =>
          getSubstitutions(a1,a2) ::: getSubstitutions(b1,b2)
        case _ =>
          Nil
      }
      case Product(tpes1) => app match {
        case Product(tpes2) if tpes1.size == tpes2.size =>
          tpes1.zip(tpes2).flatMap(getSubstitutions)
        case _ =>
          Nil
      }
      case Generic(name) =>
        (name, app) :: Nil
      case _ =>
        Nil
    }

    def unify(sub: Name, by: Type, tpe: Type): Type = tpe match {
      case FunctionType(arg, body) =>
        FunctionType(unify(sub, by, arg), unify(sub, by, body))
      case AppliedType(f, args) =>
        AppliedType(unify(sub, by, f), args.map(unify(sub, by, _)))
      case Product(tpes) =>
        Product(tpes.map(unify(sub, by, _)))
      case Generic(`sub`) =>
        by
      case _ =>
        tpe 
    }

    (funTyp, proto) match {
      case (FunctionType(arg, ret), FunctionType(arg1, ret1)) =>
        val substitutions = getSubstitutions(arg, arg1)
        val FunctionType(arg2, ret2) =
          substitutions.foldLeft(funTyp) { (acc, pair) =>
            val (sub, by) = pair
            unify(sub, by, acc)
          }
        if arg1 =!= arg2 then
            if ret2 =!= ret1 then
              ret2
            else {
              import implied TreeOps._
              import implied TypeOps._
              val retStr  = ret2.userString
              val ret1Str = ret1.userString
              CompilerError.UnexpectedType(
                s"Function Definition type `$retStr` does not match return type `$ret1Str`.")
            }
        else {
          import implied TreeOps._
          import implied TypeOps._
          val arg2Str  = arg2.userString
          val arg1Str = arg1.userString
          val treeStr = tree.userString
          CompilerError.UnexpectedType(
            s"Function definition `${funTyp.userString}` does not match args. Expected `$arg2Str` but was `$arg1Str` in application expr:\n$treeStr\nARG2:$arg2\nARG1:$arg1")
        }
      case _ =>
        CompilerError.IllegalState("Can not apply to non function type")
    }
  }

  def (tree: Tree) typedAsExpr(pt: Type): Contextual[Checked[Tree]] = {
    implied for Mode = Mode.Term
    tree.typed(pt)
  }

  def (tree: Tree) typedAsType(pt: Type): Contextual[Checked[Tree]] = {
    implied for Mode = Mode.Type
    tree.typed(pt)
  }

  def (tree: Tree) typedAsPrimitive(pt: Type): Contextual[Checked[Tree]] = {
    implied for Mode = Mode.PrimitiveType
    tree.typed(pt)
  }

  def (tree: Tree) typedAsPattern(pt: Type): Contextual[Checked[Tree]] = {
    implied for Mode = Mode.Pat
    tree.typed(pt)
  }

  def constantTpe: Constant => Type = {
    case BooleanConstant(_) => Bootstraps.BooleanType
    case BigDecConstant(_)  => Bootstraps.DecimalType
    case BigIntConstant(_)  => Bootstraps.IntegerType
    case CharConstant(_)    => Bootstraps.CharType
    case StringConstant(_)  => Bootstraps.StringType
  }

  def typedFunctionTerm(args: List[Tree], body: Tree)
                       (id: Id, pt: Type): Contextual[Modal[Checked[Tree]]] = {
    def function(args1: List[Tree], body1: Tree): Checked[Tree] = {
      val tpes  = args1.map(_.tpe)
      val fType = tpes.foldRight(body1.tpe) { FunctionType(_,_) }
      if !fType.isComputationType then
        CompilerError.UnexpectedType(
          "Function does not have computational co-domain")
      else
        Function(args1, body1)(id, fType)
    }

    for {
      fCtx  <- ctx.lookIn(id)
      args1 <- args.mapE(_.typed(any) given fCtx)
      body1 <- body.typed(any) given fCtx
      f     <- function(args1, body1)
    } yield f
  }

  def typedFunctionType(args: List[Tree], body: Tree)
                       (id: Id, pt: Type): Contextual[Modal[Checked[Tree]]] = {
    def function(args1: List[Tree], body1: Tree): Checked[Tree] = {
      import Mode._
      val argTpes = toType(args1.map(_.tpe))
      val fType = FunctionType(argTpes, body1.tpe)
      if mode != PrimitiveType && !fType.isComputationType then
        CompilerError.UnexpectedType("Function does not have computational co-domain")
      else
        Function(args1, body1)(id, fType)
    }

    for {
      args1 <- args.mapE(_.typed(any))
      body1 <- body.typed(any)
      f     <- function(args1, body1)
    } yield f
  }

  def typedTagged(arg: Name, tpeTree: Tree)
                 (id: Id, pt: Type): Contextual[Modal[Checked[Tree]]] = 
    for (tpeTree1 <- tpeTree.typedAsType(pt))
      yield {
        ctx.putType(arg -> tpeTree1.tpe)
        Tagged(arg, tpeTree1)(id, tpeTree1.tpe)
      }

  def typedParens(ts: List[Tree])
                 (id: Id, pt: Type): Contextual[Modal[Checked[Tree]]] = {
    def typeAsTuple(ts: List[Tree], pt: Type): Checked[List[Tree]] = {
      if pt == any then
        ts.mapE(_.typed(any))
      else {
        val ptAsTuple = typeToList(pt)
        if ts.length != ptAsTuple.length then
          if ptAsTuple.length == 1 then {
          import implied TypeOps._
          val ptStr = pt.userString
          CompilerError.UnexpectedType(s"expected `$ptStr` but was a Tuple.")
        } else {
          CompilerError.UnexpectedType("Tuple lengths do not match")
        } else {
          ts.zip(ptAsTuple).mapE { _.typed(_) }
        }
      }
    }

    for {
      ts1 <- typeAsTuple(ts, pt)
    } yield {
      val tupleTyp = toType(ts1.map(_.tpe))
      if tupleTyp =!= pt then {
        Parens(ts1)(id, tupleTyp)
      } else {
        import Mode._
        import implied TypeOps._
        import implied TreeOps._
        val tupleTypeStr  = tupleTyp.userString
        val expectedStr   = pt.userString
        val treeStr       = Parens(ts)(Id.noId, NoType).userString
        CompilerError.UnexpectedType(
          s"expected `$expectedStr` but was `$tupleTypeStr` in $mode:\n$treeStr")
      }
    }
  }

  def typedApplyType(functor: Tree, args: List[Tree])
                    (id: Id, pt: Type): Contextual[Modal[Checked[Tree]]] = {
      /*TODO: type application for generic functors
        form type prototype by typing args and then forming typeTpe:
          [...args1] => pt

        then type functor and retrieve its symbol
        check tpe matches prototype
      */
      for {
        functor1  <- functor.typed(any)
        args1     <- args.mapE(_.typed(any))
      } yield {
        val selTpe  = functor1.tpe
        val argTpes = args1.map(_.tpe)
        val tpe     = AppliedType(selTpe, argTpes)
        Apply(functor1, args1)(id, tpe)
      }
    }

  def typedApplyTerm(fun: Tree, args: List[Tree])
                    (id: Id, pt: Type): Contextual[Modal[Checked[Tree]]] = {
    for {
      args1     <-  args.mapE(_.typed(any))
      funProto  <-  FunctionType(toType(args1.map(_.tpe)), pt)
      fun1      <-  fun.typed(any)
      tpe       <-  checkFunWithProto(fun1.tpe, funProto, pt)(
                      Apply(fun, args)(Id.noId, Type.NoType))
    } yield Apply(fun1, args1)(id, tpe)
  }

  def typedLet(letId: Tree, value: Tree, continuation: Tree)
              (id: Id, pt: Type): Contextual[Modal[Checked[Tree]]] = {
    import implied TreeOps._
    import implied core.Names.NameOps._

    val Ident(name) = letId

    def unwrapCompApply(tpe: Type): Checked[Type] = tpe match {
      case AppliedType(TypeRef(Name.ComputationTag), List(t)) =>
        t
      case _ =>
        CompilerError.UnexpectedType(s"Can not infer type of `!${name.userString} = ${value.userString}` as of ! type.")
    }

    def contIsComp(tpe: Type): Checked[Type] =
      if tpe.isComputationType then
        tpe
      else
        CompilerError.UnexpectedType("Let body does not have computation type.")

    for {
      lCtx          <- ctx.lookIn(id)
      value1        <- value.typed(any) // need to typecheck value1 as being comp type
      value1Inner   <- unwrapCompApply(value1.tpe)
      letId1        <- Ident(name)(letId.id, value1Inner)
      _             <- lCtx.putType(name -> value1Inner)
      continuation1 <- continuation.typed(pt) given lCtx
      tpe           <- contIsComp(continuation1.tpe)
    } yield Let(letId1, value1, continuation1)(id, tpe)
  }

  def typedCaseExpr(selector: Tree, cases: List[Tree])
                   (id: Id, pt: Type): Contextual[Modal[Checked[Tree]]] = {
    def (ts: List[Tree]) mapAsCaseClauses(selTpe: Type)(
      pt: Type): Contextual[Modal[Checked[List[Tree]]]] = {
        ts.mapE({
          case t @ CaseClause(p,g,b) =>
            typedCaseClause(p, g, b, selTpe)(t.id, pt)
          case unknown =>
            CompilerError.IllegalState(
              s"$unknown is not Tree.CaseClause(_,_,_,_)")
        })
      }

    def selectorTpe(selector: Tree): Type = selector.tpe match {
      case UntypedExpect(t) => t
      case _                => any
    }

    for {
      selector1 <- selector.typed(selectorTpe(selector))
      cases1    <- cases.mapAsCaseClauses(selector1.tpe)(pt)
      tpe       <- cases1.unifiedTpe
    } yield CaseExpr(selector1, cases1)(id, tpe)
  }

  def typedCaseClause(pat: Tree, guard: Tree, body: Tree, selTpe: Type)
                     (id: Id, pt: Type): Contextual[Modal[Checked[Tree]]] =
    for {
      ccCtx   <- ctx.lookIn(id)
      pat1    <- pat.typedAsPattern(selTpe) given ccCtx
      guard1  <- guard.typedAsExpr(Bootstraps.BooleanType) given ccCtx
      body1   <- body.typedAsExpr(pt) given ccCtx
    } yield CaseClause(pat1, guard1, body1)(id, body1.tpe)

  // def typedUnapply(f: Tree, args: List[Tree])
  //                 (id: Id, pt: Type): Contextual[Modal[Checked[Tree]]] = {
  //     import TypeOps._

  //     def (f: Tree) typedAsFunctor(pt: Type): Checked[Tree] = pt match {
  //       case AppliedType(TypeRef(name), args) =>
  //         for {
  //           nameCtx <- ctx.firstCtx(name)
  //           fTpe  <- nameCtx.getType(name) // lookup type for name
  //           f1    <- f.typedAsExpr(fTpe) // fTpe should be type lambda
  //         } yield f1
  //       case _ =>
  //         CompilerError.UnexpectedType(s"expected Functor type, but was ${pt.userString}")
  //     }

  //     def (ts: List[Tree]) typedAsFunctorArgs(fTpe: Type, pt: Type): Checked[List[Tree]] = fTpe match {
  //       case FunctionType(WildcardType, AppliedType(TypeRef(name), args)) =>
  //         pt match {
  //           case AppliedType(TypeRef(`name`), ptArgs) =>
  //             if ptArgs.length != args.length then
  //               CompilerError.UnexpectedType("Functor type does not unify with expected type")
  //             else if ts.length != args.length then
  //               CompilerError.UnexpectedType("Functor args lengths do not match")
  //             else {
  //               ts.zip(ptArgs).mapE { _.typed(_) }
  //             }
  //           case _ =>
  //             CompilerError.IllegalState("Unapply does not match Functor type.")
  //         }
  //       case _ =>
  //         CompilerError.IllegalState("Functor type which is not single arg.")
  //     }

  //     def unifyUnapply(fType: Type, args: List[Type]): Checked[Type] = fType match {
  //       case FunctionType(WildcardType, AppliedType(TypeRef(name), _)) =>
  //         AppliedType(TypeRef(name), args)
  //       case _ =>
  //         CompilerError.IllegalState("Functor type which is not single arg.")
  //     }

  //     for {
  //       f1    <- f.typedAsFunctor(pt)
  //       args1 <- args.typedAsFunctorArgs(f1.tpe, pt)
  //       tpe   <- unifyUnapply(f1.tpe, args1.map(_.tpe))
  //     } yield
  //       Unapply(f1, args1)(id, tpe) // need to lookup id1 to get idArgs, if idArgs types match args1 then ok
  //   }

  def typedBind(name: Name, body: Tree)
               (id: Id, pt: Type): Contextual[Modal[Checked[Tree]]] = {
    import Mode._
    for {
      body1 <- body.typed(pt)
    } yield {
      if name == Name.Wildcard then
        body1
      else if mode == PatAlt then
        CompilerError.IllegalState(
          s"Illegal variable ${name} in pattern alternative")
      else
        ctx.putType(name -> body1.tpe)
        Bind(name, body1)(id, body1.tpe)
    }
  }

  def typedAlternative(patterns: List[Tree])
                      (id: Id, pt: Type): Contextual[Modal[Checked[Tree]]] = {
    implied for Mode = Mode.PatAlt
    for {
      patterns1 <- patterns.mapE(_.typed(pt))
      tpe       <- patterns1.unifiedTpe
    } yield Alternative(patterns1)(id, tpe)
  }

  def typedPackageDef(pid: Tree, stats: List[Tree])
                     (id: Id, pt: Type): Contextual[Modal[Checked[Tree]]] = {
    import TypeOps._
    import implied TreeOps._
    import Name._

    val toIdNamePairs = Convert[Tree, List[(Id, Name)]]

    def typePackaging(tree: Tree): Checked[(Context, Tree)] =
      toIdNamePairs(tree) match {
        case (id, name) :: tail =>
          for {
            next  <- ctx.lookIn(id)
            tpe   <- ctx.declarePackage(From(rootString), name)
          } yield (
            tail.foldLeftE(next, Ident(name)(id, tpe)) { (acc, pair) =>
              val (current, parent) = acc
              val (id, name) = pair
              for {
                next  <- current.lookIn(id)
                tpe   <- current.declarePackage(packageName(parent.tpe), name)
              } yield {
                (next, Select(parent, name)(id, tpe))
              }
            }
          )
        case _ =>
          (ctx, EmptyTree)
      }

    for {
      pair    <- typePackaging(pid)
      pkgCtx  <- pair._1
      pid1    <- pair._2
      stats1  <- stats.mapE(_.typed(any) given pkgCtx)
    } yield {
      PackageDef(pid1, stats1)(id, pid1.tpe)
    }
  }

  def typedDefDef(modifiers: Set[Modifier], sig: Tree, tpeAs: Tree, body: Tree)
                 (id: Id, pt: Type): Contextual[Modal[Checked[Tree]]] = {
    import TypeOps._
    import implied TreeOps._
    val getName = Convert[Tree, Name]
    val typeTpeAs =
      if modifiers.contains(Modifier.Primitive) then
        typedAsPrimitive
      else
        typedAsType
    val tpd = for {
      tpeAs1  <- typeTpeAs(tpeAs)(any)
      tpe     <- tpeAs1.tpe
      sig1    <- sig.typed(tpe)
      bodyCtx <- ctx.lookIn(sig1.id)
      body1   <- body.typed(toReturnType(sig1, tpe)) given bodyCtx
      _       <- ctx.putType(getName(sig), tpe)
    } yield DefDef(modifiers, sig1, tpeAs1, body1)(id, tpe)
    Context.commitId(sig.id)
    tpd
  }

  def typedDefSig(name: Name, args: List[Tree])
                 (id: Id, pt: Type): Contextual[Modal[Checked[Tree]]] = {
      import TypeOps._
      import implied TreeOps._
      import implied NameOps._

      def mapArgs(
        args: List[Tree], pts: List[Type]): Contextual[Checked[List[Tree]]] =
          args
            .flatMap(Convert[Tree, List[(Id, Name)]])
            .zip(pts)
            .mapE { (pair, tpe) =>
              val (id, name) = pair
              for {
                nameCtx <- ctx.lookIn(id)
              } yield {
                ctx.putType(name, tpe)
                Ident(name)(id, tpe)
              }
            }

      val pts = toCurriedList(pt)

      if pts.length <= args.length then
        CompilerError.UnexpectedType(
          s"Function declaration arguments do not match declared type for declaration ${name.userString}. REF: $args; $pts")
      else
        for {
          bodyCtx   <- ctx.lookIn(id)
          args1     <- mapArgs(args, pts) given bodyCtx
        } yield DefSig(name, args1)(id, pt)
    }

  def typedSelectType(from: Tree, name: Name)
                     (id: Id, pt: Type): Contextual[Modal[Checked[Tree]]] =
    CompilerError.SyntaxError("Member selections do not exist for types.")

  def typedSelectTerm(from: Tree, name: Name)
                     (id: Id, pt: Type): Contextual[Modal[Checked[Tree]]] =
    CompilerError.SyntaxError("Member selections do not exist for terms.")

  def typedIdentType(name: Name)
                    (id: Id, pt: Type): Contextual[Modal[Checked[Tree]]] = {
    val lookup = ctx.firstCtx(name).flatMap(_.getType(name))
    val tpe = lookup.fold { notFound => Generic(name) }{ tpe =>
      if Context.isDefined(tpe) then
        TypeRef(name)
      else
        Generic(name)
    }
    Ident(name)(id, tpe)
  }

  def typedIdentPat(name: Name)
                   (id: Id, pt: Type): Contextual[Modal[Checked[Tree]]] = {
    import Mode._
    import Name._
    if mode == PatAlt && name != Wildcard then
      CompilerError.SyntaxError("Illegal variable in pattern alternative")
    else {
      if name != Wildcard then {
        ctx.putType(name -> pt)
      }
      Ident(name)(id, pt)
    }
  }

  def typedIdentTerm(name: Name)
                    (id: Id, pt: Type): Contextual[Modal[Checked[Tree]]] = {
      for {
        fstCtx    <- ctx.firstCtx(name)
        idRefTpe  <- fstCtx.getType(name)
      } yield {
        if idRefTpe =!= pt then {
          Ident(name)(id, idRefTpe)
        } else {
          import implied NameOps._
          import implied TypeOps._
          val nameStr     = name.userString
          val idRefTpeStr = idRefTpe.userString
          val ptStr       = pt.userString
          CompilerError.UnexpectedType(
            s"Expected type for identifier $nameStr of `$ptStr` but was `$idRefTpe`")
        }
      }
    }

  def typedLiteral(constant: Constant)
                  (id: Id): Contextual[Modal[Checked[Tree]]] =
    Literal(constant)(id, constantTpe(constant))

  def check(typed: Tree)(pt: Type): Contextual[Modal[Checked[Tree]]] = {
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
        import implied TypeOps._
        import implied TreeOps._
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
    def inner(tree: Tree, pt: Type) = tree match {
      /* Type Trees */
      case Select(t,n)      if mode.isType    => typedSelectType(t,n)(tree.id, pt)
      case Ident(n)         if mode.isType    => typedIdentType(n)(tree.id, pt)
      case Apply(t,ts)      if mode.isType    => typedApplyType(t,ts)(tree.id, pt)
      case Function(ts,t)   if mode.isType    => typedFunctionType(ts,t)(tree.id, pt)
      /* Pattern Trees */
      case Ident(n)         if mode.isPattern => typedIdentPat(n)(tree.id, pt)
      // case Unapply(t,ts)    if mode.isPattern => typedUnapply(t,ts)(tree.id, pt)
      case Bind(n,t)        if mode.isPattern => typedBind(n,t)(tree.id, pt)
      case Alternative(ts)  if mode.isPattern => typedAlternative(ts)(tree.id, pt)
      /* Term Trees */
      case PackageDef(t,ts) if mode.isTerm    => typedPackageDef(t,ts)(tree.id, pt)
      case Apply(t,ts)      if mode.isTerm    => typedApplyTerm(t,ts)(tree.id, pt)
      case DefDef(m,s,t,b)  if mode.isTerm    => typedDefDef(m,s,t,b)(tree.id, pt)
      case DefSig(n,ns)     if mode.isTerm    => typedDefSig(n,ns)(tree.id, pt)
      case Let(n,v,c)       if mode.isTerm    => typedLet(n,v,c)(tree.id, pt)
      case Function(ts,t)   if mode.isTerm    => typedFunctionTerm(ts,t)(tree.id, pt)
      case Tagged(n,t)      if mode.isTerm    => typedTagged(n,t)(tree.id, pt)
      case CaseExpr(t,ts)   if mode.isTerm    => typedCaseExpr(t,ts)(tree.id, pt)
      case Select(t,n)      if mode.isTerm    => typedSelectTerm(t,n)(tree.id, pt)
      case Ident(n)         if mode.isTerm    => typedIdentTerm(n)(tree.id, pt)
      /* any mode */
      case Literal(c)                         => typedLiteral(c)(tree.id)
      case Parens(ts)                         => typedParens(ts)(tree.id, pt)
      case t @ ( TreeSeq(_)
               | CaseClause(_,_,_)
               | EmptyTree)                   => t
      /* error case */
      case _ =>
        import implied TreeOps._
        CompilerError.IllegalState(
          s"Typing not implemented for <${mode.userString}, ${tree.userString}>")
    }
    inner(tree, pt).map(check(_)(pt))
  }
}