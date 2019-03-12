package eec
package compiler
package types

object Typers {

  import Types._
  import Type._
  import core.Names._
  import core.Constants._
  import core.Modifiers._
  import ast._
  import ast.Trees._
  import Tree._
  import error.CompilerErrors._
  import CompilerErrorOps._
  import core.Contexts._
  import Context._
  import Mode._

  private val toType = {
    import implied TypeOps._
    import util.Convert
    Convert[List[Type], Type]
  }
  
  private val typeToList = {
    import implied TypeOps._
    import util.Convert
    Convert[Type, List[Type]]
  }

  private val getName = {
    import implied TreeOps._
    import util.Convert
    Convert[Tree, Name]
  }

  private[Typers] val any = WildcardType

  def (tpe: Type) isComputationType: Boolean = {
    import Name._
    tpe match {
      case AppliedType(TypeRef(ComputationTag), List(_)) =>
        true
      case FunctionType(_, tpe) =>
        tpe.isComputationType
      case Product(ts) =>
        ts.forall(isComputationType)
      case _ =>
        false
    }
  }

  def (ts: List[Tree]) unifiedTpe: Checked[Type] =
    if ts.isEmpty then {
      EmptyType
    } else {
      import implied TypeOps._
      val tpe = ts.head.tpe
      val unified = ts.tail.foldLeft(true)((acc, t) => acc && t.tpe == tpe)
      if unified then
        tpe
      else
        CompilerError.UnexpectedType(
          s"Types do not unify to ${tpe.userString}")
    }

  def resolveVariables(tpe: Type) given Context: Type = tpe match {
    case FunctionType(arg, body) =>
      FunctionType(resolveVariables(arg), resolveVariables(body))
    case AppliedType(f, args) =>
      AppliedType(resolveVariables(f), args.map(resolveVariables))
    case Product(tpes) =>
      Product(tpes.map(resolveVariables))
    case TypeRef(name) =>
      val lookup = firstCtx(name).flatMap { ctx1 =>
        implied for Context = ctx1
        getType(name)
      }
      lookup.fold
        { notFound => Variable(name) }
        { tpe =>
          if isDefined(tpe) then
            TypeRef(name)
          else
            Variable(name)
        }
    case _ =>
      tpe
  }

  def checkFunctorWithProto(functorTyp: Type, proto: Type): Checked[Type] = {
    import TypeOps._
    (functorTyp, proto) match {
      case (FunctionType(arg, ret), FunctionType(arg1, ret1)) =>
        val substitutions = getSubstitutions(functorTyp, proto)
        val functorTyp1 =
          substitutions.foldLeft(functorTyp) { (acc, pair) =>
            val (sub, by) = pair
            unify(sub, by, acc)
          }
        val proto1 =
          substitutions.foldLeft(proto) { (acc, pair) =>
            val (sub, by) = pair
            unify(sub, by, acc)
          }
        if functorTyp1 =!= proto1 then
            TypeOps.toCurriedList(functorTyp1).last
        else {
          import implied TreeOps._
          import implied TypeOps._
          val functorTyp1Str  = functorTyp1.userString
          val proto1Str       = proto1.userString
          CompilerError.UnexpectedType(
            s"Functor definition `${functorTyp.userString}` does not match args. Expected `$functorTyp1Str` but was `$proto1Str` in application expr.")
        }
      case _ =>
        CompilerError.IllegalState(s"Can not apply to non function type.")
    }
  }

  def checkFunWithProto(funTyp: Type, proto: Type): Checked[Type] = {
    import TypeOps._
    (funTyp, proto) match {
      case (FunctionType(arg, ret), FunctionType(arg1, ret1)) =>
        val substitutions = getSubstitutions(funTyp, proto)
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
              val ret2Str = ret2.userString
              val ret1Str = ret1.userString
              CompilerError.UnexpectedType(
                s"Function Definition type `$ret2Str` does not match return type `$ret1Str`.")
            }
        else {
          import implied TreeOps._
          import implied TypeOps._
          val arg2Str  = arg2.userString
          val arg1Str = arg1.userString
          CompilerError.UnexpectedType(
            s"Function definition `${funTyp.userString}` does not match args. Expected `$arg2Str` but was `$arg1Str` in application expr.")
        }
      case _ =>
        CompilerError.IllegalState(s"Can not apply to non function type.")
    }
  }

  def (tree: Tree) typedAsExpr(pt: Type) given Context: Checked[Tree] = {
    implied for Mode = Term
    tree.typed(pt)
  }

  def (tree: Tree) typedAsTyping(pt: Type) given Context: Checked[Tree] = {
    implied for Mode = Typing
    tree.typed(pt)
  }

  def (tree: Tree) typedAsPrimitive(pt: Type) given Context: Checked[Tree] = {
    implied for Mode = PrimitiveType
    tree.typed(pt)
  }

  def (tree: Tree) typedAsPattern(pt: Type) given Context: Checked[Tree] = {
    implied for Mode = Pat
    tree.typed(pt)
  }

  def constantTpe(c: Constant): Type = {
    import Constant._
    c match {
      case BooleanConstant(_) => Bootstraps.BooleanType
      case BigDecConstant(_)  => Bootstraps.DecimalType
      case BigIntConstant(_)  => Bootstraps.IntegerType
      case CharConstant(_)    => Bootstraps.CharType
      case StringConstant(_)  => Bootstraps.StringType
    }
  }

  private def functionTermTpe(args1: List[Tree], body1: Tree): Checked[Type] = {
    val tpes  = args1.map(_.tpe)
    val fType = tpes.foldRight(body1.tpe) { FunctionType(_,_) }
    if !fType.isComputationType then
      CompilerError.UnexpectedType(
        "Function does not have computational co-domain")
    else
      fType
  }

  def typedFunctionTerm(args: List[Tree], body: Tree)
                       (id: Id, pt: Type) given Context, Mode: Checked[Tree] =
    lookIn(id).flatMap { fCtx =>
      implied for Context = fCtx
      for {
        args1 <-  args.mapE(_.typed(any))
        body1 <-  body.typed(any)
        fTpe  <-  functionTermTpe(args1, body1)
      } yield Function(args1, body1)(id, fTpe)
    }

  private def functionTypeTpe(args1: List[Tree], body1: Tree)
                          given Mode: Checked[Type] = {
    val argTpes = toType(args1.map(_.tpe))
    val fType   = FunctionType(argTpes, body1.tpe)
    if mode != PrimitiveType && !fType.isComputationType then
      CompilerError.UnexpectedType(
        "Function does not have computational co-domain")
    else
      fType
  }

  def typedFunctionType(args: List[Tree], body: Tree)
                       (id: Id, pt: Type) given Context, Mode: Checked[Tree] =
    for {
      args1 <- args.mapE(_.typed(any))
      body1 <- body.typed(any)
      fTpe  <- functionTypeTpe(args1, body1)
    } yield Function(args1, body1)(id, fTpe)

  def typedTagged(arg: Name, tpeTree: Tree)
                 (id: Id, pt: Type) given Context, Mode: Checked[Tree] = 
    for (tpeTree1 <- tpeTree.typedAsTyping(pt))
      yield {
        putType(arg -> tpeTree1.tpe)
        Tagged(arg, tpeTree1)(id, tpeTree1.tpe)
      }

  private def typeAsTuple(ts: List[Tree], pt: Type)
                          given Context, Mode: Checked[List[Tree]] =
    if pt == any then
      ts.mapE(_.typed(any))
    else {
      val ptAsTuple = typeToList(pt)
      if ts.length != ptAsTuple.length then {
        if ptAsTuple.length == 1 then {
          import implied TypeOps._
          CompilerError.UnexpectedType(
            s"expected `${pt.userString}` but was a Tuple.")
        } else {
          CompilerError.UnexpectedType("Tuple lengths do not match")
        }
      } else {
        ts.zip(ptAsTuple).mapE(_.typed(_))
      }
    }

  def typedParens(ts: List[Tree])
                 (id: Id, pt: Type) given Context, Mode: Checked[Tree] = {
    import TypeOps._
    for (ts1 <- typeAsTuple(ts, pt))
      yield {
        val tupleTyp = toType(ts1.map(_.tpe))
        if tupleTyp =!= pt then {
          Parens(ts1)(id, tupleTyp)
        } else {
          import implied TypeOps._
          import implied TreeOps._
          val tupleTypeStr  = tupleTyp.userString
          val expectedStr   = pt.userString
          val treeStr       = Parens(ts)(Id.noId, EmptyType).userString
          CompilerError.UnexpectedType(
            s"expected `$expectedStr` but was `$tupleTypeStr` in $mode:\n$treeStr")
        }
      }
  }

  private def typeAsDestructor(fTpe: Type, pt: Type): Checked[(List[Type], Type)] = {
    import TypeOps._
    import implied TypeOps._
    toCurriedList(fTpe).reverse match {
      case ret0 :: args0 =>
        val subs      = getSubstitutions(ret0, pt)
        val ret       = replace(ret0, subs)
        val fTpeArgs  = args0.reverse.map(replace(_, subs))
        (fTpeArgs, ret)
      case _ =>
        CompilerError.UnexpectedType(s"Empty Functor Type ${fTpe.userString}")
    }
  }

  private def typeAsFunctorArgs
      (functor: Name, ts: List[Tree], fTpeArgs: List[Type])
      given Context, Mode: Checked[List[Tree]] = {
    import TypeOps._
    if ts.length != fTpeArgs.length then
      CompilerError.UnexpectedType("arg lengths do not match")
    else
      ts.zip(fTpeArgs).mapE(_.typed(_))
  }

  private def unifyConstructorToHkType
      (constructor: Type, args: List[Type]): Checked[Type] = {
    import TypeOps._
    val res :: args0 = toCurriedList(constructor).reverse
    res match {
      case _ if args0.length == args.length =>
        res
      case _ =>
        import implied TypeOps._
        val argsOrdered = args0.reverse
        val argsExpect  = argsOrdered.map(_.userString).mkString("[", ", ", "]")
        val argsPassed  = args.map(_.userString).mkString("[", ", ", "]")
        val hint =
          if args0.length == 0 then
            " Perhaps functor type is unknown."
          else
            ""
        CompilerError.UnexpectedType(
          s"HK args do not match. Expected $argsExpect but got $argsPassed.$hint")
    }
  }

  def typedApplyType(functor: Tree, args: List[Tree])
                    (id: Id, pt: Type) given Context, Mode: Checked[Tree] =
    for {
      functor1  <-  functor.typed(any)
      args1     <-  args.mapE(_.typed(any))
      applied   <-  unifyConstructorToHkType(functor1.tpe, args1.map(_.tpe))
      funProto  <-  TypeOps.toFunctionType(args1.map(_.tpe) :+ applied)
      tpe       <-  checkFunctorWithProto(functor1.tpe, funProto)
    } yield Apply(functor1, args1)(id, tpe)

  def typedApplyTerm(fun: Tree, args: List[Tree])
                    (id: Id, pt: Type) given Context, Mode: Checked[Tree] =
    for {
      fun1      <-  fun.typed(any)
      args1     <-  args.mapE(_.typed(any))
      funProto  <-  FunctionType(toType(args1.map(_.tpe)), pt)
      tpe       <-  checkFunWithProto(fun1.tpe, funProto)
    } yield Apply(fun1, args1)(id, tpe)

  private def unwrapCompApply(tpe: Type)
                             (name: Name, value: Tree): Checked[Type] = {
    import Name._
    import implied TreeOps._
    import implied core.Names.NameOps._
    tpe match {
      case AppliedType(TypeRef(ComputationTag), List(t)) =>
        t
      case _ =>
        CompilerError.UnexpectedType(
          s"Can not infer type of `!${name.userString} = ${value.userString}` as of ! type.")
    }
  }

  def typedLet(name: Name, nId: Id)(value: Tree, cont: Tree)
              (id: Id, pt: Type) given Context, Mode: Checked[Tree] =
    for {
      lCtx    <-  lookIn(id)
      value1  <-  value.typed(any)
      value1U <-  unwrapCompApply(value1.tpe)(name, value)
      letId1  <-  Ident(name)(nId, value1U)
      cont1   <-  checked {
                    implied for Context = lCtx
                    putType(name -> value1U)
                    cont.typed(pt)
                        .filter(_.tpe.isComputationType) { _ =>
                          CompilerError.UnexpectedType(
                            "Let body does not have computation type.")
                        }
                  }
    } yield Let(letId1, value1, cont1)(id, cont1.tpe)

  def typeAsCaseClauses(ts: List[Tree], selTpe: Type)
                       (pt: Type) given Context, Mode: Checked[List[Tree]] =
    ts.mapE {
      case t @ CaseClause(p,g,b) =>
        typedCaseClause(p, g, b, selTpe)(t.id, pt)
      case unknown =>
        CompilerError.IllegalState(
          s"$unknown is not Tree.CaseClause(_,_,_,_)")
    }

  def typedCaseExpr(selector: Tree, cases: List[Tree])
                   (id: Id, pt: Type) given Context, Mode: Checked[Tree] = {
    import TypeOps._
    for {
      selector1 <- selector.typed(untypedToChecked(selector.tpe))
      cases1    <- typeAsCaseClauses(cases, selector1.tpe)(pt)
      tpe       <- cases1.unifiedTpe
    } yield CaseExpr(selector1, cases1)(id, tpe)
  }

  def typedCaseClause(pat: Tree, guard: Tree, body: Tree, selTpe: Type)
                     (id: Id, pt: Type) given Context, Mode: Checked[Tree] =
    lookIn(id).flatMap { ccCtx =>
      implied for Context = ccCtx
      for {
        pat1    <- pat.typedAsPattern(selTpe)
        guard1  <- guard.typedAsExpr(Bootstraps.BooleanType)
        body1   <- body.typedAsExpr(pt)
      } yield CaseClause(pat1, guard1, body1)(id, body1.tpe)
    }

  def typedBind(name: Name, body: Tree)
               (id: Id, pt: Type) given Context, Mode: Checked[Tree] =
    for (body1 <- body.typed(pt))
      yield
        if name == Name.Wildcard then {
          body1
        } else if mode == PatAlt then {
          CompilerError.IllegalState(
            s"Illegal variable ${name} in pattern alternative")
        } else {
          putType(name -> body1.tpe)
          Bind(name, body1)(id, body1.tpe)
        }

  def typedAlternative(patterns: List[Tree])
                      (id: Id, pt: Type) given Context, Mode: Checked[Tree] = {
    implied for Mode = Mode.PatAlt
    for {
      patterns1 <- patterns.mapE(_.typed(pt))
      tpe       <- patterns1.unifiedTpe
    } yield Alternative(patterns1)(id, tpe)
  }

  inline def replace(tpe: Type, substitutions: List[(Name, Type)]): Type =
    substitutions.foldLeft(tpe) { (acc, pair) =>
      import TypeOps._
      val (sub, by) = pair
      unify(sub, by, acc)
    }

  def getPrimitiveType(name: Name) given Context: Checked[Type] =
    if isPrimitive(name) then {
      getType(name)
    } else {
      import implied NameOps._
      CompilerError.UnexpectedType(
        s"${name.userString} is not a constructor.")
    }

  def typedUnapply(functor: Name, args: List[Tree])
                  (id: Id, pt: Type) given Context, Mode: Checked[Tree] =
    for {
      fst   <-  firstCtx(functor)
      fTpe  <-  checked {
                  implied for Context = fst
                  getPrimitiveType(functor)
                }
      pair  <-  typeAsDestructor(fTpe, pt)
      args1 <-  checked {
                  val (fTpeArgs, _) = pair
                  typeAsFunctorArgs(functor, args, fTpeArgs)
                }
    } yield {
      val (_, tpe) = pair
      Unapply(functor, args1)(id, tpe)
    }

  def typePackaging(tree: Tree) given Context: Checked[(Context, Tree)] = {
    import TypeOps._
    import TreeOps._
    import Name._
    tree.toNamePairs match {
      case (id, name) :: tail =>
        for {
          next  <- lookIn(id)
          tpe   <- declarePackage(From(rootString), name)
        } yield (
          tail.foldLeftE(next, Ident(name)(id, tpe)) { (acc, pair) =>
            val (current, parent) = acc
            val (id, name)        = pair
            implied for Context   = current
            for {
              next  <- lookIn(id)
              tpe   <- declarePackage(packageName(parent.tpe), name)
            } yield
              (next, Select(parent, name)(id, tpe))
          }
        )
      case _ =>
        (ctx, EmptyTree)
    }
  }

  def typedPackageDef(pid: Tree, stats: List[Tree])
                     (id: Id, pt: Type) given Context, Mode: Checked[Tree] =
    for {
      pair    <-  typePackaging(pid)
      stats1  <-  checked {
                    val (pkgCtx, _) = pair
                    implied for Context = pkgCtx
                    stats.mapE(_.typed(any))
                  }
    } yield {
      val (_, pid1) = pair
      PackageDef(pid1, stats1)(id, pid1.tpe)
    }

  def typedDefDef(modifiers: Set[Modifier], sig: Tree, tpeD: Tree, body: Tree)
                 (id: Id, pt: Type) given Context, Mode: Checked[Tree] = {
    val typeTpeAs =
      if modifiers.contains(Modifier.Primitive) then
        typedAsPrimitive
      else
        typedAsTyping
    val tpd = for {
      tpeD1 <-  typeTpeAs(tpeD)(any)
      tpe   <-  tpeD1.tpe
      sig1  <-  sig.typed(tpe)
      body1 <-  lookIn(sig1.id).flatMap { bodyCtx =>
                  import TypeOps._
                  val ret = toReturnType(sig1, tpe)
                  implied for Context = bodyCtx
                  body.typed(ret)
                }
      _     <-  checked {
                  val name = getName(sig)
                  putType(name, resolveVariables(tpe))
                  if modifiers.contains(Modifier.Primitive) then {
                    setPrimitive(name)
                  }
                }
    } yield DefDef(modifiers, sig1, tpeD1, body1)(id, tpe)
    commitId(sig.id)
    tpd
  }

  def mapArgs(args: List[Tree], pts: List[Type])
             given Context: Checked[List[Tree]] = {
    import TreeOps._
    args.flatMap(_.toNamePairs)
        .zip(pts)
        .mapE { zipped =>
          val ((id, name), tpe) = zipped
          for (nameCtx <- lookIn(id))
            yield {
              putType(name, tpe)
              Ident(name)(id, tpe)
            }
        }
  }

  def typedDefSig(name: Name, args: List[Tree])
                 (id: Id, pt: Type) given Context, Mode: Checked[Tree] = {
    import TypeOps._

    val pts = toCurriedList(pt)

    if pts.length <= args.length then {
      import implied NameOps._
      CompilerError.UnexpectedType(
        s"Function declaration arguments do not match declared type for declaration ${name.userString}.")
    } else {
      lookIn(id).flatMap { bodyCtx =>
        implied for Context = bodyCtx
        for (args1 <- mapArgs(args, pts))
          yield DefSig(name, args1)(id, pt)
      }
    }
  }

  def typedSelectType(from: Tree, name: Name)
                     (id: Id, pt: Type) given Context, Mode: Checked[Tree] =
    CompilerError.SyntaxError("Member selections do not exist for types.")

  def typedSelectTerm(from: Tree, name: Name)
                     (id: Id, pt: Type) given Context, Mode: Checked[Tree] =
    CompilerError.SyntaxError("Member selections do not exist for terms.")

  def typedIdentType(name: Name)
                    (id: Id, pt: Type) given Context, Mode: Checked[Tree] = {
    val lookup = firstCtx(name).flatMap { ctx1 =>
      implied for Context = ctx1
      getType(name)
    }
    val tpe = lookup.fold
      { notFound => TypeRef(name) }
      { tpe =>
        if isDefined(tpe) then
          tpe
        else
          TypeRef(name)
      }
    Ident(name)(id, tpe)
  }

  def typedIdentPat(name: Name)
                   (id: Id, pt: Type) given Context, Mode: Checked[Tree] = {
    import Name._
    if mode == PatAlt && name != Wildcard then {
      CompilerError.SyntaxError("Illegal variable in pattern alternative")
    } else {
      if name != Wildcard then {
        putType(name -> pt)
      }
      Ident(name)(id, pt)
    }
  }

  def typedIdentTerm(name: Name)
                    (id: Id, pt: Type) given Context, Mode: Checked[Tree] = {
    import TypeOps._
    for {
      fstCtx    <- firstCtx(name)
      idRefTpe  <- checked {
        implied for Context = fstCtx
        getType(name)
      }
    } yield
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

  def typedLiteral(constant: Constant)
                  (id: Id) given Context, Mode: Checked[Tree] =
    Literal(constant)(id, constantTpe(constant))

  def check(typed: Tree)(pt: Type) given Context, Mode: Checked[Tree] = {
    import TypeOps._

    lazy val typedTpe = typed.tpe

    def ignoreType(tree: Tree) = tree match {
      case EmptyTree | TreeSeq(_) => true
      case _ => false
    }

    val safe = pt =!= typedTpe ||
      ignoreType(typed)

    if safe then {
      typed
    } else {
      import implied TypeOps._
      import implied TreeOps._
      import implied ModeOps._
      val treeStr = typed.userString
      val modeStr = mode.userString
      val ptStr   = pt.userString
      val tpeStr  = typedTpe.userString
      CompilerError.UnexpectedType(
        s"Check failed. Type $tpeStr != $ptStr in $modeStr:\n$treeStr")
    }
  }

  def (tree: Tree) typed(pt: Type) given Context, Mode: Checked[Tree] = {
    def inner(tree: Tree, pt: Type) = tree match {
      // Types
      case Select(t,n)      if isType     => typedSelectType(t,n)(tree.id, pt)
      case Ident(n)         if isType     => typedIdentType(n)(tree.id, pt)
      case Apply(t,ts)      if isType     => typedApplyType(t,ts)(tree.id, pt)
      case Function(ts,t)   if isType     => typedFunctionType(ts,t)(tree.id, pt)
      // Patterns
      case Ident(n)         if isPattern  => typedIdentPat(n)(tree.id, pt)
      case Bind(n,t)        if isPattern  => typedBind(n,t)(tree.id, pt)
      case Alternative(ts)  if isPattern  => typedAlternative(ts)(tree.id, pt)
      case Unapply(f,ts)    if isPattern  => typedUnapply(f,ts)(tree.id, pt)
      // Terms
      case PackageDef(t,ts) if isTerm     => typedPackageDef(t,ts)(tree.id, pt)
      case Apply(t,ts)      if isTerm     => typedApplyTerm(t,ts)(tree.id, pt)
      case DefDef(m,s,t,b)  if isTerm     => typedDefDef(m,s,t,b)(tree.id, pt)
      case DefSig(n,ns)     if isTerm     => typedDefSig(n,ns)(tree.id, pt)
      case Let(             // Let
        i @ Ident(n),       // Let
        v,                  // Let
        c)                  if isTerm     => typedLet(n,i.id)(v,c)(tree.id, pt)
      case Function(ts,t)   if isTerm     => typedFunctionTerm(ts,t)(tree.id, pt)
      case Tagged(n,t)      if isTerm     => typedTagged(n,t)(tree.id, pt)
      case CaseExpr(t,ts)   if isTerm     => typedCaseExpr(t,ts)(tree.id, pt)
      case Select(t,n)      if isTerm     => typedSelectTerm(t,n)(tree.id, pt)
      case Ident(n)         if isTerm     => typedIdentTerm(n)(tree.id, pt)
      // Any
      case Literal(c)                     => typedLiteral(c)(tree.id)
      case Parens(ts)                     => typedParens(ts)(tree.id, pt)
      case t @ ( TreeSeq(_)
               | CaseClause(_,_,_)
               | EmptyTree)               => t
      // Error
      case _                              =>
        import implied TreeOps._
        import implied ModeOps._
        CompilerError.IllegalState(
          s"Typing not implemented for <${mode.userString}, ${tree.userString}>")
    }
    inner(tree, pt).map(check(_)(pt))
  }
}