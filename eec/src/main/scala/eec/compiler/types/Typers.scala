package eec
package compiler
package types

import Types._
import TypeOps._
import Type._
import core.Names._
import Name._
import core.Constants._
import Constant._
import core.Modifiers._
import ast._
import ast.Trees._
import Tree._
import TreeOps._
import error._
import CompilerErrors._
import CompilerErrorOps._
import core.Contexts._
import Context._
import util.{Convert, Showable}
import Mode._
import Convert._

import implied NameOps._
import implied TypeOps._
import implied CompilerErrorOps._
import implied TreeOps._

object Typers {
  import Stoup._
  import StoupOps._

  private[Typers] val any = WildcardType

  enum Stoup {
    case DependsOn(z: Name)
    case Blank
  }

  object Stoup {
    def stoup given (stoup: Stoup): Stoup = stoup
  }

  object StoupOps {

    implied given Context for Showable[Stoup] {
      def (stoup: Stoup) show = stoup match {
        case Blank => "Γ | -"

        case DependsOn(name) =>
          val tpe = for {
            ctx <- firstCtx(name)
            tpe <- checked {
              implied for Context = ctx
              getType(name)
            }
          } yield tpe
          tpe.fold
            { err => s"Γ | ${name.show}: <error: Not Found>" }
            { tpe => s"Γ | ${name.show}: ${tpe.show}" }
      }
    }
  }

  def (tree: Tree) typedWith(pt: Type) given Context: Checked[Tree] = {
    implied for Stoup = Blank
    tree.typedAsExpr(pt)
  }

  private def (tree: Tree) typedAsExpr(pt: Type) given Context, Stoup: Checked[Tree] = {
    implied for Mode  = Term
    tree.typed(pt)
  }

  private def (tree: Tree) typedAsTyping(pt: Type) given Context, Stoup: Checked[Tree] = {
    implied for Mode  = Typing
    tree.typed(pt)
  }

  private def (tree: Tree) typedAsPrimitive(pt: Type) given Context, Stoup: Checked[Tree] = {
    implied for Mode  = PrimitiveType
    tree.typed(pt)
  }

  private def (tree: Tree) typedAsPattern(pt: Type) given Context, Stoup: Checked[Tree] = {
    implied for Mode  = Pat
    tree.typed(pt)
  }

  private def (ts: List[Tree]) unifiedTpe: Checked[Type] = {
    if ts.isEmpty then {
      EmptyType
    } else {
      val tpe = ts.head.tpe
      val unified = ts.tail.foldLeft(true)((acc, t) => acc && t.tpe == tpe)
      if unified then
        tpe
      else
        TyperErrors.tpesNotUnifyTo(tpe)
    }
  }

  private def resolveVariables(tpe: Type) given Context: Type = {
    tpe.mapTypeRefs { name =>
      val lookup = firstCtx(name).flatMap { ctx1 =>
        implied for Context = ctx1
        getType(name)
      }
      lookup.fold
        { _   => Variable(name) }
        { tpe =>
            if isDefined(tpe) then TypeRef(name)
            else Variable(name)
        }
    }
  }

  private def checkFunctorWithProto(functor: Tree, proto: Type): Checked[Type] = {
    val functorTyp = functor.tpe
    (functorTyp, proto) match {
      case (FunctionType(arg, ret), FunctionType(arg1, ret1)) =>
        val unifications  = functorTyp.unifications(proto)
        val functorTyp1   = functorTyp.unifyFromAll(unifications)
        val proto1        = proto.unifyFromAll(unifications)
        if functorTyp1 =!= proto1 then
          toCurriedList(functorTyp1).last
        else
          TyperErrors.functorNotMatchProto(functor, functorTyp1, proto1)

      case _ => TyperErrors.noApplyNonFunctionType
    }
  }

  private def checkFunWithProto(fun: Tree, argProto: Type)(pt: Type): Checked[Type] = {
    val funTyp = fun.tpe
    funTyp match {
      case FunctionType(arg, ret) =>
        val FunctionType(arg1, ret1) = funTyp.unifyFrom(arg)(argProto)
        if arg1 =!= argProto then
          ret1.unify(pt)
        else
          TyperErrors.functionNotMatchProto(fun, arg1, argProto)

      case _ => TyperErrors.noApplyNonFunctionType
    }
  }

  private def constantTpe(c: Constant): Type = c match {
    case _: BooleanConstant => Bootstraps.BooleanType
    case _: BigDecConstant  => Bootstraps.DecimalType
    case _: BigIntConstant  => Bootstraps.IntegerType
    case _: CharConstant    => Bootstraps.CharType
    case _: StringConstant  => Bootstraps.StringType
  }

  private def functionTermTpe(args1: List[Tree], body1: Tree): Checked[Type] = {
    val tpes  = args1.map(_.tpe)
    val fType = tpes.foldRight(body1.tpe) { FunctionType(_,_) }
    if !fType.isComputationType then
      TyperErrors.noCompCodomain
    else
      fType
  }

  private def typedFunctionTerm(args: List[Tree], body: Tree)
                               (id: Id, pt: Type)
                               given Context, Mode, Stoup: Checked[Tree] = {
    lookIn(id).flatMap { fCtx =>
      implied for Context = fCtx
      for {
        args1 <-  args.mapE(_.typed(any))
        body1 <-  body.typed(any)
        fTpe  <-  functionTermTpe(args1, body1)
      } yield Function(args1, body1)(id, fTpe)
    }
  }

  private def typedLinearFunctionTerm(arg: Tree, body: Tree)
                                     (id: Id, pt: Type)
                                     given Context, Mode, Stoup: Checked[Tree] = {
    lookIn(id).flatMap { fCtx =>
      implied for Context = fCtx
      for {
        arg1  <-  arg.typed(any)
        body1 <-  checked {
          implied for Stoup = DependsOn(arg.convert)
          body.typed(any)
        }
        fTpe  <-  linearFunctionTypeTpe(arg1, body1)
      } yield LinearFunction(arg1, body1)(id, fTpe)
    }
  }

  private def functionTypeTpe(args1: List[Tree], body1: Tree)
                             given Mode: Checked[Type] = {
    val argTpe  = args1.map(_.tpe).convert
    val fType   = FunctionType(argTpe, body1.tpe)
    if mode != PrimitiveType && !fType.isComputationType then
      TyperErrors.noCompCodomain
    else
      fType
  }

  private def linearFunctionTypeTpe(arg1: Tree, body1: Tree)
                             given Mode: Checked[Type] = {
    if arg1.tpe.isValueType then
      TyperErrors.noCompArg
    else if body1.tpe.isValueType then
      TyperErrors.noLinearCompCodomain
    else
      LinearFunctionType(arg1.tpe, body1.tpe)
  }

  private def typedFunctionType(args: List[Tree], body: Tree)
                       (id: Id, pt: Type)
                       given Context, Mode, Stoup: Checked[Tree] = {
    for {
      args1 <- args.mapE(_.typed(any))
      body1 <- body.typed(any)
      fTpe  <- functionTypeTpe(args1, body1)
    } yield Function(args1, body1)(id, fTpe)
  }

  private def typedLinearFunctionType(arg: Tree, body: Tree)
                                     (id: Id, pt: Type)
                                     given Context, Mode, Stoup: Checked[Tree] = {
    for {
      arg1  <- arg.typed(any)
      body1 <- body.typed(any)
      fTpe  <- linearFunctionTypeTpe(arg1, body1)
    } yield LinearFunction(arg1, body1)(id, fTpe)
  }

  private def typedTagged(arg: Name, tpeTree: Tree)
                 (pt: Type)
                 given Context, Mode, Stoup: Checked[Tree] = {
    for (tpeTree1 <- tpeTree.typedAsTyping(pt))
    yield {
      putType(arg -> tpeTree1.tpe)
      Tagged(arg, tpeTree1)(tpeTree1.tpe)
    }
  }

  private def typeAsTuple(ts: List[Tree], pt: Type)
                         given Context, Mode, Stoup: Checked[List[Tree]] = {
    if pt == any then {
      ts.mapE(_.typed(any))
    } else {
      val ptAsTuple = pt.convert
      if ts.length != ptAsTuple.length then
        TyperErrors.tupleNoMatch(pt, ptAsTuple)
      else
        ts.zip(ptAsTuple).mapE(_.typed(_))
    }
  }

  private def typedParens(ts: List[Tree])
                 (pt: Type)
                 given Context, Mode, Stoup: Checked[Tree] = {
    for (ts1 <- typeAsTuple(ts, pt))
    yield Parens(ts1)(ts1.map(_.tpe).convert)
  }

  private def typeAsDestructor(fTpe: Type, pt: Type): Checked[(List[Type], Type)] = {
    toCurriedList(fTpe).reverse match {
      case ret0 :: args0 =>
        val unifications  = ret0.unifications(pt)
        val ret           = ret0.unifyFromAll(unifications)
        val fTpeArgs      = args0.reverse.map(_.unifyFromAll(unifications))
        (fTpeArgs, ret)

      case _ => TyperErrors.emptyFunctor(fTpe)
    }
  }

  private def typeAsFunctorArgs(
        functor: Name, ts: List[Tree], fTpeArgs: List[Type])
      given Context, Mode, Stoup: Checked[List[Tree]] = {
    if ts.length != fTpeArgs.length then
      TyperErrors.argsNotMatchLength
    else
      ts.zip(fTpeArgs).mapE(_.typed(_))
  }

  private def unifyConstructorToHkType(functor: Tree, args: List[Type]): Checked[Type] = {
    val res :: args0 = toCurriedList(functor.tpe).reverse
    if args0.length == args.length then res
    else TyperErrors.hkArgsDoNotUnify(functor, args0, args)
  }

  private def typedApplyType(functor: Tree, args: List[Tree])
                            given Context, Mode, Stoup: Checked[Tree] = {
    for {
      functor1  <-  functor.typed(any)
      args1     <-  args.mapE(_.typed(any))
      applied   <-  unifyConstructorToHkType(functor1, args1.map(_.tpe))
      funProto  <-  toFunctionType(args1.map(_.tpe) :+ applied)
      tpe       <-  checkFunctorWithProto(functor1, funProto)
    } yield Apply(functor1, args1)(tpe)
  }

  private def typedInfixApplyType(functor: Tree, a1: Tree, a2: Tree)
                                 given Context, Mode, Stoup: Checked[Tree] = {
    for {
      functor1  <-  functor.typed(any)
      a11       <-  a1.typed(any)
      a21       <-  a2.typed(any)
      args1     =   a11.tpe :: a21.tpe :: Nil
      applied   <-  unifyConstructorToHkType(functor1, args1)
      funProto  <-  toFunctionType(args1 :+ applied)
      tpe       <-  checkFunctorWithProto(functor1, funProto)
    } yield InfixApplyType(functor1, a11, a21)(tpe)
  }

  private def typedApplyTerm(fun: Tree, args: List[Tree])
                            (pt: Type)
                            given Context, Mode, Stoup: Checked[Tree] = {
    for {
      fun1      <-  fun.typed(any)
      args1     <-  checked {
                      implied for Stoup = Blank
                      args.mapE(_.typed(any))
                    }
      argsProto <-  args1.map(_.tpe).convert
      tpe       <-  checkFunWithProto(fun1, argsProto)(pt)
    } yield Apply(fun1, args1)(tpe)
  }

  private def unwrapCompApply(
       tpe: Type)
      (name: Name, value: Tree): Checked[Type] = tpe match {
    case AppliedType(TypeRef(ComputationTag), List(t)) => t

    case _ =>
      TyperErrors.noBangLetValue(name, value)
  }

  private def typedLet(name: Name, value: Tree, cont: Tree)
                      (id: Id, pt: Type)
                      given Context, Mode, Stoup: Checked[Tree] = {
    for {
      lCtx    <-  lookIn(id)
      value1  <-  value.typed(any)
      value1U <-  unwrapCompApply(value1.tpe)(name, value)
      cont1   <-  checked {
                    implied for Context = lCtx
                    implied for Stoup   = Blank
                    putType(name -> value1U)
                    cont.typed(pt)
                        .filter(TyperErrors.noCompLetContinuation) {
                          _.tpe.isComputationType
                        }
                  }
    } yield Let(name, value1, cont1)(id, cont1.tpe)
  }

  private def typeAsCaseClauses(ts: List[Tree], selTpe: Type)
                               (pt: Type)
                               given Context, Mode, Stoup: Checked[List[Tree]] = {
    ts.mapE {
      case t @ CaseClause(p,g,b)  => typedCaseClause(p, g, b, selTpe)(t.id, pt)
      case unknown                => TyperErrors.notCaseClase(unknown)
    }
  }

  private def typedCaseExpr(selector: Tree, cases: List[Tree])
                           (pt: Type)
                           given Context, Mode, Stoup: Checked[Tree] = {
    for {
      selector1  <- checked {
                      implied for Stoup = Blank
                      selector.typed(any)
                    }
      cases1     <- typeAsCaseClauses(cases, selector1.tpe)(pt)
      tpe        <- cases1.unifiedTpe
    } yield CaseExpr(selector1, cases1)(tpe)
  }

  private def typedCaseClause(pat: Tree, guard: Tree, body: Tree, selTpe: Type)
                             (id: Id, pt: Type)
                             given Context, Mode, Stoup: Checked[Tree] = {
    lookIn(id).flatMap { ccCtx =>
      implied for Context = ccCtx
      for {
        pat1    <- pat.typedAsPattern(selTpe)
        guard1  <- guard.typedAsExpr(Bootstraps.BooleanType)
        body1   <- body.typedAsExpr(pt)
      } yield CaseClause(pat1, guard1, body1)(id, body1.tpe)
    }
  }

  private def typedBind(name: Name, body: Tree)
                       (pt: Type)
                       given Context, Mode, Stoup: Checked[Tree] = {
    for (body1 <- body.typed(pt))
    yield {
      if name == Name.Wildcard then {
        body1
      } else if mode == PatAlt then {
        TyperErrors.nameInPattAlt(name)
      } else {
        putType(name -> body1.tpe)
        Bind(name, body1)(body1.tpe)
      }
    }
  }

  private def typedAlternative(patterns: List[Tree])
                              (pt: Type)
                              given Context, Mode, Stoup: Checked[Tree] = {
    implied for Mode = Mode.PatAlt
    for {
      patterns1 <- patterns.mapE(_.typed(pt))
      tpe       <- patterns1.unifiedTpe
    } yield Alternative(patterns1)(tpe)
  }

  private def getPrimitiveType(name: Name) given Context: Checked[Type] =
    if isPrimitive(name) then getType(name)
    else TyperErrors.nameNotConstructor(name)

  private def typedUnapply(functor: Name, args: List[Tree])
                          (pt: Type)
                          given Context, Mode, Stoup: Checked[Tree] = {
    for {
      fCtx  <-  firstCtx(functor)
      fTpe  <-  checked {
                  implied for Context = fCtx
                  getPrimitiveType(functor)
                }
      pair  <-  typeAsDestructor(fTpe, pt)
      (fTpeArgs, tpe) = pair
      args1 <-  typeAsFunctorArgs(functor, args, fTpeArgs)
    } yield Unapply(functor, args1)(tpe)
  }

  private def typePackaging(tree: Tree)
                           given Context: Checked[(Context, Tree)] = {

    def foldNamePairs(id: Id, name: Name, tail: List[(Id, Name)]) given Context = {
      for {
        z <- for {
          next  <- lookIn(id)
          tpe   <- declarePackage(rootName, name)
        } yield (next, Ident(name)(id, tpe))
        pair <- tail.foldLeftE(z)(accumulatePackages)
      } yield pair
    }

    def accumulatePackages(z: (Context, Tree), current: (Id, Name)) = {
      val (ctx, parent)   = z
      val (id, name)      = current
      implied for Context = ctx
      declareInParent(parent, id, name)
    }

    def declareInParent(parent: Tree, id: Id, name: Name) given Context = {
      for {
        next  <- lookIn(id)
        tpe   <- declarePackage(packageName(parent.tpe), name)
      } yield (next, Select(parent, name)(id, tpe))
    }

    tree.toNamePairs match {
      case (id, name) :: tail => foldNamePairs(id, name, tail)
      case _                  => (ctx, EmptyTree)
    }
  }

  private def typedPackageDef(pid: Tree, stats: List[Tree])
                             (pt: Type)
                             given Context, Mode, Stoup: Checked[Tree] = {
    for {
      pair    <-  typePackaging(pid)
      (pkgCtx, pid1) = pair
      stats1  <-  checked {
                    implied for Context = pkgCtx
                    stats.mapE(_.typed(any))
                  }
    } yield PackageDef(pid1, stats1)(pid1.tpe)
  }

  private def typedDefDef(modifiers: Set[Modifier], sig: Tree & Unique, tpeD: Tree, body: Tree)
                         (pt: Type)
                         given Context, Mode, Stoup: Checked[Tree] = {
    val typeTpeAs = {
      if modifiers.contains(Modifier.Primitive) then
        typedAsPrimitive
      else
        typedAsTyping
    }
    val tpd = for {
      tpeD1    <- typeTpeAs(tpeD)(any)
      tpe      =  tpeD1.tpe
      sig1     <- sig.typed(tpe)
      ret      =  toReturnType(sig1, tpe)
      bodyCtx  <- lookIn(sig.id)
      newStoup = {
        val linear = sig.linearArg
        if linear != EmptyName then
          DependsOn(linear)
        else
          stoup
      }
      body1 <- checked {
        implied for Context = bodyCtx
        implied for Stoup   = newStoup
        body.typed(ret)
      }
    } yield {
      val name     = (sig1.convert: Name)
      val freshTpe = resolveVariables(tpe)
      putType(name, freshTpe)
      if modifiers.contains(Modifier.Primitive) then {
        setPrimitive(name)
      }
      DefDef(modifiers, sig1, tpeD1, body1)(tpe)
    }
    commitId(sig.id)
    tpd
  }

  private def mapArgs(args: List[Name], pts: List[Type])
                     given Context: Checked[Unit] = {
    args
      .zip(pts)
      .foldLeftE(()) { (_, pair) =>
        val (name, tpe) = pair
        for (_ <- lookForInScope(name))
        yield {
          putType(name, tpe)
        }
      }
  }

  private def mapLinearArg(arg: Name, tpe: Type)
                          given Context: Checked[Unit] = tpe match {
    case LinearFunctionType(argTpe, _) =>
      for (_ <- lookForInStoup(arg))
      yield {
        putType(arg, argTpe)
      }

    case _ => TyperErrors.linearArgNotMatchType(arg)
  }

  private def typedDefSig(name: Name, args: List[Name])
                         (id: Id, pt: Type)
                         given Context: Checked[Tree] = {
    val pts = toCurriedList(pt)
    if pts.length <= args.length then
      TyperErrors.declArgsNotMatchType(name)
    else
      lookIn(id).flatMap { bodyCtx =>
        implied for Context = bodyCtx
        for (_ <- mapArgs(args, pts))
        yield DefSig(name, args)(id, pt)
      }
  }

  private def typedPrimSig(name: Name, args: List[Name], linear: Name)
                          (id: Id, pt: Type)
                          given Context: Checked[Tree] = {
    val pts = toCurriedList(pt)
    if pts.length <= args.length then
      TyperErrors.declArgsNotMatchType(name)
    else
      lookIn(id).flatMap { bodyCtx =>
        implied for Context = bodyCtx
        for {
          _ <- mapArgs(args, pts)
          _ <- mapLinearArg(linear, pts.last)
        } yield PrimSig(name, args, linear)(id, pt)
      }
  }

  private def typedSelectType given Mode: Checked[Tree] =
    TyperErrors.memberSelection

  private def typedSelectTerm given Mode: Checked[Tree] =
    TyperErrors.memberSelection

  private def typedIdentType(name: Name)
                    (id: Id, pt: Type)
                    given Context: Checked[Tree] = {
    val lookup = {
      firstCtx(name).flatMap { ctx1 =>
        implied for Context = ctx1
        getType(name)
      }
    }
    val tpe = {
      lookup.fold(_ => TypeRef(name)) { tpe =>
        if isDefined(tpe) then tpe
        else TypeRef(name)
      }
    }
    Ident(name)(id, tpe)
  }

  private def typedIdentPat(name: Name)
                   (id: Id, pt: Type)
                   given Context, Mode: Checked[Tree] = {
    if mode == PatAlt && name != Wildcard then {
      TyperErrors.nameInPattAlt(name)
    } else {
      if name != Wildcard then {
        putType(name -> pt)
      }
      Ident(name)(id, pt)
    }
  }

  private def typedIdentTerm(name: Name)
                    (id: Id, pt: Type)
                    given Context, Mode, Stoup: Checked[Tree] = {
    for {
      fstCtx    <-  firstCtx(name)
      _         <-  checked {
                      implied for Context = fstCtx
                      assertStoupCondition(name)
                    }
      idRefTpe  <-  checked {
                      implied for Context = fstCtx
                      getTypeIdent(name)
                    }
    } yield Ident(name)(id, idRefTpe.freshVariables)
  }

  private def assertStoupCondition(name: Name) given Context, Stoup: Checked[Unit] = stoup match {
    case Blank if inStoup(name)              => TyperErrors.foundInStoup(name)
    case DependsOn(`name`) if !inStoup(name) => TyperErrors.notInStoup(name)
    case _ => ()
  }

  private def typedLiteral(constant: Constant): Checked[Tree] =
    Literal(constant)(constantTpe(constant))

  private def check(typed: Tree)(pt: Type) given Mode: Checked[Tree] = {
    inline def ignoreType(tree: Tree) = tree match {
      case EmptyTree | _: TreeSeq => true
      case _                      => false
    }
    if pt =!= typed.tpe || ignoreType(typed) then
      typed
    else
      TyperErrors.typecheckFail(typed, pt)
  }

  private def (tree: Tree) typed
              (pt: Type)
              given Context, Mode, Stoup: Checked[Tree] = {

    def inner(tree: Tree, pt: Type) = tree match {
      // Types
      case _: Select                if isType     => typedSelectType
      case u @ Ident(n)             if isType     => typedIdentType(n)(u.id,pt)
      case Apply(t,ts)              if isType     => typedApplyType(t,ts)
      case InfixApplyType(f,a,b)    if isType     => typedInfixApplyType(f,a,b)
      case u @ Function(ts,t)       if isType     => typedFunctionType(ts,t)(u.id, pt)
      case u @ LinearFunction(a,b)  if isType     => typedLinearFunctionType(a,b)(u.id, pt)
      // Patterns
      case u @ Ident(n)             if isPattern  => typedIdentPat(n)(u.id, pt)
      case Bind(n,t)                if isPattern  => typedBind(n,t)(pt)
      case Alternative(ts)          if isPattern  => typedAlternative(ts)(pt)
      case Unapply(f,ts)            if isPattern  => typedUnapply(f,ts)(pt)
      // Terms
      case PackageDef(t,ts)         if isTerm     => typedPackageDef(t,ts)(pt)
      case Apply(t,ts)              if isTerm     => typedApplyTerm(t,ts)(pt)
      case DefDef(
        m,
        s: (DefSig | PrimSig),
        t,
        b)                          if isTerm     => typedDefDef(m,s,t,b)(pt)
      case u @ DefSig(n,ns)         if isTerm     => typedDefSig(n,ns)(u.id, pt)
      case u @ PrimSig(n,ns,l)      if isTerm     => typedPrimSig(n,ns,l)(u.id, pt)
      case u @ Let(n,v,c)           if isTerm     => typedLet(n,v,c)(u.id, pt)
      case u @ Function(ts,t)       if isTerm     => typedFunctionTerm(ts,t)(u.id, pt)
      case u @ LinearFunction(a,b)  if isTerm     => typedLinearFunctionTerm(a,b)(u.id, pt)
      case Tagged(n,t)              if isTerm     => typedTagged(n,t)(pt)
      case CaseExpr(t,ts)           if isTerm     => typedCaseExpr(t,ts)(pt)
      case _: Select                if isTerm     => typedSelectTerm
      case u @ Ident(n)             if isTerm     => typedIdentTerm(n)(u.id, pt)
      // Any
      case Literal(c)                             => typedLiteral(c)
      case Parens(ts)                             => typedParens(ts)(pt)
      case t @ ( _: TreeSeq
               | _: CaseClause
               |    EmptyTree)                    => t
      // Error
      case _                                      => TyperErrors.typingMissing(tree)
    }
    inner(tree, pt).map(check(_)(pt))
  }
}