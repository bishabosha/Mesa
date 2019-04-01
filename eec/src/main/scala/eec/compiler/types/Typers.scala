package eec
package compiler
package types

import Types._
import TypeOps._
import Type._
import core.Names._
import Name._
import NameOps._
import core.Constants._
import Constant._
import core.Modifiers._
import ast._
import ast.Trees._
import Tree._
import TreeOps._
import error._
import types.{TyperErrors => Err}
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

  private[Typers] val any = WildcardType

  private[Typers] enum Stoup {
    case DependsOn(z: Name)
    case Blank
  }

  private[Typers] object Stoup {
    def stoup given (stoup: Stoup): Stoup = stoup
  }

  def (tree: Tree) typedWith(pt: Type) given Context: Checked[Tree] = {
    implied for Stoup = Blank
    tree.typedAsExpr(pt)
  }

  private def (tree: Tree) typedAsExpr(pt: Type) given Context, Stoup: Checked[Tree] = {
    implied for Mode = Term
    tree.typed(pt)
  }

  private def (tree: Tree) typedAsTyping(pt: Type) given Context, Stoup: Checked[Tree] = {
    implied for Mode = Typing
    tree.typed(pt)
  }

  private def (tree: Tree) typedAsPrimitive(pt: Type) given Context, Stoup: Checked[Tree] = {
    implied for Mode = PrimitiveType
    tree.typed(pt)
  }

  private def (tree: Tree) typedAsPattern(pt: Type) given Context, Stoup: Checked[Tree] = {
    implied for Mode = Pat
    tree.typed(pt)
  }

  private def (tree: Tree) typedAsLinearPattern(pt: Type) given Context, Stoup: Checked[Tree] = {
    implied for Mode = LinearPat
    tree.typed(pt)
  }

  private def (ts: List[Tree]) sameType(pt: Type): Checked[Type] = ts match {
    case t :: ts =>
      unifiesThen[Checked[Type]](t.tpe, pt)
        { t1Tpe =>
          if ts.forall(t => unifies(t.tpe, t1Tpe)) then t1Tpe
          else Err.tpesNotUnifyTo(pt)
        } { (_,_) => Err.tpesNotUnifyTo(pt) }

    case _ => EmptyType
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

  private def unifyFunctionTypes(tpe1: Type, tpe2: Type) = (tpe1, tpe2) match {
    case (_: FunctionType, _: FunctionType) => true
    case _                                  => false
  }

  private def unifyLinearFunctionTypes(tpe1: Type, tpe2: Type) = (tpe1, tpe2) match {
    case (_: LinearFunctionType, _: LinearFunctionType) => true
    case _                                              => false
  }

  private def unwrapFunctionType(tpe: Type) = (tpe: @unchecked) match {
    case FunctionType(arg, ret) => (arg, ret)
  }

  private def unwrapLinearFunctionType(tpe: Type) = (tpe: @unchecked) match {
    case LinearFunctionType(arg, ret) => (arg, ret)
  }

  private val checkFunctorWithProto: (functor: Name, fTpe: Type, proto: Type) =>
                                     Checked[Type] =
    checkFunctorWithProtoImpl(unifyFunctionTypes)(toCurriedList(_).last)

  private val checkFunctionWithProto: (function: Name, fTpe: Type, proto: Type) =>
                                      Checked[Type] =
    checkFunctorWithProtoImpl(unifyFunctionTypes)(identity)

  private val checkLinearFunctionWithProto: (function: Name, fTpe: Type, proto: Type) =>
                                            Checked[Type] =
    checkFunctorWithProtoImpl(unifyLinearFunctionTypes)(identity)

  private val checkFunWithProto: (fun: Tree, argProto: Type) =>
                                 (pt: Type) => Checked[Type] = {
    checkFunWithProtoImpl {
      case FunctionType(arg,_) => arg
      case _ => Err.noApplyNonFunctionType
    }(unwrapFunctionType)(Err.functionNotMatch)
  }

  private val checkLinearFunWithProto: (fun: Tree, argProto: Type) =>
                                       (pt: Type) => Checked[Type] = {
    checkFunWithProtoImpl {
      case LinearFunctionType(arg,_) => arg
      case _ => Err.noEvalNonLinearFunctionType
    }(unwrapLinearFunctionType)(Err.linearFunctionNotMatch)
  }

  private def checkFunctorWithProtoImpl(canUnify: (Type, Type) => Boolean)
                                       (f: Type => Type)
                                       (fun: Name, fTpe: Type, proto: Type): Checked[Type] = {
    if proto == WildcardType then {
      fTpe
    } else if canUnify(fTpe, proto) then {
      unifiesThen[Checked[Type]](fTpe, proto)(f)(
        Err.functorNotMatch(fun, fTpe, _, _))
    } else {
      Err.noApplyNonFunctionType
    }
  }

  private def checkFunWithProtoImpl(getArg: Type => Checked[Type])
                                   (unifyf: Type => (Type, Type))
                                   (onNoArgUnify: (Tree, Type, Type) => CompilerError)
                                   (fun: Tree, argProto: Type)
                                   (pt: Type): Checked[Type] = {
    for {
      arg <- getArg(fun.tpe)
      (arg1, ret) = unifyf(fun.tpe.unifyFrom(arg)(argProto))
      argProto1   = argProto.unify(arg1)
      ret1 <- checked {
        if arg1 =!= argProto1 then ret.unify(pt)
        else onNoArgUnify(fun, arg1, argProto)
      }
    } yield ret1
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
    tpes.foldRight(body1.tpe) { FunctionType(_,_) }
  }

  private def typedFunctionTerm(args: List[Tree], body: Tree)
                               (id: Id, pt: Type)
                               given Context, Mode, Stoup: Checked[Tree] = {
    lookIn(id).flatMap { fCtx =>
      implied for Context = fCtx
      for {
        args1 <- args.mapE(_.typed(any))
        body1 <- body.typed(any)
        fTpe  <- functionTermTpe(args1, body1)
        fTpe1 <- checkFunctionWithProto(EmptyName, fTpe, pt)
      } yield Function(args1, body1)(id, fTpe1)
    }
  }

  private def typedLinearFunctionTerm(arg: Tree, body: Tree)
                                     (id: Id, pt: Type)
                                     given Context, Mode, Stoup: Checked[Tree] = {
    lookIn(id).flatMap { fCtx =>
      implied for Context = fCtx
      for {
        _     <- assertEmptyStoupCondition(Err.illegalStoupLinearLambda)
        arg1  <- arg.typed(any)
        name  =  (arg.convert: Name)
        _     <- assertLinearEntryCondition(name, arg1.tpe)
        body1 <- checked {
          implied for Stoup = DependsOn(name)
          body.typed(any)
        }
        fTpe  <- linearFunctionTypeTpe(arg1, body1)
        fTpe1 <- checkLinearFunctionWithProto(EmptyName, fTpe, pt)
      } yield LinearFunction(arg1, body1)(id, fTpe1)
    }
  }

  private def functionTypeTpe(args1: List[Tree], body1: Tree)
                             given Mode: Checked[Type] = {
    val argTpe  = args1.map(_.tpe).convert
    FunctionType(argTpe, body1.tpe)
  }

  private def linearFunctionTypeTpe(arg1: Tree, body1: Tree)
                                   given Mode: Checked[Type] = {
    if arg1.tpe.isValueType then
      Err.noCompArg
    else if body1.tpe.isValueType then
      Err.noLinearCompCodomain
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

  private def tensorTermTpe(value1: Tree, comp1: Tree): Checked[Type] = {
    if comp1.tpe.isValueType then {
      Err.noTensorCompCodomain(comp1)
    } else {
      val bangTpe = AppliedType(Bootstraps.BangType, List(value1.tpe))
      InfixAppliedType(Bootstraps.TensorType, bangTpe, comp1.tpe)
    }
  }

  private def bangTermTpe(value1: Tree): Checked[Type] = {
    AppliedType(Bootstraps.BangType, List(value1.tpe))
  }

  private def typedBangTerm(t: Tree)
                           (pt: Type)
                           given Context, Mode, Stoup: Checked[Tree] = {
    for {
      _   <- assertEmptyStoupCondition(Err.illegalStoupBang)
      t1  <- t.typed(any)
      tpe <- bangTermTpe(t1)
    } yield Bang(t1)(tpe)
  }

  private def typedWhyNotTerm(t: Tree)
                             (pt: Type)
                             given Context, Mode, Stoup: Checked[Tree] = {
    for {
      t1  <- checked {
        implied for Stoup = Blank
        t.typed(Bootstraps.VoidType)
      }
    } yield WhyNot(t1)(pt)
  }

  private def typedTensorTerm(t: Tree, u: Tree)
                             (pt: Type)
                             given Context, Mode, Stoup: Checked[Tree] = {
    for {
      t1 <- checked {
        implied for Stoup = Blank
        t.typed(any)
      }
      u1  <- u.typed(any)
      tpe <- tensorTermTpe(t1, u1)
    } yield Tensor(t1, u1)(tpe)
  }

  private def typedTagged(arg: Name, tpeTree: Tree)
                 (pt: Type)
                 given Context, Mode, Stoup: Checked[Tree] = {
    for {
      tpeTree1 <- tpeTree.typedAsTyping(pt)
    } yield {
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
        Err.tupleNoMatch(pt, ptAsTuple)
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

      case _ => Err.emptyFunctor(fTpe)
    }
  }

  private def typeAsLinearDestructor(fTpe: Type, pt: Type): Checked[(Type, Type)] = {
    toCurriedList(fTpe).reverse match {
      case linearFType :: Nil =>
        linearFType match {
          case LinearFunctionType(linearArg, ret) =>
            val unifications  = ret.unifications(pt)
            val ret1          = ret.unifyFromAll(unifications)
            val linearArg1    = linearArg.unifyFromAll(unifications)
            (linearArg1, ret1)

          case _ => Err.emptyFunctorLinearCtx(fTpe)
        }

      case _ => Err.emptyFunctor(fTpe)
    }
  }

  private def typeAsFunctorArgs(
        functor: Name, ts: List[Tree], fTpeArgs: List[Type])
      given Context, Mode, Stoup: Checked[List[Tree]] = {
    if ts.length != fTpeArgs.length then
      Err.argsNotMatchLength
    else
      ts.zip(fTpeArgs).mapE(_.typed(_))
  }

  private def unifyConstructorToHkType(functor: Tree, args: List[Type]): Checked[Type] = {
    val res :: args0 = toCurriedList(functor.tpe).reverse
    if args0.length == args.length then res
    else Err.hkArgsDoNotUnify(functor, args0, args)
  }

  private def typedApplyType(functor: Tree, args: List[Tree])
                            given Context, Mode, Stoup: Checked[Tree] = {
    for {
      functor1 <- functor.typed(any)
      args1    <- args.mapE(_.typed(any))
      applied  <- unifyConstructorToHkType(functor1, args1.map(_.tpe))
      funProto <- toFunctionType(args1.map(_.tpe) :+ applied)
      name     =  uniqName(functor1)
      tpe      <- checkFunctorWithProto(name, functor1.tpe, funProto)
    } yield Apply(functor1, args1)(tpe)
  }

  private def typedInfixApply(functor: Tree, a1: Tree, a2: Tree)
                             given Context, Mode, Stoup: Checked[Tree] = {
    for {
      functor1 <- functor.typed(any)
      a11      <- a1.typed(any)
      a21      <- a2.typed(any)
      args1    =  a11.tpe :: a21.tpe :: Nil
      applied  <- unifyConstructorToHkType(functor1, args1)
      funProto <- toFunctionType(args1 :+ applied)
      name     =  uniqName(functor1)
      tpe      <- checkFunctorWithProto(name, functor1.tpe, funProto)
    } yield InfixApply(functor1, a11, a21)(tpe)
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

  private def typedEvalTerm(fun: Tree, arg: Tree)
                           (pt: Type)
                           given Context, Mode, Stoup: Checked[Tree] = {
    for {
      fun1  <-  checked {
                  implied for Stoup = Blank
                  fun.typed(any)
                }
      arg1  <-  arg.typed(any)
      tpe   <-  checkLinearFunWithProto(fun1, arg1.tpe)(pt)
    } yield Eval(fun1, arg1)(tpe)
  }

  private def unwrapCompApply(tpe: Type)
                             (x: Name, t: Tree): Checked[Type] = tpe match {
    case AppliedType(
      TypeRef(BangTag),
      List(t)
    ) => t

    case _ => Err.noBangLetValue(x, t)
  }

  private def unwrapTensorType(tpe: Type)
                              (x: Name, z: Name, s: Tree): Checked[(Type, Type)] = {
    tpe match {
      case InfixAppliedType(
        TypeRef(TensorTag),
        AppliedType(TypeRef(BangTag), List(x)),
        z
      ) => (x, z)

      case _ =>
        Err.noTensorLetValue(x, z, s)
    }
  }

  private def typedLet(x: Name, t: Tree, u: Tree)
                      (id: Id, pt: Type)
                      given Context, Mode, Stoup: Checked[Tree] = {
    for {
      lCtx  <-  lookIn(id)
      t1    <-  t.typed(any)
      t1U   <-  unwrapCompApply(t1.tpe)(x, t)
      u1    <-  checked {
                  implied for Context = lCtx
                  implied for Stoup   = Blank
                  putType(x -> t1U)
                  u.typed(pt).filter(Err.noCompLetContinuation) {
                    _.tpe.isComputationType
                  }
                }
    } yield Let(x, t1, u1)(id, u1.tpe)
  }

  private def typedLetTensor(x: Name, z: Name, s: Tree, t: Tree)
                            (id: Id, pt: Type)
                            given Context, Mode, Stoup: Checked[Tree] = {
    for {
      lCtx <- lookIn(id)
      s1   <- s.typed(any)
      s1U  <- unwrapTensorType(s1.tpe)(x, z, s)
      t1   <- checked {
        implied for Context = lCtx
        implied for Stoup   = DependsOn(z)
        val (xTpe, zType)   = s1U
        putType(x -> xTpe)
        putType(z -> zType)
        t.typed(pt)
          .filter(Err.noCompLetContinuation) {
            _.tpe.isComputationType
          }
      }
    } yield LetTensor(x, z, s1, t1)(id, t1.tpe)
  }

  private def typeAsCaseClauses(ts: List[Tree], selTpe: Type)
                               (pt: Type)
                               given Context, Mode, Stoup: Checked[List[Tree]] = {
    ts.mapE {
      case t @ CaseClause(p,g,b)  => typedCaseClause(p, g, b, selTpe)(t.id, pt)
      case unknown                => Err.notCaseClase(unknown)
    }
  }

  private def typeAsLinearCaseClauses(ts: List[Tree], selTpe: Type)
                                     (pt: Type)
                                     given Context, Mode, Stoup: Checked[List[Tree]] = {
    ts.mapE {
      case t @ LinearCaseClause(p, b) =>
        typedLinearCaseClause(p,b,selTpe)(t.id, pt)

      case unknown => Err.notLinearCaseClase(unknown)
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
      tpe        <- cases1.sameType(pt)
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

  private def typedLinearCaseExpr(selector: Tree, cases: List[Tree])
                                 (pt: Type)
                                 given Context, Mode, Stoup: Checked[Tree] = {
    for {
      selector1  <- selector.typed(any)
      cases1     <- typeAsLinearCaseClauses(cases, selector1.tpe)(pt)
      tpe        <- cases1.sameType(pt)
    } yield LinearCaseExpr(selector1, cases1)(tpe)
  }

  private def typedLinearCaseClause(pat: Tree, body: Tree, selTpe: Type)
                                   (id: Id, pt: Type)
                                   given Context, Mode, Stoup: Checked[Tree] = {
    lookIn(id).flatMap { ccCtx =>
      implied for Context = ccCtx
      for {
        pat1    <- pat.typedAsLinearPattern(selTpe)
        stoup   <- uniqueLinearVariable
        body1   <- checked {
          implied for Stoup = DependsOn(stoup)
          body.typedAsExpr(pt)
        }
      } yield LinearCaseClause(pat1, body1)(id, body1.tpe)
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
        Err.nameInPattAlt(name)
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
      tpe       <- patterns1.sameType(pt)
    } yield Alternative(patterns1)(tpe)
  }

  private def getPrimitiveType(name: Name) given Context: Checked[Type] =
    if isPrimitive(name) then getType(name)
    else Err.nameNotConstructor(name)

  private def typedUnapply(functor: Name, args: List[Tree])
                          (pt: Type)
                          given Context, Mode, Stoup: Checked[Tree] = {
    for {
      fCtx <- firstCtx(functor)
      fTpe <- checked {
        implied for Context = fCtx
        getPrimitiveType(functor)
      }
      pair <- typeAsDestructor(fTpe, pt)
      (fTpeArgs, tpe) = pair
      args1 <- typeAsFunctorArgs(functor, args, fTpeArgs)
    } yield Unapply(functor, args1)(tpe)
  }

  private def typedLinearUnapply(functor: Name, args: List[Tree])
                                (pt: Type)
                                given Context, Mode, Stoup: Checked[Tree] = {

    def assertSingleArg(args: List[Tree]): Checked[Tree] = args match {
      case arg :: Nil => arg
      case _ => Err.linearUnapplyArgLengthGT1
    }

    for {
      arg  <- assertSingleArg(args)
      fCtx <- firstCtx(functor)
      fTpe <- checked {
        implied for Context = fCtx
        getPrimitiveType(functor)
      }
      pair <- typeAsLinearDestructor(fTpe, pt)
      (linearArg, tpe) = pair
      arg1 <- arg.typed(linearArg)
    } yield Unapply(functor, List(arg1))(tpe)
  }

  private def typePackaging(tree: Tree)
                           given Context: Checked[(Context, Tree)] = {

    def foldEmptyNamePairs(id: Id, name: Name, tail: List[(Id, Name)]) given Context = {
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
      case (id, name) :: tail => foldEmptyNamePairs(id, name, tail)
      case _                  => (ctx, EmptyTree)
    }
  }

  private def typedPackageDef(pid: Tree, stats: List[Tree])
                             (pt: Type)
                             given Context, Mode, Stoup: Checked[Tree] = {
    for {
      pair <- typePackaging(pid)
      (pkgCtx, pid1) = pair
      stats1 <- checked {
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
      ret      =  toBodyType(sig1, tpe)
      bodyCtx  <- lookIn(sig.id)
      newStoup =  sig1.linearArg.foldEmptyName(stoup)(DependsOn)
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
      for (_ <- lookForInLinear(arg))
      yield putType(arg, argTpe)

    case _ => Err.linearArgNotMatchType(arg)
  }

  private def typedDefSig(name: Name, args: List[Name])
                         (id: Id, pt: Type)
                         given Context: Checked[Tree] = {
    val pts = toCurriedList(pt)
    if pts.length <= args.length then {
      Err.declArgsNotMatchType(name)
    } else {
      lookIn(id).flatMap { bodyCtx =>
        implied for Context = bodyCtx
        for (_ <- mapArgs(args, pts))
        yield DefSig(name, args)(id, pt)
      }
    }
  }

  private def typedLinearSig(name: Name, args: List[Name], linear: Name)
                            (id: Id, pt: Type)
                            given Context: Checked[Tree] = {
    val pts = toCurriedList(pt)
    if pts.length <= args.length then {
      Err.declArgsNotMatchType(name)
    } else {
      lookIn(id).flatMap { bodyCtx =>
        implied for Context = bodyCtx
        for {
          _ <- mapArgs(args, pts)
          _ <- mapLinearArg(linear, pts.last)
        } yield LinearSig(name, args, linear)(id, pt)
      }
    }
  }

  private def typedSelectType given Mode: Checked[Tree] =
    Err.memberSelection

  private def typedSelectTerm given Mode: Checked[Tree] =
    Err.memberSelection

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
      Err.nameInPattAlt(name)
    } else {
      for {
        _ <- name.foldWildcard(()) { n =>
          for (_ <- assertLinearEntryCondition(n, pt))
          yield putType(n -> pt)
        }
      } yield Ident(name)(id, pt)
    }
  }

  private def typedIdentTerm(name: Name)
                            (id: Id, pt: Type)
                            given Context, Mode, Stoup: Checked[Tree] = {
    for {
      fstCtx   <- firstCtx(name)
      idRefTpe <- checked {
        implied for Context = fstCtx
        for {
          _        <- assertDependencyStoupCondition(name)
          tpe      <- getType(name)
          _        <- assertIdentStoupCondition(tpe, name)
        } yield tpe: Type
      }
    } yield Ident(name)(id, idRefTpe.freshVariables)
  }

  private def assertDependencyStoupCondition(x: Name)
                                            given Context, Stoup: Checked[Unit] = {
    if isLinear(x) && stoup != DependsOn(x) then
      Err.illegalStoupDependency(x)
    else
      ()
  }

  private def assertIdentStoupCondition(tpe: Type, name: Name) given Stoup: Checked[Unit] = {
    if tpe.isValueType then
      assertEmptyStoupCondition(Err.illegalStoupValueIdent(_, name, tpe))
    else
      ()
  }

  private def assertEmptyStoupCondition(msg: Name => CompilerError)
                              given Stoup: Checked[Unit] = stoup match {
    case Blank => ()
    case DependsOn(z) => msg(z)
  }

  def assertLinearEntryCondition(name: Name, tpe: Type)
                                given Context: Checked[Unit] = {
    if tpe.isValueType && isLinear(name) then {
      Err.illegalStoupEntry(name, tpe)
    } else {
      ()
    }
  }

  private def typedLiteral(constant: Constant) given Stoup: Checked[Tree] = {
    for {
      _ <- assertEmptyStoupCondition(Err.illegalStoupValueLiteral)
    } yield Literal(constant)(constantTpe(constant))
  }

  private def check(typed: Tree)(pt: Type) given Mode: Checked[Tree] = {
    inline def ignoreType(tree: Tree) = tree match {
      case EmptyTree | _: TreeSeq => true
      case _                      => false
    }
    if ignoreType(typed) || unifies(typed.tpe, pt) then
      typed
    else
      Err.typecheckFail(typed)(typed.tpe, pt)
  }

  private def (tree: Tree) typed
              (pt: Type)
              given Context, Mode, Stoup: Checked[Tree] = {

    def inner(tree: Tree, pt: Type) = tree match {
      // Types
      case _: Select                if isType     => typedSelectType
      case u @ Ident(n)             if isType     => typedIdentType(n)(u.id,pt)
      case Apply(t,ts)              if isType     => typedApplyType(t,ts)
      case InfixApply(f,a,b)        if isType     => typedInfixApply(f,a,b)
      case u @ Function(ts,t)       if isType     => typedFunctionType(ts,t)(u.id, pt)
      case u @ LinearFunction(a,b)  if isType     => typedLinearFunctionType(a,b)(u.id, pt)
      // Linear patterns
      case Unapply(f,ts)            if isLPattern => typedLinearUnapply(f,ts)(pt)
      // Patterns
      case u @ Ident(n)             if isPattern  => typedIdentPat(n)(u.id, pt)
      case Bind(n,t)                if isPattern  => typedBind(n,t)(pt)
      case Alternative(ts)          if isPattern  => typedAlternative(ts)(pt)
      case Unapply(f,ts)            if isPattern  => typedUnapply(f,ts)(pt)
      // Terms
      case PackageDef(t,ts)         if isTerm     => typedPackageDef(t,ts)(pt)
      case Apply(t,ts)              if isTerm     => typedApplyTerm(t,ts)(pt)
      case Eval(t,c)                if isTerm     => typedEvalTerm(t,c)(pt)
      case DefDef(                  // DefDef
        m,                          // DefDef
        s: (DefSig | LinearSig),    // DefDef
        t,                          // DefDef
        b)                          if isTerm     => typedDefDef(m,s,t,b)(pt)
      case u @ DefSig(n,ns)         if isTerm     => typedDefSig(n,ns)(u.id, pt)
      case u @ LinearSig(n,ns,l)    if isTerm     => typedLinearSig(n,ns,l)(u.id, pt)
      case u @ Let(n,v,c)           if isTerm     => typedLet(n,v,c)(u.id, pt)
      case u @ LetTensor(x,z,v,t)   if isTerm     => typedLetTensor(x,z,v,t)(u.id, pt)
      case u @ Function(ts,t)       if isTerm     => typedFunctionTerm(ts,t)(u.id, pt)
      case u @ LinearFunction(a,b)  if isTerm     => typedLinearFunctionTerm(a,b)(u.id, pt)
      case WhyNot(t)                if isTerm     => typedWhyNotTerm(t)(pt)
      case Bang(t)                  if isTerm     => typedBangTerm(t)(pt)
      case Tensor(t,u)              if isTerm     => typedTensorTerm(t,u)(pt)
      case Tagged(n,t)              if isTerm     => typedTagged(n,t)(pt)
      case CaseExpr(t,ts)           if isTerm     => typedCaseExpr(t,ts)(pt)
      case LinearCaseExpr(t,ts)     if isTerm     => typedLinearCaseExpr(t,ts)(pt)
      case _: Select                if isTerm     => typedSelectTerm
      case u @ Ident(n)             if isTerm     => typedIdentTerm(n)(u.id, pt)
      // Any
      case Literal(c)                             => typedLiteral(c)
      case Parens(ts)                             => typedParens(ts)(pt)
      case t @ ( _: TreeSeq
               | _: CaseClause
               |    EmptyTree)                    => t
      // Error
      case _                                      => Err.typingMissing(tree)
    }
    inner(tree, pt).map(check(_)(pt))
  }
}