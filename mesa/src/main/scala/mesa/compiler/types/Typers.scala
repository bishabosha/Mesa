package mesa
package compiler
package types

import scala.language.implicitConversions

import scala.collection.mutable
import scala.annotation.tailrec

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
import typed._
import Tree._
import TreeOps._
import error._
import types.{TyperErrors => Err}
import CompilerErrors._
import CompilerErrorOps._
import core.Contexts._
import Context._
import util.{Show, foldMap, eval}
import Mode._

import given NameOps._
import given TypeOps._
import given CompilerErrorOps._
import given TreeOps._

object Typers {
  import Stoup._
  import Closure._

  private val any = WildcardType

  private enum Stoup {
    case DependsOn(z: Name)
    case Blank
  }

  private object Stoup {
    def stoup given (stoup: Stoup): Stoup = stoup
  }

  private enum Closure { case Closed, Free }

  private object Closure {
    def closure given (closure: Closure): Closure = closure

    def (tpe: Type) resolveVariables given Closure =
      if closure == Free then
        tpe.mapTypeRefs(Variable(_))
      else
        tpe
  }

  def (tree: Tree) typed given Context: Lifted[Tree] = {
    given as Stoup = Blank
    given as Closure = Free
    val res = tree.typedAsExpr(any)
    resetLocalCtx
    res.doOnError(_ => removeFromCtx(tree.uniqId))
    res
  }

  private def (tree: Tree) typedAsExpr(pt: Type)
                                      given Context, Stoup, Closure: Lifted[Tree] = {
    given as Mode = Term
    tree.typed(pt)
  }

  private def (tree: Tree) typedAsTyping(pt: Type)
                                        given Context, Stoup, Closure: Lifted[Tree] = {
    given as Mode = Typing
    tree.typed(pt)
  }

  private def (tree: Tree) typedAsPattern(pt: Type)
                                         given Context, Stoup, Closure: Lifted[Tree] = {
    given as Mode = Pat
    tree.typed(pt)
  }

  private def (tree: Tree) typedAsLinearPattern(pt: Type)
                                               given Context, Stoup, Closure: Lifted[Tree] = {
    given as Mode = LinearPat
    tree.typed(pt)
  }

  private def (ts: List[Tree]) sameType(pt: Type): Lifted[Type] = ts match {
    case t :: ts =>
      unifiesThen[Lifted[Type]](t.tpe, pt)
        { t1Tpe =>
          if ts.forall(t => unifies(t.tpe, t1Tpe)) then t1Tpe
          else Err.tpesNotUnifyTo(pt)
        }
        { (_,_) => Err.tpesNotUnifyTo(pt) }

    case _ => EmptyType
  }

  private def resolveVariablesCtor(ctor: Name, data: Name, names: List[Name])
                                  (tpe: Type)
                                  given Context: Lifted[Type] = {
    import TypeOps._
    val assertThere = tpe.foldLeftLifted(()) { (_, t) =>
      dataDefinitionName(t) match {
        case `data`    => Err.recursiveData(ctor, data)

        case _ => t match {
          case TypeRef(name) =>
            if names.contains(name) then ()
            else Err.unresolvedVariable(ctor, data, name)

          case _ => ()
        }
      }
    }
    assertThere.map { _ =>
      tpe.mapTypeRefs { name =>
        if names.contains(name) then Variable(name)
        else TypeRef(name)
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

  private val checkFunctorWithProto: (functor: Name, fTpe: Type, proto: Type)
                                      => Lifted[Type] =
    checkFunctorWithProtoImpl((_,_) => true)(Err.functorNotMatch)

  private val checkFunctionWithProto: (function: Name, fTpe: Type, proto: Type)
                                        => Lifted[Type] =
    checkFunctorWithProtoImpl(unifyFunctionTypes)(Err.funcTpeNotMatch)

  private val checkLinearFunctionWithProto: (function: Name, fTpe: Type, proto: Type)
                                              => Lifted[Type] =
    checkFunctorWithProtoImpl(unifyLinearFunctionTypes)(Err.lfuncTpeNotMatch)

  private val checkFunWithProto: (fun: Tree, argProto: Type) =>
                                 (pt: Type) => Lifted[Type] = {
    checkFunWithProtoImpl {
      case FunctionType(arg,_) => arg
      case _                   => Err.noApplyNonFunctionType
    } (unwrapFunctionType)(Err.functionNotMatch)
  }

  private val checkLinearFunWithProto: (fun: Tree, argProto: Type) =>
                                       (pt: Type) => Lifted[Type] = {
    checkFunWithProtoImpl {
      case LinearFunctionType(arg,_) => arg
      case _                         => Err.noEvalNonLinearFunctionType
    } (unwrapLinearFunctionType)(Err.linearFunctionNotMatch)
  }

  private def checkFunctorWithProtoImpl(canUnify: (Type, Type) => Boolean)
                                       (onNoArgUnify: (Name, Type, Type, Type) => CompilerError)
                                       (fun: Name, fTpe: Type, proto: Type): Lifted[Type] = {
    if proto == WildcardType then {
      fTpe
    } else if canUnify(fTpe, proto) then {
      unifiesThen[Lifted[Type]](fTpe, proto)(identity)(
        onNoArgUnify(fun, fTpe, _, _))
    } else {
      Err.noApplyNonFunctionType
    }
  }

  private def checkFunWithProtoImpl(arg: Type => Lifted[Type])
                                   (unwrap: Type => (Type, Type))
                                   (onNoArgUnify: (Tree, Type, Type) => CompilerError)
                                   (fun: Tree, argProto: Type)
                                   (pt: Type): Lifted[Type] = {
    for
      arg         <- arg(fun.tpe)
      (arg1, ret) =  unwrap(fun.tpe.unifyFrom(arg)(argProto))
      argProto1   =  argProto.unify(arg1)
      subs        =  arg1.unifications(argProto1)
      arg2        =  arg1.unifyFromAll(subs)
      argProto2   =  argProto1.unifyFromAll(subs)
      ret1        <- lift {
        if arg2 =!= argProto2 then ret.unifyFromAll(subs).unify(pt)
        else onNoArgUnify(fun, arg, argProto1)
      }
    yield ret1
  }

  private def constantTpe(c: Constant): Type = c.asScala match {
    case _: BigDecimal => Bootstraps.DecimalType
    case _: BigInt     => Bootstraps.IntegerType
    case _: Char       => Bootstraps.CharType
    case _: String     => Bootstraps.StringType
    case _: Boolean    => Bootstraps.BooleanType
  }

  private def functionTermTpe(args1: List[Tree], body1: Tree): Lifted[Type] = {
    args1
      .map(_.tpe)
      .foldRight(body1.tpe)(FunctionType(_,_))
  }

  private def typedFunctionTerm(args: List[Tree], body: Tree)
                               (id: Id, pt: Type)
                               given Context, Mode, Stoup, Closure: Lifted[Tree] = {
    lookIn(id).flatMap { fCtx =>
      given as Context = fCtx
      for
        args1 <- args.mapE(_.typed(any))
        _ <- args1.foldLeftE(()) { (_, arg) =>
          val name: Name = arg
          name.foldEmptyName(())(assertStoupIsNot(_)(Err.illegalStoupLambda))
        }
        body1 <- lift {
          given as Closure = Closed
          body.typed(any)
        }
        fTpe  <- functionTermTpe(args1, body1)
        fTpe1 <- checkFunctionWithProto(EmptyName, fTpe, pt)
      yield Function(args1, body1)(id, fTpe1.resolveVariables)
    }
  }

  private def typedLinearFunctionTerm(arg: Tree, body: Tree)
                                     (id: Id, pt: Type)
                                     given Context, Mode, Stoup, Closure: Lifted[Tree] = {
    lookIn(id).flatMap { fCtx =>
      given as Context = fCtx
      for
        _     <- assertStoupEmpty(Err.illegalStoupLinearLambda)
        arg1  <- arg.typed(any)
        name  =  arg: Name
        _     <- assertIsLinear(name, arg1.tpe)
        body1 <- lift {
          given as Stoup   = DependsOn(name)
          given as Closure = Closed
          body.typed(any)
        }
        fTpe  <- linearFunctionTypnt(arg1, body1)
        fTpe1 <- checkLinearFunctionWithProto(EmptyName, fTpe, pt)
      yield LinearFunction(arg1, body1)(id, fTpe1.resolveVariables)
    }
  }

  private def functionTypnt(args1: List[Tree], body1: Tree)
                             given Mode: Lifted[Type] = {
    val argTpe = args1.map(_.tpe)
    FunctionType(argTpe, body1.tpe)
  }

  private def linearFunctionTypnt(arg1: Tree, body1: Tree)
                                   given Mode: Lifted[Type] = {
    if arg1.tpe.isValueType then
      Err.noCompArg
    else if body1.tpe.isValueType then
      Err.noLinearCompCodomain
    else
      LinearFunctionType(arg1.tpe, body1.tpe)
  }

  private def typedFunctionType(args: List[Tree], body: Tree)
                               (id: Id, pt: Type)
                               given Context, Mode, Stoup, Closure: Lifted[Tree] = {
    for
      args1 <- args.mapE(_.typed(any))
      body1 <- body.typed(any)
      fTpe  <- functionTypnt(args1, body1)
    yield Function(args1, body1)(id, fTpe)
  }

  private def typedLinearFunctionType(arg: Tree, body: Tree)
                                     (id: Id, pt: Type)
                                     given Context, Mode, Stoup, Closure: Lifted[Tree] = {
    for
      arg1  <- arg.typed(any)
      body1 <- body.typed(any)
      fTpe  <- linearFunctionTypnt(arg1, body1)
    yield LinearFunction(arg1, body1)(id, fTpe)
  }

  private def tensorTermTpe(value1: Tree, comp1: Tree): Lifted[Type] = {
    if comp1.tpe.isValueType then {
      Err.noTensorCompCodomain(comp1)
    } else {
      InfixAppliedType(
        TensorTag,
        bangTermTpe(value1),
        comp1.tpe
      )
    }
  }

  private def bangTermTpe(value1: Tree): Type =
    AppliedType(BangTag, value1.tpe :: Nil)

  private def typedBangTerm(t: Tree)
                           (pt: Type)
                           given Context, Mode, Stoup, Closure: Lifted[Tree] = {
    for
      _   <- assertStoupEmpty(Err.illegalStoupBang)
      t1  <- t.typed(any)
      tpe <- bangTermTpe(t1)
    yield Bang(t1)(tpe)
  }

  private def typedWhyNotTerm(t: Tree)
                             (pt: Type)
                             given Context, Mode, Stoup, Closure: Lifted[Tree] = {
    for
      t1  <- lift {
        given as Stoup = Blank
        t.typed(Bootstraps.VoidType)
      }
    yield WhyNot(t1)(pt)
  }

  private def typedTensorTerm(t: Tree, u: Tree)
                             (pt: Type)
                             given Context, Mode, Stoup, Closure: Lifted[Tree] = {
    for
      t1 <- lift {
        given as Stoup = Blank
        t.typed(any)
      }
      u1  <- u.typed(any)
      tpe <- tensorTermTpe(t1, u1)
    yield Tensor(t1, u1)(tpe)
  }

  private def typedTagged(arg: Name, tpeTree: Tree)
                         (pt: Type)
                         given Context, Mode, Stoup, Closure: Lifted[Tree] = {
    for tpeTree1 <- tpeTree.typedAsTyping(pt)
    yield {
      putTermType(arg -> tpeTree1.tpe)
      Tagged(arg, tpeTree1)(tpeTree1.tpe)
    }
  }

  private def typeAsTuple(ts: List[Tree], pt: Type)
                         given Context, Mode, Stoup, Closure: Lifted[List[Tree]] = {
    if pt == any then {
      ts.mapE(_.typed(any))
    } else {
      val ptAsTuple = pt: List[Type]
      if ts.length != ptAsTuple.length then
        Err.tupleNoMatch(pt, ptAsTuple)
      else
        ts.zip(ptAsTuple).mapE(_.typed(_))
    }
  }

  private def typedParens(ts: List[Tree])
                         (pt: Type)
                         given Context, Mode, Stoup, Closure: Lifted[Tree] = {
    ts.foldMap(unit) { ts =>
      for ts1 <- typeAsTuple(ts, pt)
      yield Parens(ts1)(ts1.map[Type, List[Type]](_.tpe))
    }
  }

  def unifyPattern(
      ctor: Name, ctorTpe: Type, sumTpe: Type): Lifted[(Name, List[Type])] =
    typeAsDestructor(ctor, ctorTpe, sumTpe).map(ctor -> _._1)

  private def unifyLinearPattern(
      ctor: Name, ctorTpe: Type, sumTpe: Type): Lifted[(Name, List[Type])] =
    typeAsLinearDestructor(ctor, ctorTpe, sumTpe).map(t => ctor -> (t._1.toList))

  private def typeAsDestructor(name: Name, fTpe: Type, pt: Type): Lifted[(List[Type], Type)] = {
    fTpe.toCurriedList.reverse match {
      case ret0 :: args0 =>
        val unifications  = ret0.unifications(pt)
        val ret           = ret0.unifyFromAll(unifications)
        val fTpeArgs      = args0.reverse.map(_.unifyFromAll(unifications))
        (fTpeArgs, ret)

      case _ => Err.emptyFunctor(name, fTpe)
    }
  }

  private def typeAsLinearDestructor(name: Name, fTpe: Type, pt: Type): Lifted[(Option[Type], Type)] = {
    fTpe.toCurriedList.reverse match {
      case Nil => Err.emptyLinearFunctor(name, fTpe)

      case linearFType :: Nil =>
        linearFType match {
          case LinearFunctionType(linearArg, ret) =>
            val unifications  = ret.unifications(pt)
            val ret1          = ret.unifyFromAll(unifications)
            val linearArg1    = linearArg.unifyFromAll(unifications)
            (Some(linearArg1), ret1)

          case ret => (None, ret.unify(pt))
        }

      case _ => Err.functionInLinearUnapply(name, fTpe)
    }
  }

  private def typeAsSumPatternArgs(
       functor: Name, ts: List[Tree], fTpeArgs: List[Type])
      given Context, Mode, Stoup, Closure: Lifted[List[Tree]] = {
    if ts.length != fTpeArgs.length then
      Err.sumCtorArgsNotMatchLengthPattern(functor, fTpeArgs.length, ts.length)
    else
      ts.zip(fTpeArgs).mapE(_.typed(_))
  }

  private def typedApplyType(functor: Tree, args: List[Tree])
                            given Context, Mode, Stoup, Closure: Lifted[Tree] = {
    for
      functor1 <- functor.typed(any)
      args1    <- args.mapE(_.typed(any))
      name     =  uniqName(functor1)
      funProto <- AppliedType(name, args1.map(_.tpe))
      tpe      <- checkFunctorWithProto(name, functor1.tpe, funProto)
    yield Apply(functor1, args1)(tpe)
  }

  private def typedInfixApply(functor: Tree, t: Tree, u: Tree)
                             given Context, Mode, Stoup, Closure: Lifted[Tree] = {
    for
      functor1 <- functor.typed(any)
      t1       <- t.typed(any)
      u1       <- u.typed(any)
      name     =  uniqName(functor1)
      funProto <- InfixAppliedType(name, t1.tpe, u1.tpe)
      tpe      <- checkFunctorWithProto(name, functor1.tpe, funProto)
    yield InfixApply(functor1, t1, u1)(tpe)
  }

  private def typedApplyTerm(fun: Tree, args: List[Tree])
                            (pt: Type)
                            given Context, Mode, Stoup, Closure: Lifted[Tree] = {
    for
      fun1  <- fun.typed(any)
      args1 <- lift {
        given as Stoup = Blank
        args.mapE(_.typed(any))
      }
      argsProto <- args1.map[Type, List[Type]](_.tpe): Type
      tpe       <- checkFunWithProto(fun1, argsProto)(pt)
    yield Apply(fun1, args1)(tpe)
  }

  private def typedEvalTerm(fun: Tree, arg: Tree)
                           (pt: Type)
                           given Context, Mode, Stoup, Closure: Lifted[Tree] = {
    for
      fun1 <- lift {
        given as Stoup = Blank
        fun.typed(any)
      }
      arg1 <- arg.typed(any)
      _    <- assertLinearFuncValid(fun1.tpe)
      _    <- assertCompEvalArg(arg1)
      tpe  <- checkLinearFunWithProto(fun1, arg1.tpe)(pt)
    yield Eval(fun1, arg1)(tpe)
  }

  private def unwrapCompApply(t: Tree): Lifted[Type] = t.tpe match {
    case AppliedType(BangTag, t :: Nil) => t
    case _                              => Err.noBangLetValue(t)
  }

  private def unwrapTensorType(t: Tree): Lifted[(Type, Type)] = {
    t.tpe match {
      case InfixAppliedType(
        TensorTag,
        AppliedType(BangTag, x :: Nil),
        z
      ) =>
        if z.isValueType then
          Err.noTensorCompCodomainTpe(z)
        else
          (x, z)

      case _ => Err.noTensorLetValue(t)
    }
  }

  private def singletonPattern(patt: Tree, selTpe: Type)
                              given Context, Mode, Stoup, Closure: Lifted[Tree] = {
    for
      patt1 <- patt.typedAsPattern(selTpe)
      _     <- assertExhaustiveSingleton(patt1, selTpe)(unifyPattern)
    yield patt1: Tree
  }

  private def singletonLinearPattern(patt: Tree, selTpe: Type)
                                    given Context, Mode, Stoup, Closure: Lifted[Tree] = {
    for
      patt1 <- patt.typedAsLinearPattern(selTpe)
      _     <- assertExhaustiveSingleton(patt1, selTpe)(unifyLinearPattern)
    yield patt1: Tree
  }

  private def typedLet(patt: Tree, t: Tree, u: Tree)
                      (id: Id, pt: Type)
                      given Context, Mode, Stoup, Closure: Lifted[Tree] = {
    for
      lCtx  <- lookIn(id)
      t1    <- t.typed(any)
      t1U   <- unwrapCompApply(t1)
      let1  <- lift {
        given as Context = lCtx
        for
          patt1 <- singletonPattern(patt, t1U)
          u1    <- lift {
            given as Stoup = Blank
            u.typed(pt)
          }
          _ <- assertLetCompContinuation(u1)
        yield Let(patt1, t1, u1)(id, u1.tpe)
      }
    yield let1
  }

  private def typedLetTensor(x: Tree, z: Tree, s: Tree, t: Tree)
                            (id: Id, pt: Type)
                            given Context, Mode, Stoup, Closure: Lifted[Tree] = {
    for
      lCtx   <- lookIn(id)
      s1     <- s.typed(any)
      s1U    <- unwrapTensorType(s1)
      tensor <- lift {
        given as Context = lCtx
        val (xTpe, zTpe)    = s1U
        for
          x1    <- singletonPattern(x, xTpe)
          z1    <- singletonLinearPattern(z, zTpe)
          stoup =  typedLinearVariable.foldEmptyName(Blank)(DependsOn(_))
          t1    <- lift {
            given as Stoup = stoup
            t.typed(pt)
          }
          _ <- assertLetCompContinuation(t1)
        yield LetTensor(x1, z, s1, t1)(id, t1.tpe)
      }
    yield tensor
  }

  private def typeAsCaseClauses given Context, Mode, Stoup, Closure:
      (ts: List[Tree], selTpe: Type) =>
      (pt: Type) =>
      (unify: (ctor: Name, ctorTpe: Type, sumTpe: Type)
                => Lifted[(Name, List[Type])]) =>
      Lifted[List[Tree]] = {

    inline def guardIdempotent(guard: Tree) =
      guard == EmptyTree || guard == litTrue

    typeAsGenCaseClauses { (selTpe, pt, ts1, remaining, c) =>
      c match {
        case t @ CaseClause(p,g,b) =>
          for c1 <- typedCaseClause(p, g, b, selTpe)(t.id, pt)
          yield (
            c1 :: ts1,
            remaining.filterNot(guardIdempotent(g) && unifiesWithTemplate(p)(_))
          )

        case unknown => Err.notCaseClause(unknown)
      }
    }
  }

  private def typeAsLinearCaseClauses given Context, Mode, Stoup, Closure:
      (ts: List[Tree], selTpe: Type) =>
      (pt: Type) =>
      (unify: (ctor: Name, ctorTpe: Type, sumTpe: Type)
                => Lifted[(Name, List[Type])]) =>
      Lifted[List[Tree]] = {
    typeAsGenCaseClauses { (selTpe, pt, ts1, remaining, c) =>
      c match {
        case t @ LinearCaseClause(p, b) =>
          for c1 <- typedLinearCaseClause(p,b,selTpe)(t.id, pt)
          yield (c1 :: ts1, remaining.filterNot(unifiesWithTemplate(p)))

        case unknown => Err.notLinearCaseClause(unknown)
      }
    }
  }

  private def typeAsGenCaseClauses(
      f: (selTpe: Type, pt: Type, ts1: List[Tree],
          remaining: List[Tree], c: Tree) => Lifted[(List[Tree], List[Tree])])
      (ts: List[Tree], selTpe: Type)
      (pt: Type)
      (unify: (ctor: Name, ctorTpe: Type, sumTpe: Type)
                => Lifted[(Name, List[Type])])
      given Context, Mode, Stoup, Closure: Lifted[List[Tree]] = {
    for
      templates <- templates(selTpe)(unify)
      pair      <- ts.foldLeftE(List.empty[Tree], templates) { (acc, c) =>
        val (ts1, remaining) = acc
        f(selTpe, pt, ts1, remaining, c)
      }
      (ts1, remaining) = pair
      _ <- assertNoneRemain(remaining)
    yield ts1
  }

  private def typedCaseExpr(selector: Tree, cases: List[Tree])
                           (pt: Type)
                           given Context, Mode, Stoup, Closure: Lifted[Tree] = {
    for
      selector1  <- lift {
        given as Stoup = Blank
        selector.typed(any)
      }
      cases1 <- typeAsCaseClauses(cases, selector1.tpe)(pt)(unifyPattern)
      tpe    <- cases1.sameType(pt)
    yield CaseExpr(selector1, cases1)(tpe)
  }

  private def typedCaseClause(pat: Tree, guard: Tree, body: Tree, selTpe: Type)
                             (id: Id, pt: Type)
                             given Context, Mode, Stoup, Closure: Lifted[Tree] = {
    lookIn(id).flatMap { ccCtx =>
      given as Context = ccCtx
      for
        pat1   <- pat.typedAsPattern(selTpe)
        guard1 <- guard.typedAsExpr(Bootstraps.BooleanType)
        body1  <- body.typedAsExpr(pt)
      yield CaseClause(pat1, guard1, body1)(id, body1.tpe)
    }
  }

  private def typedLinearCaseExpr(selector: Tree, cases: List[Tree])
                                 (pt: Type)
                                 given Context, Mode, Stoup, Closure: Lifted[Tree] = {
    for
      selector1 <- selector.typed(any)
      cases1    <- typeAsLinearCaseClauses(cases, selector1.tpe)(pt)(
                      unifyLinearPattern)
      tpe <- cases1.sameType(pt)
    yield LinearCaseExpr(selector1, cases1)(tpe)
  }

  private def typedLinearCaseClause(pat: Tree, body: Tree, selTpe: Type)
                                   (id: Id, pt: Type)
                                   given Context, Mode, Stoup, Closure: Lifted[Tree] = {
    lookIn(id).flatMap { ccCtx =>
      given as Context = ccCtx
      for
        pat1  <- pat.typedAsLinearPattern(selTpe)
        stoup =  typedLinearVariable.foldEmptyName(Blank)(DependsOn(_))
        body1 <- lift {
          given as Stoup = stoup
          body.typedAsExpr(pt)
        }
        _ <- assertLinearCaseCompContinuation(body1)
      yield LinearCaseClause(pat1, body1)(id, body1.tpe)
    }
  }

  private def typedBind(name: Name, body: Tree)
                       (pt: Type)
                       given Context, Mode, Stoup, Closure: Lifted[Tree] = {
    for body1 <- body.typed(pt)
    yield {
      if name == Name.Wildcard then {
        body1
      } else if mode == PatAlt then {
        Err.nameInPattAlt(name)
      } else {
        putTermType(name -> body1.tpe)
        Bind(name, body1)(body1.tpe)
      }
    }
  }

  private def typedAlternative(patterns: List[Tree])
                              (pt: Type)
                              given Context, Mode, Stoup, Closure: Lifted[Tree] = {
    given as Mode = Mode.PatAlt
    for
      patterns1 <- patterns.mapE(_.typed(pt))
      tpe       <- patterns1.sameType(pt)
    yield Alternative(patterns1)(tpe)
  }

  private def typedUnapply(constructor: Name, args: List[Tree])
                          (pt: Type)
                          given Context, Mode, Stoup, Closure: Lifted[Tree] = {
    for
      cTpe <- lookupConstructorType(constructor)
      pair <- typeAsDestructor(constructor, cTpe, pt)
      (fTpeArgs, tpe) = pair
      args1 <- typeAsSumPatternArgs(constructor, args, fTpeArgs)
    yield Unapply(constructor, args1)(tpe)
  }

  private def typedLinearUnapply(constructor: Name, args: List[Tree])
                                (pt: Type)
                                given Context, Mode, Stoup, Closure: Lifted[Tree] = {

    def assertMaxSingleArg(args: List[Tree]): Lifted[List[Tree]] = args match {
      case _ :: Nil | Nil => args
      case _ => Err.linearUnapplyArgLengthGT1
    }

    for
      args1 <- assertMaxSingleArg(args)
      cTpe  <- lookupConstructorType(constructor)
      pair  <- typeAsLinearDestructor(constructor, cTpe, pt)
      (linearArg, tpe) = pair
      args2 <- linearArg.mapE(tpe => args1.headOption.mapE(_.typed(tpe)))
    yield Unapply(constructor, args2.flatten.toList)(tpe)
  }

  private def typePackaging(tree: Tree)
                           given Context: Lifted[(Context, Tree)] = {

    def foldEmptyNamePairs(id: Id, name: Name, tail: List[(Id, Name)]) given Context = {
      for
        z <- for {
          next <- lookIn(id)
          tpe  <- declarePackage(rootName, name)
        } yield (next, Ident(name)(id, tpe))
        pair <- tail.foldLeftE(z)(accumulatePackages)
      yield pair
    }

    def accumulatePackages(z: (Context, Tree), current: (Id, Name)) = {
      val (ctx, parent)   = z
      val (id, name)      = current
      given as Context = ctx
      declareInParent(parent, id, name)
    }

    def declareInParent(parent: Tree, id: Id, name: Name) given Context = {
      for
        next <- lookIn(id)
        tpe  <- declarePackage(packageName(parent.tpe), name)
      yield (next, Select(parent, name)(id, tpe))
    }

    tree.toNamePairs match {
      case (id, name) :: tail => foldEmptyNamePairs(id, name, tail)
      case _                  => (ctx, EmptyTree)
    }
  }

  private def typedPackageDef(pid: Tree, stats: List[Tree])
                             (pt: Type)
                             given Context, Mode, Stoup, Closure: Lifted[Tree] = {
    for
      pair <- typePackaging(pid)
      (pkgCtx, pid1) = pair
      stats1 <- lift {
        given as Context = pkgCtx
        stats.mapE(_.typed(any))
      }
    yield PackageDef(pid1, stats1)(pid1.tpe)
  }

  private def typedDataDcl(name: Name, args: List[Name], ctors: List[Tree])
                          given Context, Mode, Stoup, Closure: Lifted[Tree] = {
    val tpe = AppliedType(name, args.map(Variable(_)))
    putDataType(name -> tpe)
    for ctors1 <- typeAsCtors(ctors)(name, args, tpe)
    yield DataDcl(name, args, ctors1)(tpe)
  }

  private def typedInfixDataDcl(name: Name, left: Name, right: Name, ctors: List[Tree])
                               given Context, Mode, Stoup, Closure: Lifted[Tree] = {
    val tpe = InfixAppliedType(name, Variable(left), Variable(right))
    putDataType(name -> tpe)
    for ctors1 <- typeAsCtors(ctors)(name, left :: right :: Nil, tpe)
    yield InfixDataDcl(name, left, right, ctors1)(tpe)
  }

  private def typeAsCtors(ctors: List[Tree])
                         (dataType: Name, tpeVars: List[Name], ret: Type)
                         given Context, Mode, Stoup, Closure: Lifted[List[Tree]] = {
    ctors.mapE {
      case CtorSig(name, args) => typedCtor(name, args)(dataType, tpeVars, ret)

      case LinearCtorSig(name, arg) =>
        typedLinearCtor(name, arg)(dataType, tpeVars, ret)

      case unknown => Err.notGenCtorSig(unknown)
    }
  }

  private def typedCtor(constructor: Name, args: List[Tree])
                       (dataType: Name, tpeVars: List[Name], ret: Type)
                       given Context, Mode, Stoup, Closure: Lifted[Tree] = {
    for
      args1 <- args.mapE(_.typedAsTyping(any))
      args2 <- args1.mapE { a =>
        resolveVariablesCtor(constructor, dataType, tpeVars)(a.tpe)
      }
      cTpe =  toFunctionType(args2 :+ ret)
      pair <- typeAsDestructor(constructor, cTpe, ret)
      (fTpeArgs, tpe1) = pair
      cTpe1 =  toFunctionType(fTpeArgs :+ tpe1)
      args2 <- lift(args1.zip(fTpeArgs).map(_.withTpe(_)))
    yield {
      putTermType(constructor -> cTpe1)
      linkConstructor(constructor, cTpe1)
      CtorSig(constructor, args2)(cTpe1)
    }
  }

  private def assertLinearCtorIsComp(
      name: Name, data: Name, arg: Type): Lifted[Unit] = {
    if arg.isValueType then
      Err.noCompArgCtor(name, data, arg)
    else
      ()
  }

  private def typedLinearCtor(constructor: Name, arg: Option[Tree])
                             (dataType: Name, tpeVars: List[Name], ret: Type)
                             given Context, Mode, Stoup, Closure: Lifted[Tree] = {
    arg.fold {
      putTermType(constructor -> ret)
      linkConstructor(constructor, ret)
      LinearCtorSig(constructor, None)(ret)
    }{ arg =>
      for
        arg1 <- arg.typedAsTyping(any)
        _    <- assertLinearCtorIsComp(constructor, dataType, arg1.tpe)
        argT <- resolveVariablesCtor(constructor, dataType, tpeVars)(arg1.tpe)
        cTpe =  LinearFunctionType(argT, ret)
        pair <- typeAsLinearDestructor(constructor, cTpe, ret)
        (Some(fTpeArg), tpe1) = pair
        cTpe1 =  LinearFunctionType(fTpeArg, tpe1)
        arg2  <- arg1.withTpe(fTpeArg)
      yield {
        putTermType(constructor -> cTpe1)
        linkConstructor(constructor, cTpe1)
        LinearCtorSig(constructor, Some(arg2))(cTpe1)
      }
    }
  }

  private def typedDefDef(
       modifiers: Set[Modifier], sig: Tree & Unique, tpeD: Tree, body: Tree)
      given Context, Mode, Stoup, Closure: Lifted[Tree] = {
    for
      tpeD1    <- tpeD.typedAsTyping(any)
      tpe      =  tpeD1.tpe
      sig1     <- sig.typed(tpe)
      ret      =  toBodyType(sig1, tpe)
      bodyCtx  <- lookIn(sig.id)
      newStoup =  sig1.linearArg.foldEmptyName(stoup)(DependsOn(_))
      body1 <- lift {
        given as Context = bodyCtx
        given as Stoup   = newStoup
        given as Closure = Closed
        body.typed(ret)
      }
    yield {
      val freshTpe = tpe.resolveVariables
      putTermType(sig1, freshTpe)
      DefDef(modifiers, sig1, tpeD1, body1)(freshTpe)
    }
  }

  private def mapArgs(args: List[Name], pts: List[Type])
                     given Context: Lifted[Unit] = {
    args
      .zip(pts)
      .foldLeftE(()) { (_, pair) =>
        val (name, tpe) = pair
        for _ <- lookForInScope(name)
        yield {
          putTermType(name, tpe)
        }
      }
  }

  private def mapLinearArg(arg: Name, tpe: Type)
                          given Context: Lifted[Unit] = tpe match {
    case t @ LinearFunctionType(argTpe, _) =>
      for
        _ <- assertLinearFuncValid(t)
        _ <- lookForInLinear(arg)
      yield {
        putTermType(arg, argTpe)
      }

    case _ => Err.linearArgNotMatchType(arg)
  }

  private def typedDefSig(name: Name, args: List[Name])
                         (id: Id, pt: Type)
                         given Context: Lifted[Tree] = {
    val pts = pt.toCurriedList
    if pts.length <= args.length then {
      Err.declArgsNotMatchType(name)
    } else if name.isOperator && pts.length < 3 then {
      Err.declArgsInfixNotBinary(name)
    } else {
      lookIn(id).flatMap { bodyCtx =>
        given as Context = bodyCtx
        for _ <- mapArgs(args, pts)
        yield DefSig(name, args)(id, pt)
      }
    }
  }

  private def typedLinearSig(name: Name, args: List[Name], linear: Name)
                            (id: Id, pt: Type)
                            given Context: Lifted[Tree] = {
    val pts = pt.toCurriedList
    if pts.length <= args.length then {
      Err.declArgsNotMatchType(name)
    } else if name.isOperator && pts.length < 2 then {
      Err.declArgsInfixLNotBinary(name)
    } else {
      lookIn(id).flatMap { bodyCtx =>
        given as Context = bodyCtx
        for
          _ <- mapArgs(args, pts)
          _ <- mapLinearArg(linear, pts.last)
        yield LinearSig(name, args, linear)(id, pt)
      }
    }
  }

  private def typedSelectType given Mode: Lifted[Tree] =
    Err.memberSelection

  private def typedSelectTerm given Mode: Lifted[Tree] =
    Err.memberSelection

  private def typedIdentType(name: Name)
                            (id: Id)
                            given Context: Lifted[Tree] = {
    val tpe = lookupType(name).fold(_ => TypeRef(name))(identity)
    Ident(name)(id, tpe)
  }

  private def typedIdentPat(name: Name)
                           (id: Id, pt: Type)
                           given Context, Mode: Lifted[Tree] = {
    if mode == PatAlt && name != Wildcard then {
      Err.nameInPattAlt(name)
    } else {
      for _ <- name.foldWildcard(()) { n =>
        for _ <- assertIsLinear(n, pt)
        yield {
          putTermType(n -> pt)
        }
      }
      yield Ident(name)(id, pt)
    }
  }

  private def typedIdentTerm(name: Name)
                            (id: Id)
                            given Context, Mode, Stoup, Closure: Lifted[Tree] = {
    for
      tpe <- inTermCtx(name) {
        for
          tpe <- termType(name)
          _   <- assertInScope(name, tpe)
        yield tpe: Type
      }
    yield Ident(name)(id, tpe.freshVariables)
  }

  private def assertInScope(name: Name, tpe: Type)
                           given Context, Stoup, Closure, Mode: Lifted[Unit] = {
    if isLinear(name) then
      assertStoupIs(name)(Err.illegalStoupDependency)
    else
      assertStoupEmpty(Err.illegalStoupValueIdent(_, name, tpe))
  }

  private def assertStoupEmpty(err: Name => CompilerError)
                              given Stoup: Lifted[Unit] = stoup match {
    case DependsOn(z) => err(z)
    case _            => ()
  }

  private def assertStoupIs(name: Name)(err: Name => CompilerError)
                           given Stoup: Lifted[Unit] = stoup match {
    case DependsOn(`name`) => ()
    case _                 => err(name)
  }

  private def assertStoupIsNot(name: Name)(err: Name => CompilerError)
                              given Stoup, Context: Lifted[Unit] = stoup match {
    case DependsOn(`name`) => err(name)
    case _                 => ()
  }

  private def assertIsLinear(name: Name, tpe: Type) given Context: Lifted[Unit] = {
    if tpe.isValueType && isLinear(name) then
      Err.illegalStoupEntry(name, tpe)
    else
      ()
  }

  private def assertNoneRemain(templates: List[Tree]): Lifted[Unit] = {
    if templates.nonEmpty then
      Err.nonExhaustivePatterns(templates)
    else
      ()
  }

  private def assertLinearFuncValid(fun1: Type): Lifted[Unit] = fun1 match {
    case LinearFunctionType(t1, t2) =>
      if t1.isValueType then
        Err.noCompArg
      else if t2.isValueType then
        Err.noLinearCompCodomain
      else
        ()

    case _ => ()
  }

  private def assertCompEvalArg(arg1: Tree): Lifted[Unit] = {
    if arg1.tpe.isComputationType then ()
    else Err.noCompEvalArg
  }

  private def assertLetCompContinuation(cont1: Tree): Lifted[Unit] = {
    if cont1.tpe.isComputationType then ()
    else Err.noCompLetContinuation
  }

  private def assertLinearCaseCompContinuation(cont1: Tree): Lifted[Unit] = {
    if cont1.tpe.isComputationType then ()
    else Err.noCompLinearCaseContinuation
  }

  private def assertExhaustiveSingleton(
       patt1: Tree, selTpe: Type)
      (unify: (ctor: Name, ctorTpe: Type, sumTpe: Type)
                => Lifted[(Name, List[Type])])
      given Context: Lifted[Unit] = patt1 match {
    case _: Ident => ()

    case _ =>
      for
        templates <- templates(selTpe)(unify)
        remaining <- lift(templates.filterNot(unifiesWithTemplate(patt1)))
        _         <- assertNoneRemain(remaining)
      yield ()
  }

  def unifiesWithTemplate(pattern: Tree)(template: Tree): Boolean = {

    @tailrec
    def inner(pss: List[List[Tree]], tss: List[List[Tree]]): Boolean = (pss, tss) match {
      case (Nil::pss,Nil::tss) => true

      case ((p::ps)::pss, (t::ts)::tss) => (p,t) match {
        case (Ident(_),_) => inner(ps::pss,ts::tss)

        case (Literal(True),Literal(True)) | (Literal(False),Literal(False)) =>
          inner(ps::pss,ts::tss)

        case (Literal(_),_) => inner(pss,(t::ts)::tss)

        case (Parens(ps1),Parens(ts1)) =>
          if ps1.size == ts1.size then
            inner((ps1:::ps)::pss,(ts1:::ts)::tss)
          else
            inner(pss,(t::ts)::tss)

        case (Unapply(op1, ps1),Unapply(op2,ts1)) =>
          if op1 == op2 then
            inner((ps1:::ps)::pss,(ts1:::ts)::tss)
          else
            inner(pss,(t::ts)::tss)

        case (Bind(_, p),_) => inner((p::ps)::pss, (t::ts)::tss)

        case (Alternative(ps1),_) =>
          inner(
            ps1.map(_::ps) ::: pss,
            List.fill(ps1.length)(t::ts) ::: tss
          )

        case _ => inner((p::ps)::pss, tss)
      }
      case _ => false
    }
    inner((pattern::Nil)::Nil, (template::Nil)::Nil)
  }

  def templates(
       selTpe: Type)
      (unify: (ctor: Name, ctorTpe: Type, sumTpe: Type) => Lifted[(Name, List[Type])])
      given Context: Lifted[List[Tree]] = {

    type StackT = List[Tree]
    type StatT  = StackT => StackT
    type ProgT  = List[StatT]

    val constructors = new mutable.HashMap[Name, List[(Name, Type)]]()

    def cacheConstructors(name: Name) = {
      constructors.getOrElse(name, {
        for ctors <- lookupConstructors(name)
        yield {
          constructors += name -> ctors
          ctors
        }
      })
    }

    def constructorsForSum(name: Name, sumTpe: Type) = {
      for
        ctors   <- cacheConstructors(name)
        unified <- ctors.mapE(unify(_, _, sumTpe))
      yield unified
    }

    @tailrec
    def inner(
        acc: List[Tree],
        programs: List[ProgT],
        selTpess: List[List[Type]]): Lifted[List[Tree]] = selTpess match {
      case Nil => acc

      case selTpes::selTpess => selTpes match {

        case Nil =>
          val program::programs1 = programs
          val templates = program.foldLeft(List.empty[Tree])(eval)
          inner(templates:::acc, programs1, selTpess)

        case selTpe::selTpes => selTpe match {

          case Product(ts) =>
            val program::programs1 = programs
            val mkParens: StatT = { stack =>
              ts.foldMap(unit::stack) { ts =>
                val (ts1, stack1) = stack.splitAt(ts.size)
                Parens(ts1)(EmptyType)::stack1
              }
            }
            inner(
              acc,
              (mkParens::program)::programs1,
              (ts:::selTpes)::selTpess
            )

          case BaseType(BooleanTag) =>
            val program::programs1 = programs
            val putTrue: StatT = { stack =>
              litTrue::stack
            }
            val putFalse: StatT = { stack =>
              litFalse::stack
            }
            val programs2 = (putFalse::program)::(putTrue::program)::programs1
            inner(
              acc,
              programs2,
              selTpes :: selTpes :: selTpess
            )

          case _ => dataDefinitionName(selTpe) match {

            case EmptyName =>
              val program::programs1 = programs
              val putIdent: StatT = { stack =>
                Ident(Wildcard)(Id.empty, selTpe) :: stack
              }
              inner(
                acc,
                (putIdent::program)::programs1,
                selTpes::selTpess
              )

            case sumName =>
              val program :: programs1 = programs
              constructorsForSum(sumName, selTpe) match {
                case err: CompilerError => err

                case constructors0 =>
                  val constructors = unlift(constructors0)
                  val mkCtors = {
                    for (name, args) <- constructors
                    yield {
                      val mkCtor: StatT = { stack =>
                        val (args1, stack1) = stack.splitAt(args.length)
                        Unapply(name, args1)(any)::stack1
                      }
                      mkCtor::program
                    }
                  }
                  val ctorArgLists = constructors.map(_._2:::selTpes)
                  inner(
                    acc,
                    mkCtors:::programs1,
                    ctorArgLists:::selTpess
                  )
              }
          }
        }
      }
    }
    inner(Nil, Nil :: Nil, (selTpe :: Nil) :: Nil)
  }

  private def typedLiteral(constant: Constant) given Stoup: Lifted[Tree] = {
    for _ <- assertStoupEmpty(Err.illegalStoupValueLiteral)
    yield constant match {
      case True  => litTrue
      case False => litFalse
      case _     => Literal(constant)(constantTpe(constant))
    }
  }

  private def check(typed: Tree)(pt: Type) given Mode: Lifted[Tree] = {
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
              given Context, Mode, Stoup, Closure: Lifted[Tree] = {

    def inner(tree: Tree, pt: Type) = tree match {
      // Types
      case _: Select                if isType     => typedSelectType
      case u @ Ident(n)             if isType     => typedIdentType(n)(u.id)
      case Apply(t,ts)              if isType     => typedApplyType(t,ts)
      case InfixApply(f,a,b)        if isType     => typedInfixApply(f,a,b)
      case u @ Function(ts,t)       if isType     => typedFunctionType(ts,t)(u.id,pt)
      case u @ LinearFunction(a,b)  if isType     => typedLinearFunctionType(a,b)(u.id,pt)
      // Linear patterns
      case Unapply(f,ts)            if isLPattern => typedLinearUnapply(f,ts)(pt)
      // Patterns
      case u @ Ident(n)             if isPattern  => typedIdentPat(n)(u.id,pt)
      case Bind(n,t)                if isPattern  => typedBind(n,t)(pt)
      case Alternative(ts)          if isPattern  => typedAlternative(ts)(pt)
      case Unapply(f,ts)            if isPattern  => typedUnapply(f,ts)(pt)
      // Terms
      case PackageDef(t,ts)         if isTerm     => typedPackageDef(t,ts)(pt)
      case Apply(t,ts)              if isTerm     => typedApplyTerm(t,ts)(pt)
      case Eval(t,c)                if isTerm     => typedEvalTerm(t,c)(pt)
      case DataDcl(n,a,c)           if isTerm     => typedDataDcl(n,a,c)
      case InfixDataDcl(n,l,r,c)    if isTerm     => typedInfixDataDcl(n,l,r,c)
      case DefDef(                  // DefDef
        m,                          // DefDef
        s: (DefSig | LinearSig),    // DefDef
        t,                          // DefDef
        b)                          if isTerm     => typedDefDef(m,s,t,b)
      case u @ DefSig(n,ns)         if isTerm     => typedDefSig(n,ns)(u.id,pt)
      case u @ LinearSig(n,ns,l)    if isTerm     => typedLinearSig(n,ns,l)(u.id,pt)
      case u @ Let(p,v,c)           if isTerm     => typedLet(p,v,c)(u.id, pt)
      case u @ LetTensor(x,z,v,t)   if isTerm     => typedLetTensor(x,z,v,t)(u.id,pt)
      case u @ Function(ts,t)       if isTerm     => typedFunctionTerm(ts,t)(u.id,pt)
      case u @ LinearFunction(a,b)  if isTerm     => typedLinearFunctionTerm(a,b)(u.id,pt)
      case WhyNot(t)                if isTerm     => typedWhyNotTerm(t)(pt)
      case Bang(t)                  if isTerm     => typedBangTerm(t)(pt)
      case Tensor(t,u)              if isTerm     => typedTensorTerm(t,u)(pt)
      case Tagged(n,t)              if isTerm     => typedTagged(n,t)(pt)
      case CaseExpr(t,ts)           if isTerm     => typedCaseExpr(t,ts)(pt)
      case LinearCaseExpr(t,ts)     if isTerm     => typedLinearCaseExpr(t,ts)(pt)
      case _: Select                if isTerm     => typedSelectTerm
      case u @ Ident(n)             if isTerm     => typedIdentTerm(n)(u.id)
      // Any
      case Literal(c)                             => typedLiteral(c)
      case Parens(ts)                             => typedParens(ts)(pt)
      case t @ (_: TreeSeq | EmptyTree)           => t
      // Error
      case _                                      => Err.typingMissing(tree)
    }
    inner(tree, pt).map(check(_)(pt))
  }
}