package eec
package compiler
package types

object Types {
  import collection.generic._
  import collection.mutable._
  import annotation._
  import core.Names
  import Names._
  import Name._
  import Type._
  import ast.Trees._

  object Bootstraps {
    val BooleanType = TypeRef(BooleanTag)
    val DecimalType = TypeRef(DecimalTag)
    val IntegerType = TypeRef(IntegerTag)
    val CharType    = TypeRef(CharTag)
    val StringType  = TypeRef(StringTag)
  }

  enum Type derives Eql {
    case PackageInfo(parent: Type, name: Name)
    case TypeRef(name: Name)
    case Variable(name: Name)
    case FunctionType(arg: Type, body: Type)
    case Product(args: List[Type])
    case AppliedType(typ: Type, args: List[Type])
    case WildcardType
    case Untyped
    case EmptyType
  }

  opaque type TypeVariableOps = Type

  private object TypeVariableOps {
    import TypeOps._

    def apply(tpe: Type): TypeVariableOps = tpe

    def (ops: TypeVariableOps) zipWith[O, That](tpe: Type)
        (f: (Name, Type) => O)
        given (bf: CanBuild[O, That]): That = {

      val b = ops.zipFold(tpe)(bf()) { (acc, arg, app) =>
        arg match {
          case Variable(name) => acc += f(name, app)
          case _              => acc
        }
      }

      b.result
    }

    def unifyImpl(tpe: Type, sub: Name, by: Type): Type = tpe.mapVariables {
      case `sub` =>
        if by == WildcardType then
          Variable(sub)
        else
          by
      case other =>
        Variable(other)
    }

    def (tpe: TypeVariableOps) mapVariables(f: Name => Type): Type = {

      type Program = List[Type] => List[Type]

      inline def program(f: Program) = f

       def compile(tpe: Type): List[Program] =
        tpe.foldLeft[List[Program]](Nil) { (programs, tpe) =>
          tpe match {
            case FunctionType(arg, body) =>
              val prog = program { stack =>
                val a1 :: a2 :: rest = stack
                FunctionType(a1, a2) :: rest
              }
              prog :: programs
            case AppliedType(f, args) =>
              val prog = program { stack =>
                val (removed, rest) = stack.splitAt(args.length + 1)
                AppliedType(removed.head, removed.tail) :: rest
              }
              prog :: programs
            case Product(tpes1) =>
              val prog = program { stack =>
                val (removed, rest) = stack.splitAt(tpes1.length)
                Product(removed) :: rest
              }
              prog :: programs
            case Variable(name) =>
              program(f(name) :: _) :: programs
            case _ =>
              program(tpe :: _) :: programs
          }
        }

      @tailrec
      def unsafeInterpret(stack: List[Type], programs: List[Program]): Type =
        programs match {
          case p :: ps  => unsafeInterpret(p(stack), ps)
          case Nil      => stack.head
        }

      unsafeInterpret(Nil, compile(tpe))
    }
  }

  object TypeOps {

    import collection._
    import util.Showable
    import Type._
    import Tree._
    import util.|>

    def (tpe: Type) foldLeft[O]
        (seed: O)
        (f: (O, Type) => O): O = {
      @tailrec
      def inner(acc: O, tpes: List[Type]): O = tpes match {
        case Nil => acc
        case tpe :: rest =>
          tpe match {
            case AppliedType(t, tpes) =>
              inner(f(acc, tpe), t :: tpes ::: rest)
            case FunctionType(a1, b1) =>
              inner(f(acc, tpe), a1 :: b1 :: rest)
            case Product(tpes) =>
              inner(f(acc, tpe), tpes ::: rest)
            case _ =>
              inner(f(acc, tpe), rest)
          }
      }
      inner(seed, tpe :: Nil)
    }

    def (tpe: Type) zipFold[O, That](app: Type)(seed: O)
        (f: (O, Type, Type) => O): O = {

      @tailrec
      def inner(
          acc: O,
          args: List[Type],
          apps: List[Type]): O = args match {
        case Nil => acc
        case arg :: argRest => apps match {
          case Nil => acc
          case app :: appsRest =>
            if canUnify(arg, app) then
              (arg, app) match {
                case (AppliedType(functor, args1), AppliedType(g, args2)) =>
                  inner(f(acc, arg, app), functor :: args1 ::: argRest, g :: args2 ::: appsRest)
                case (FunctionType(a1, b1), FunctionType(a2, b2)) =>
                  inner(f(acc, arg, app), a1 :: b1 :: argRest, a2 :: b2 :: appsRest)
                case (Product(tpes1), Product(tpes2)) =>
                  inner(f(acc, arg, app), tpes1 ::: argRest, tpes2 ::: appsRest)
                case _ =>
                  inner(f(acc, arg, app), argRest, appsRest)
              }
            else
              acc
        }
      }

      if WildcardType == app || WildcardType == tpe then
        seed
      else
        inner(seed, tpe :: Nil, app :: Nil)
    }

    def canUnify(arg: Type, app: Type): Boolean = (arg, app) match {
      case (Product(tpes1), Product(tpes2)) =>
        tpes1.size == tpes2.size
      case (Variable(_: Comp), _) =>
        app.isComputationType
      case (AppliedType(_, args1), AppliedType(_, args2)) =>
        args1.size == args2.size
      case ( _: FunctionType,   _: FunctionType )
      |    ( _: TypeRef,        _: TypeRef )
      |    ( _: Variable
           |    WildcardType,   _               )
      |    ( _: PackageInfo,    _: PackageInfo  )
      |    (    EmptyType,         EmptyType    )
      |    (    Untyped,           Untyped      ) =>
        true
      case _ =>
        false
    }

    def (tpe: Type) =!= (other: Type): Boolean =
      tpe   == other        ||
      tpe   == WildcardType ||
      other == WildcardType

    def packageName(t: Type): Name = t match {
      case PackageInfo(_, name) => name
      case _                    => EmptyName
    }

    def toCurriedList(t: Type): List[Type] = {
      @tailrec
      def inner(acc: List[Type], t: Type): List[Type] = t match {
        case FunctionType(arg, body)  => inner(arg :: acc, body)
        case _                        => t :: acc
      }
      inner(Nil, t).reverse
    }

    def toReturnType(defSig: Tree, t: Type): Type =
      defSig match {
        case DefSig(_, args) =>
          toFunctionType(toCurriedList(t).drop(args.length))
        case _ =>
          EmptyType
      }

    def toFunctionType(ts: List[Type]): Type =
      ts.reverse match {
        case t :: rest  => rest.foldLeft(t)((acc, t1) => FunctionType(t1, acc))
        case Nil        => EmptyType
      }

    def (tpe: Type) unifications(subWith: Type): List[(Name, Type)] = {
      import TypeVariableOps._
      TypeVariableOps(tpe).zipWith(subWith)((_, _))
    }

    def (tpe: Type) unifyFrom(subFrom: Type)(subWith: Type): Type = {
      import TypeVariableOps._
      subFrom.zipFold(subWith)(tpe) { (acc, arg, sub) =>
        arg match {
          case Variable(name) => unifyImpl(acc, name, sub)
          case _              => acc
        }
      }
    }

    inline def (tpe: Type) unify(subWith: Type) =
      tpe.unifyFrom(tpe)(subWith)

    def (tpe: Type) unifyFromAll(unifications: Iterable[(Name, Type)]): Type =
      unifications.foldLeft(tpe) { (acc, pair) =>
        import TypeVariableOps._
        val (sub, by) = pair
        unifyImpl(acc, sub, by)
      }

    def (tpe: Type) replaceVariable(from: Name)(by: Name): Type = {
      import TypeVariableOps._
      unifyImpl(tpe, from, Variable(by))
    }

    def (tpe: Type) replaceVariables(f: Name => Option[Name]): Type = {
      import TypeVariableOps._
      var seen = Set[Name]()
      tpe.foldLeft(tpe) { (acc, tpe) =>
        tpe match {
          case Variable(n) if !seen.contains(n) =>
            seen += n
            f(n).map(acc.replaceVariable(n)(_))
                .getOrElse(acc)
          case _ =>
            acc
        }
      }
    }

    def (tpe: Type) isComputationType: Boolean = {
      @tailrec
      def inner(acc: Boolean, tpes: List[Type]): Boolean = tpes match {
        case Nil => acc
        case tpe :: tpes => tpe match {
          case AppliedType(TypeRef(ComputationTag), List(_))
          |    TypeRef(_: Comp)
          |    Variable(_: Comp)      => true
          case FunctionType(_, body)  => inner(acc, body :: tpes)
          case Product(ts)            => inner(acc, ts ::: tpes)
          case _                      => false
        }
      }
      inner(true, tpe :: Nil)
    }

    implied for Showable[Type] {
      import implied NameOps._

      def (tpe: Type) show: String = {
        inline def packaged(t: Type): String = {
          @tailrec
          def packageNamed(acc: List[Name], tpe: Type): List[Name] =
            tpe match {
              case PackageInfo(parent, name) =>
                packageNamed(name :: acc, parent)
              case _ =>
                acc
            }
          packageNamed(Nil, t)
            .map(_.show)
            .mkString("package ", ".", "")
        }
        
        inline def applied(t: Type, ts: List[Type]): String = {
          if ts.isEmpty then
            t.show
          else {
            val args = ts.map {
              case f: (FunctionType | AppliedType) =>
                val str = f.show
                s"($str)"
              case f =>
                f.show
            }
            val functorStr = t.show
            val argsStr = args.mkString(" ")
            s"$functorStr $argsStr"
          }
        }

        inline def fromFunction(arg: Type, body: Type): String = arg match {
          case _: FunctionType  => s"(${arg.show}) -> ${body.show}"
          case _                => s"${arg.show} -> ${body.show}"
        }

        tpe match {
          case _: PackageInfo =>
            packaged(tpe)
          case FunctionType(arg, body) =>
            fromFunction(arg, body)
          case Product(ts) =>
            ts.map(_.show).mkString("(", ", ", ")")
          case AppliedType(t, ts) =>
            applied(t, ts)
          case TypeRef(t) =>
            t.show
          case Variable(t) =>
            t.show
          case WildcardType =>
            "<anytype>"
          case Untyped =>
            "<untyped>"
          case EmptyType =>
            "<emptytype>"
        }
      }
    }

    implied for (List[Type] |> Type) {
      def apply(ts: List[Type]) = ts match {
        case tpe :: Nil => tpe
        case types      => Product(types)
      }
    }

    implied for (Type |> List[Type]) {
      def apply(t: Type) = t match {
        case Product(ls)  => ls
        case tpe          => tpe :: Nil
      }
    }

    import Tree._
    def (tree: Tree) withType(tpe: Type): Tree = tree match {
      case t: Select        => t.copy()(t.id, tpe)
      case t: Ident         => t.copy()(t.id, tpe)
      case t: PackageDef    => t.copy()(t.id, tpe)
      case t: DefDef        => t.copy()(t.id, tpe)
      case t: DefSig        => t.copy()(t.id, tpe)
      case t: Apply         => t.copy()(t.id, tpe)
      case t: Function      => t.copy()(t.id, tpe)
      case t: Let           => t.copy()(t.id, tpe)
      case t: Literal       => t.copy()(t.id, tpe)
      case t: CaseExpr      => t.copy()(t.id, tpe)
      case t: CaseClause    => t.copy()(t.id, tpe)
      case t: Alternative   => t.copy()(t.id, tpe)
      case t: Parens        => t.copy()(t.id, tpe)
      case t: Bind          => t.copy()(t.id, tpe)
      case t: Unapply       => t.copy()(t.id, tpe)
      case t: Tagged        => t.copy()(t.id, tpe)
      case t: TreeSeq       => t
      case EmptyTree        => EmptyTree
    }
  }
}