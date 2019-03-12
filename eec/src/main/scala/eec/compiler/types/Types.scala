package eec
package compiler
package types

object Types {

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
    case UntypedExpect(typ: Type)
    case Untyped
    case EmptyType
  }

  object TypeOps {

    import scala.annotation._
    import util.Showable
    import Type._
    import Tree._
    import util.|>

    def (tpe: Type) =!= (other: Type): Boolean =
      tpe   == other        ||
      tpe   == WildcardType ||
      other == WildcardType

    def packageName(t: Type): Name = t match {
      case PackageInfo(_, name) => name
      case _                    => EmptyName
    }

    def untypedToChecked(tpe: Type): Type = tpe match {
      case UntypedExpect(t) => t
      case Untyped          => WildcardType
      case _                => EmptyType
    }

    def toCurriedList(t: Type): List[Type] = {
      @tailrec
      def inner(acc: List[Type], t: Type): List[Type] = t match {
        case FunctionType(arg, body) => inner(arg :: acc, body)
        case _ => t :: acc
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

    def getSubstitutions(arg: Type, app: Type): List[(Name, Type)] = arg match {
      case AppliedType(f, args1) =>
        app match {
          case AppliedType(g, args2) if args1.size == args2.size =>
            getSubstitutions(f,g) ::: args1.zip(args2).flatMap(getSubstitutions)
          case _ =>
            Nil
        }
      case FunctionType(a1, b1) =>
        app match {
          case FunctionType(a2, b2) =>
            getSubstitutions(a1,a2) ::: getSubstitutions(b1,b2)
          case _ =>
            Nil
        }
      case Product(tpes1) =>
        app match {
          case Product(tpes2) if tpes1.size == tpes2.size =>
            tpes1.zip(tpes2).flatMap(getSubstitutions)
          case _ =>
            Nil
        }
      case Variable(name) =>
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
      case g @ Variable(`sub`) =>
        if by == WildcardType then
          g
        else
          by
      case _ =>
        tpe
    }

    implied for Showable[Type] {
      import implied NameOps._

      def (tpe: Type) userString: String = {
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
            .map(_.userString)
            .mkString("package ", ".", "")
        }
        
        inline def applied(t: Type, ts: List[Type]): String = {
          if ts.isEmpty then
            t.userString
          else {
            val args = ts.map {
              case f @ (FunctionType(_,_) | AppliedType(_,_)) =>
                val str = f.userString
                s"($str)"
              case f =>
                f.userString
            }
            val functorStr = t.userString
            val argsStr = args.mkString(" ")
            s"$functorStr $argsStr"
          }
        }

        inline def fromFunction(arg: Type, body: Type): String = arg match {
          case FunctionType(_,_) => s"(${arg.userString}) -> ${body.userString}"
          case _                 => s"${arg.userString} -> ${body.userString}"
        }

        tpe match {
          case PackageInfo(_,_) =>
            packaged(tpe)
          case FunctionType(arg, body) =>
            fromFunction(arg, body)
          case Product(ts) =>
            ts.map(_.userString).mkString("(", ", ", ")")
          case AppliedType(t, ts) =>
            applied(t, ts)
          case TypeRef(t) =>
            t.userString
          case Variable(t) =>
            t.userString
          case WildcardType =>
            "<anytype>"
          case Untyped | UntypedExpect(_) =>
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
      case t @ Select(_,_)        => t.copy()(t.id, tpe)
      case t @ Ident(_)           => t.copy()(t.id, tpe)
      case t @ PackageDef(_,_)    => t.copy()(t.id, tpe)
      case t @ DefDef(_,_,_,_)    => t.copy()(t.id, tpe)
      case t @ DefSig(_,_)        => t.copy()(t.id, tpe)
      case t @ Apply(_,_)         => t.copy()(t.id, tpe)
      case t @ Function(_,_)      => t.copy()(t.id, tpe)
      case t @ Let(_,_,_)         => t.copy()(t.id, tpe)
      case t @ Literal(_)         => t.copy()(t.id, tpe)
      case t @ CaseExpr(_,_)      => t.copy()(t.id, tpe)
      case t @ CaseClause(_,_,_)  => t.copy()(t.id, tpe)
      case t @ Alternative(_)     => t.copy()(t.id, tpe)
      case t @ Parens(_)          => t.copy()(t.id, tpe)
      case t @ Bind(_,_)          => t.copy()(t.id, tpe)
      case t @ Unapply(_,_)       => t.copy()(t.id, tpe)
      case t @ Tagged(_,_)        => t.copy()(t.id, tpe)
      case t @ TreeSeq(_)         => t
      case EmptyTree              => EmptyTree
    }
  }
}