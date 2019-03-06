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
    case Generic(name: Name)
    case FunctionType(arg: Type, body: Type)
    case Product(args: List[Type])
    case AppliedType(typ: Type, args: List[Type])
    case NoType
    case WildcardType
    case UntypedExpect(typ: Type)
    case Untyped
  }

  object TypeOps {

    import scala.annotation._
    import util.Showable
    import Type._
    import Tree._
    import util.|>
    import implied NameOps._

    def packageName(t: Type): Name = t match {
      case PackageInfo(_, name) => name
      case _                    => EmptyName
    }

    def toCurriedList(t: Type): List[Type] = {
      @tailrec
      def inner(acc: List[Type], t: Type): List[Type] = t match {
        case FunctionType(arg, body) => inner(arg :: acc, body)
        case _ => t :: acc
      }
      inner(Nil, t).reverse
    }

//  lift f : (a -> !b) -> !a -> !b
//  [f], [a -> !b, !a, !b] => !a -> !b

    def toReturnType(defSig: Tree, t: Type): Type =
      defSig match {
        case DefSig(_, args) =>
          toCurriedList(t) match {
            case Nil    => NoType
            case many   => toFunctionType(many.drop(args.length))
          }
        case _ =>
          NoType
      }

    def toFunctionType(ts: List[Type]): Type =
      ts.reverse match {
        case t :: rest  => rest.foldLeft(t)((acc, t1) => FunctionType(t1, acc))
        case Nil        => NoType
      }

    implied for Showable[Type] {

      private def (tree: Tree) named: String = {
        import implied TreeOps._
        import implied NameOps._
        import util.Convert
        val names = Convert[Tree, List[Name]](tree)
        names.map(_.userString).mkString(".")
      }

      @tailrec
      private def packageNamed(acc: List[Name], tpe: Type): List[Name] = tpe match {
        case PackageInfo(parent, name)  => packageNamed(name :: acc, parent)
        case _                          => acc
      }

      def (tpe: Type) userString: String = tpe match {
        case t @ PackageInfo(_,_) =>
          packageNamed(Nil, t).map(_.userString).mkString("package ", ".", "")
        case FunctionType(f: FunctionType, b) =>
          s"(${f.userString}) -> ${b.userString}"
        case FunctionType(t, b) =>
          s"${t.userString} -> ${b.userString}"
        case Product(ts) =>
          ts.map(_.userString).mkString("(", ", ", ")")
        case AppliedType(t, ts) =>
          val args = ts.map {
            case f @ FunctionType(_,_) => s"(${f.userString})"
            case f => f.userString
          }.mkString(" ")
          if args.isEmpty then
            t.userString
          else
            s"${t.userString} $args"
        case TypeRef(t) =>
          import implied NameOps._
          t.userString
        case Generic(t) =>
          import implied NameOps._
          t.userString
        case WildcardType =>
          "<anytype>"
        case Untyped | UntypedExpect(_) =>
          "<untyped>"
        case NoType =>
          "<notype>"
      }
    }

    implied fromList for (List[Type] |> Type) {
      def apply(ts: List[Type]) = ts match {
        case tpe :: Nil => tpe
        case types      => Product(types)
      }
    }

    implied toList for (Type |> List[Type]) {
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
      // case t @ Unapply(_,_,_)       => t.copy()(t.id, tpe)
      case t @ Tagged(_,_)        => t.copy()(t.id, tpe)
      case t @ TreeSeq(_)         => t
      case EmptyTree              => EmptyTree
    }
  }
}