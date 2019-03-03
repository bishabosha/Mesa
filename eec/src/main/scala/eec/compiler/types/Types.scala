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
    case FunctionType(arg: Type, body: Type)
    case Product(args: List[Type])
    case AppliedType(typ: Type, args: List[Type])
    case NoType
    case WildcardType
    case Untyped
  }

  val rootPkg = PackageInfo(NoType, From(rootString))

  object TypeOps {

    import scala.annotation._
    import util.Showable
    import Type._
    import util.|>
    import implied NameOps._

    def packageName(t: Type): Name = t match {
      case PackageInfo(_, name) => name
      case _                    => EmptyName
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
        case WildcardType =>
          "<anytype>"
        case Untyped =>
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

    // def (tree: Tree) withType(tpe: Type): Tree = tree match {
    //   case t @ Select(_,_,_)        => t.copy(tpe = tpe)
    //   case t @ Ident(_,_)           => t.copy(tpe = tpe)
    //   case t @ PackageDef(_,_,_)    => t.copy(tpe = tpe)
    //   case t @ DefDef(_,_,_,_,_)    => t.copy(tpe = tpe)
    //   case t @ DefSig(_,_,_)        => t.copy(tpe = tpe)
    //   case t @ Apply(_,_,_)         => t.copy(tpe = tpe)
    //   case t @ Function(_,_,_)      => t.copy(tpe = tpe)
    //   case t @ Let(_,_,_,_)         => t.copy(tpe = tpe)
    //   case t @ Literal(_,_)         => t.copy(tpe = tpe)
    //   case t @ CaseExpr(_,_,_)      => t.copy(tpe = tpe)
    //   case t @ CaseClause(_,_,_,_)  => t.copy(tpe = tpe)
    //   case t @ Alternative(_,_)     => t.copy(tpe = tpe)
    //   case t @ Parens(_,_)          => t.copy(tpe = tpe)
    //   case t @ Bind(_,_,_)          => t.copy(tpe = tpe)
    //   case t @ Unapply(_,_,_)       => t.copy(tpe = tpe)
    //   case t @ Tagged(_,_,_)        => t.copy(tpe = tpe)
    //   case t @ TreeSeq(_)           => t
    //   case EmptyTree                => EmptyTree
    // }

    // def (tree: Tree) tpe: Type = tree match {
    //   case Select(t,_,_)        => t
    //   case Ident(t,_)           => t
    //   case PackageDef(t,_,_)    => t
    //   case DefDef(t,_,_,_,_)    => t
    //   case DefSig(t,_,_)        => t
    //   case Apply(t,_,_)         => t
    //   case Function(t,_,_)      => t
    //   case Let(t,_,_,_)         => t
    //   case Literal(t,_)         => t
    //   case CaseExpr(t,_,_)      => t
    //   case CaseClause(t,_,_,_)  => t
    //   case Alternative(t,_)     => t
    //   case Parens(t,_)          => t
    //   case Bind(t,_,_)          => t
    //   case Unapply(t,_,_)       => t
    //   case Tagged(t,_,_)        => t
    //   case TreeSeq(_)           => Type.NoType
    //   case EmptyTree            => Type.NoType
    // }
  }
}