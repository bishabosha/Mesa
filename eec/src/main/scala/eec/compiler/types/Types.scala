package eec
package compiler
package types

object Types {

  import core.Names._
  import ast.Trees._

  object Bootstraps {

    import Type._
    import Name._

    val BooleanType = TypeRef(BooleanTag)
    val DecimalType = TypeRef(DecimalTag)
    val IntegerType = TypeRef(IntegerTag)
    val CharType    = TypeRef(CharTag)
    val StringType  = TypeRef(StringTag)
  }

  enum Type {
    case TypeRef(name: Name)
    case FunctionType(arg: Type, body: Type)
    case Product(args: List[Type])
    case AppliedType(typ: Type, args: List[Type])
    case NoType
    case WildcardType
    case Untyped
  }

  object Type {

    import Type._
    import eec.util.Showable

    implicit val TypeShowable: Showable[Type] = new {
      import Tree._

      private def (tree: Tree) named: String = {
        import TreeOps._
        import NameOps._
        tree.toNames.map(_.userString).mkString(".")
      }

      override def (typ: Type) userString: String = typ match {
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
          import NameOps._
          t.userString
        // case Derived(tree) =>
        //   s"<derived from: `${tree.named}`>"
        case WildcardType =>
          "<anytype>"
        case Untyped =>
          "<untyped>"
        case NoType =>
          "<notype>"
      }
    }
  }

  object TypeOps {

    import ast.Trees._
    import ast.Trees.Tree._

    def (types: List[Type]) toType: Type = types match {
      case tpe :: Nil => tpe
      case _          => Type.Product(types)
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