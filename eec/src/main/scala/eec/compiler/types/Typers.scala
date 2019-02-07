package eec
package compiler
package types

object Typers {

  import Types._
  import Types.Type._
  import core.Names.Name._
  import core.Constants._
  import core.Constants.Constant._
  import ast.Trees._
  import ast.Trees.Tree._
  import ast.Trees.untyped._
  import error.TyperErrors._
  import error.TyperErrors.TyperError._

  enum Mode {
    case Type, Term, Pat
  }

  def (tree: Tree) typd (implicit mode: Mode): Typed[Tree] = {
    import Mode._
    import TypeOps._

    def constantTpe(const: Constant): Type = const match {
      case _ @ BooleanConstant(_) => TypeRef(BooleanTag)
      case _ @ BigDecConstant(_) => TypeRef(DecimalTag)
      case _ @ BigIntConstant(_) => TypeRef(IntegerTag)
      case _ @ CharConstant(_) => TypeRef(CharTag)
      case _ @ StringConstant(_) => TypeRef(StringTag)
    }

    def functionTerm(f: Function): implicit Mode => Typed[Tree] = {
      def function(args1: List[Tree], body1: Tree) = {
        val tpes = args1.map(_.tpe)
        val fType = tpes.foldRight(body1.tpe) { FunctionType(_,_) }
        Function(fType, args1, body1)
      }
      f match {
        case Function(_, args, body) =>
          for {
            args1 <- args.mapM(_.typd)
            body1 <- body.typd
          } yield function(args1, body1)
      }
    }

    def functionType(f: Function): implicit Mode => Typed[Tree] = {
      def function(args1: List[Tree], body1: Tree) = {
        val tpe =
          if args1.length == 1 then
            args1.head.tpe
          else
            Product(args1.map(_.tpe))
        val fType = FunctionType(tpe, body1.tpe)
        Function(fType, args1, body1)
      }
      f match {
        case Function(_, args, body) =>
          for {
            args1 <- args.mapM(_.typd)
            body1 <- body.typd
          } yield function(args1, body1)
      }
    }

    def tagged(t: Tagged): implicit Mode => Typed[Tree] = t match {
      case Tagged(_, arg, tpeTree) =>
        implicit val mode1 = Type
        tpeTree.typd.map { tpeTree1 =>
          Tagged(tpeTree1.tpe, arg, tpeTree1)
        }
    }

    def parens(t: Parens): implicit Mode => Typed[Tree] = t match {
      case Parens(_, List()) =>
        Parens(Product(List()), List())
      case Parens(_, es) =>
        es.mapM(_.typd).map { es1 =>
          val tpes = es1.map(_.tpe)
          val tupleTyp = Product(tpes)
          Parens(tupleTyp, es1)
        }
    }
 
    tree match {
      case Literal(_,c) => Literal(constantTpe(c), c)
      case Ident(_, name) if mode == Type => Ident(TypeRef(name), name)
      // case Ident(_, name) if mode == Term => Ident(TypeRef(name), name)
      case Apply(_, e, es) if mode == Type =>
        for {
          e1 <- e.typd
          es1 <- es.mapM(_.typd)
        } yield {
          val tpe = AppliedType(e1.tpe, es1.map(_.tpe))
          Apply(tpe, e, es1)
        }
      case t @ Function(_,_,_) if mode == Term => functionTerm(t)
      case t @ Function(_,_,_) if mode == Type => functionType(t)
      case t @ Tagged(_,_,_) => tagged(t)
      case t @ Parens(_,_) => parens(t)
      case EmptyTree => EmptyTree
      case _ => Typing(s"Typing not implemented for <$mode `$tree`>")
    }
  }
}