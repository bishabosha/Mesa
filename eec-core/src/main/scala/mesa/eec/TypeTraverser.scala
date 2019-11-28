package mesa.eec

import quoted._
// import quoted.matching._

object TypeTraverser with

  abstract class =>*:[-T,+U] extends (T => U)

  def \[T, U](f: T =>*: U): T =>*: U = f

  def traverseTypesImpl[T: Type](expr: Expr[T])(given qctx: QuoteContext): Expr[Unit] =
    import qctx.tasty.{Type => _, _,given}

    expr match
      case '{ \($x: $t =>*: $u) } => traverseTypesImpl(x)

      case f @ '{ type $t; type $u; ($x: `$t`) => ($body: `$u`) } =>
        if f.unseal.tpe <:< typeOf[$t =>*: $u]
          '{ println(s"linear lambda (${${Expr(x.name)}}: ${${Expr(t.show)}}) =>. (${${Expr(body.show)}}): ${${Expr(u.show)}}") }
        else
          '{ println(s"lambda (${${Expr(x.name)}}: ${${Expr(t.show)}}) => (${${Expr(body.show)}}): ${${Expr(u.show)}}") }

      case e => '{ println(s"expr ${${Expr(e.show)}} is of type ${${Expr(summon[Type[T]].show)}}") }

  inline def traverseTypes[T](expr: => T): Unit = ${ traverseTypesImpl('expr) }
