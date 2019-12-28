package mesa.eec

import quoted._

object TypeTraverser with

  abstract class =>*:[-T,+U] extends (T => U)

  def \[T, U](f: T =>*: U): T =>*: U = f

  case object !

  type ![A] = (A & !.type) // remember to unwrap expressions of this type to prevent classcastexception

  given BangOps: [A](x: A) extended with
    def unary_! : ![A] = x.asInstanceOf[![A]]

  def traverseTypesImpl[T: Type](expr: Expr[T])(given qctx: QuoteContext): Expr[Unit] =
    import qctx.tasty.{Type => _, _,given}

    expr match
      case '{ \($x: $t =>*: $u) } => traverseTypesImpl(x)

      case f @ '{ type $t; type $u; ($x: `$t`) => ($body: `$u`) } =>
        if f.unseal.tpe <:< typeOf[$t =>*: $u]
          '{ println(s"\\(${${Expr(x.name)}}: ${${Expr(t.show)}}) =>* (${${Expr(body.show)}}): ${${Expr(u.show)}}") }
        else
          '{ println(s"(${${Expr(x.name)}}: ${${Expr(t.show)}}) => (${${Expr(body.show)}}): ${${Expr(u.show)}}") }

      case '{ type $a; type $b; val $x = ($t: ![`$a`]); ($body: `$b`) } =>
        '{ println(s"let !${${Expr(x.name)}} be (${${Expr(t.show)}}): ${${Expr(a.show)}} in (${${Expr(body.show)}}): ${${Expr(b.show)}}") }

      case e => '{ println(s"expr ${${Expr(e.show)}} is of type ${${Expr(summon[Type[T]].show)}}") }

  inline def traverseTypes[T](expr: => T): Unit = ${ traverseTypesImpl('expr) }
