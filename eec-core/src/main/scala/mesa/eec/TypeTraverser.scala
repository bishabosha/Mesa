package mesa.eec

import quoted._

object TypeTraverser with

  abstract class =>*:[-T,+U] extends (T => U)

  def \[T, U](f: T =>*: U): T =>*: U = f

  case class ![A](a: A)

  given BangOps: [A](x: A) extended with
    def repeatable : ![A] = new !(x)

  def traverseTypesImpl[T: Type](expr: Expr[T])(given qctx: QuoteContext): Expr[Unit] =
    import qctx.tasty.{Type => TastyType, _,given}

    expr match
      case '{ \($x: $t =>*: $u) } => traverseTypesImpl(x)

      case f @ '{ type $t; type $u; ($x: `$t`) => ($bodyFn: `$t` => `$u`)(`$x`) } =>
        val arr = if f.unseal.tpe <:< typeOf[$t =>*: $u] then Expr("=>*") else Expr("=>")
        Expr.open(bodyFn)((body, _) =>
          '{ println(s"\\(${${Expr(x.name)}}: ${${Expr(t.show)}}) ${$arr} (${${Expr(body.show)}} : ${${Expr(u.show)}})") }
        )

      case '{ type $a; val $x = ($t: ![`$a`]); ($bodyFn: ![`$a`] => ?)(`$x`) } =>
        Expr.open(bodyFn) { (body, close) =>
          quoted.util.Var(t) { tVar =>
            close(body)(tVar.get) match
            case '{ $body: $b } =>
              '{ println(s"let !${${Expr(x.name)}} be (${${Expr(t.show)}} : ![${${Expr(a.show)}}]) in (${${Expr(body.show)}} : ${${Expr(b.show)}})") }
          }
        }

      case e => '{ println(s"expr ${${Expr(e.unseal.underlyingArgument.show)}} is of type ${${Expr(summon[Type[T]].show)}}") }

  inline def traverseTypes[T](expr: => T): Unit = ${ traverseTypesImpl('expr) }
