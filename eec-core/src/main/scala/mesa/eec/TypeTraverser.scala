package mesa.eec

import quoted._
import java.util.concurrent.atomic.AtomicInteger

object TypeTraverser:

  abstract class =>*:[-T,+U] extends (T => U)

  def \[T, U](f: T =>*: U): T =>*: U = f

  case class ![A](a: A)

  given BangOps: AnyRef with
    extension [A](x: A) def repeatable : ![A] = new !(x)

  private val varnames = AtomicInteger(0)
  def freshVarName(prefix: String = "x"): String = s"${prefix}${varnames.getAndIncrement()}"

  def traverseTypesImpl[T: Type](expr: Expr[T])(using qctx: Quotes): Expr[Unit] =
    import qctx.reflect.{TypeRepr => TastyType, _,given}

    expr match
      case '{ \($x: t =>*: u) } => traverseTypesImpl(x)

      case f @ '{ type t; type u; ((y: `t`) => $bodyFn(y): `u`).apply($x) } =>
        val arr = if f.asTerm.tpe <:< TastyType.of[t =>*: u] then Expr("=>*") else Expr("=>")
        '{ println(s"\\(${${Expr(x.asTerm.symbol.name)}}: ${${Expr(Type.show[t])}}) ${$arr} (${${Expr(bodyFn.show)}} : ${${Expr(Type.show[u])}})") }

      case '{ type a; val x = ($t: ![`a`]); ($bodyFn: ![`a`] => ?)(`x`) } =>
        Expr.betaReduce('{ $bodyFn($t) }) match
          case '{ $body: b } =>
            '{ println(s"let !${${Expr(freshVarName())}} be (${${Expr(t.show)}} : ![${${Expr(Type.show[a])}}]) in (${${Expr(body.show)}} : ${${Expr(Type.show[b])}})") }

      case e => '{ println(s"expr ${${Expr(e.show)}} is of type ${${Expr(Type.show[T])}}") }

  inline def traverseTypes[T](expr: => T): Unit = ${ traverseTypesImpl('expr) }
