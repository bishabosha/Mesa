package eec
package compiler
package ast

enum Arithmetic[T] {
  case Number(n: Double)                extends Arithmetic[Double]
  case Expr(o: Arithmetic[Op[Double]],
            l: Arithmetic[Double],
            r: Arithmetic[Double])      extends Arithmetic[Double]
  case Op(o: String)                    extends Arithmetic[Op[Double]]
}