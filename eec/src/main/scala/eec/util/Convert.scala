package eec
package util

trait |>[A,B] extends (A => B)

object Convert {
  def (a: A) convert[A, B] given (c: A |> B) = c(a)
}