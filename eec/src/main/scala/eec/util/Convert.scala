package eec
package util

trait |>[A,B] extends (A => B)

object Convert {
  inline def apply[A, B] given (c: A |> B) = c
}