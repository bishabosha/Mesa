package eec
package util

trait Readable[O] {
  def (string: String) readAs: O
}

object Readable {
  def read[O: Readable](str: String): O = str.readAs
}