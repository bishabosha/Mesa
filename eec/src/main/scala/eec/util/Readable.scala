package eec
package util

trait Readable[O] {
  def (string: String) readAs: O
}