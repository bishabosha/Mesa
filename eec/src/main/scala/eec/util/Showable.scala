package eec
package util

trait Showable[O] {
  def (o: O) userString: String
}