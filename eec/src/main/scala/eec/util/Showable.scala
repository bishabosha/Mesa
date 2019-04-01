package eec
package util

trait Showable[O] {
  def (o: O) show: String
}