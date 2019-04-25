package eec.util

@FunctionalInterface
trait Read[O] {
  def (string: String) readAs: O
}