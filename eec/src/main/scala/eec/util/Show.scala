package eec.util

@FunctionalInterface
trait Show[O] {
  def (o: O) show: String
}