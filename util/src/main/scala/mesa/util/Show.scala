package mesa.util

@FunctionalInterface
trait Show[O] {
  def (o: O) show: String
}