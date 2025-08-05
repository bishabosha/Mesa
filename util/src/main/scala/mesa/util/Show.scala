package mesa.util

@FunctionalInterface
trait Show[O] {
  extension (o: O) def show: String
}