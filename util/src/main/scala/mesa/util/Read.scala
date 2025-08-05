package mesa.util

@FunctionalInterface
trait Read[O] {
  extension (string: String) def readAs: O
}