package mesa.util

/**For converting to a string that appears as part of a printed definition
 */
@FunctionalInterface
trait Define[O] {
  extension (o: O) def define: String
}