package eec
package util

object Builders {
  
  import collection.generic.CanBuild
  import collection.mutable.Builder

  implied for CanBuild[Unit, Unit] {
    def apply(from: Nothing): Builder[Unit, Unit] = apply()
    def apply(): Builder[Unit, Unit] = new {
      def += (unit: Unit) = this
      def clear(): Unit = {}
      def result(): Unit = ()
    }
  }
}