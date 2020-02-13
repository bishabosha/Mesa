package mesa.util

@FunctionalInterface
trait Show[-O] {
  def (o: O) show: String
}

object Show {

  def show[T: Show](t: T): String = t.show

  given Show[String] = identity

  given [A](given Show[A]): Show[List[A]] =
    _.map(_.show).mkString("[", ",", "]")

  given [A,B](given Show[A], Show[B]): Show[(A,B)] =
    (a,b) => s"(${a.show}, ${b.show})"

  given [A,B,C](given Show[A], Show[B], Show[C]): Show[(A,B,C)] =
    (a,b,c) => s"(${a.show}, ${b.show}, ${c.show})"

  given [A,B,C,D](given Show[A], Show[B], Show[C], Show[D]): Show[(A,B,C,D)] =
    (a,b,c,d) => s"(${a.show}, ${b.show}, ${c.show}, ${d.show})"
}
