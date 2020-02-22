package mesa.util

@FunctionalInterface
trait Show[-O] {
  def (o: O) show: String
}

object Show {

  def show[T: Show](t: T): String = t.show

  given Show[String]  = identity
  given Show[Boolean] = _.toString

  given [A](given Show[A]): Show[List[A]] =
    _.map(_.show).mkString("[", ",", "]")

  given [A](given Show[A]): Show[Option[A]] = opt =>
    show(opt.map(_.show).toList)

  given [A,B](given Show[A], Show[B]): Show[(A,B)] =
    (a,b) => s"(${a.show}, ${b.show})"

  given [A,B,C](given Show[A], Show[B], Show[C]): Show[(A,B,C)] =
    (a,b,c) => s"(${a.show}, ${b.show}, ${c.show})"

  given [A,B,C,D](given Show[A], Show[B], Show[C], Show[D]): Show[(A,B,C,D)] =
    (a,b,c,d) => s"(${a.show}, ${b.show}, ${c.show}, ${d.show})"

  given [A,B,C,D,E](given Show[A], Show[B], Show[C], Show[D], Show[E]): Show[(A,B,C,D,E)] =
    (a,b,c,d,e) => s"(${a.show}, ${b.show}, ${c.show}, ${d.show}, ${e.show})"
}
