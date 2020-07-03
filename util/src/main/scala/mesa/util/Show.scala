package mesa.util

@FunctionalInterface
trait Show[-O] {
  def (o: O) show: String
}

object Show {

  def show[T: Show](t: T): String = t.show

  given as Show[String]  = identity
  given as Show[Boolean] = _.toString

  given [A](using Show[A]) as Show[List[A]] =
    _.map(_.show).mkString("[", ",", "]")

  given as Show[Unit] = _ => "()"

  given [A](using Show[A]) as Show[Option[A]] = opt =>
    show(opt.map(_.show).toList)

  given [A,B](using Show[A], Show[B]) as Show[(A,B)] =
    (a,b) => s"(${a.show}, ${b.show})"

  given [A,B,C](using Show[A], Show[B], Show[C]) as Show[(A,B,C)] =
    (a,b,c) => s"(${a.show}, ${b.show}, ${c.show})"

  given [A,B,C,D](using Show[A], Show[B], Show[C], Show[D]) as Show[(A,B,C,D)] =
    (a,b,c,d) => s"(${a.show}, ${b.show}, ${c.show}, ${d.show})"

  given [A,B,C,D,E](using Show[A], Show[B], Show[C], Show[D], Show[E]) as Show[(A,B,C,D,E)] =
    (a,b,c,d,e) => s"(${a.show}, ${b.show}, ${c.show}, ${d.show}, ${e.show})"
}
