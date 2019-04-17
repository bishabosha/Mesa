package eec.util

import scala.annotation.tailrec
import scala.collection.SeqView

object Utils {
  def (ia: IArray[A]) view[A]: SeqView[A, Seq[_]] =
    (0 until ia.length).view.map(ia(_))

  def eval[A,B](a: A, f: A => B) = f(a)

  def (ts: List[A]) foldMap[A, O](empty: => O)(f: List[A] => O) =
    if ts.isEmpty then empty
    else f(ts)
}
