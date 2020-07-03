package mesa.util

import scala.collection.SeqView
import scala.collection.IndexedSeq

def [A](ia: IArray[A]) view: SeqView[A] =
  ia.asInstanceOf[Array[A]].view.asInstanceOf

def eval[A,B](a: A, f: A => B) = f(a)

def const[A,B](a: A)(b: B) = a

def [CC[_] <: collection.Iterable[?], A, O](ts: CC[A]) foldMap(empty: => O)(f: CC[A] => O) =
  if ts.isEmpty then empty
  else f(ts)
