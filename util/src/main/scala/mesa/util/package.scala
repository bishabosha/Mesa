package mesa.util

import scala.collection.SeqView
import scala.collection.IndexedSeq

extension [A](ia: IArray[A]) def view: SeqView[A] =
  ia.asInstanceOf[Array[A]].view.asInstanceOf

def eval[A,B](a: A, f: A => B) = f(a)

def const[A,B](a: A)(b: B) = a

extension (a: Any) inline def checkAtRuntime: a.type @unchecked = a: @unchecked

extension [CC[X] <: collection.Iterable[X], A, O](ts: CC[A]) def foldMap(empty: => O)(f: CC[A] => O) =
  if ts.isEmpty then empty
  else f(ts)
