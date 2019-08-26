package mesa.util

import scala.annotation.tailrec
import scala.collection.SeqView
import scala.collection.IndexedSeq

def (ia: IArray[A]) view[A]: SeqView[A, IArray[A]] =
  ia.asInstanceOf[Array[A]].view.asInstanceOf

def eval[A,B](a: A, f: A => B) = f(a)

def const[A,B](a: A)(b: B) = a

def (ts: CC[A]) foldMap[CC[_] <: Seq[_], A, O](empty: => O)(f: CC[A] => O) =
  if ts.isEmpty then empty
  else f(ts)
