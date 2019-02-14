package eec
package util

object Safety {

  opaque type Maybe[O] = O | Null

  object Maybe {

    def some[O](o: O): Maybe[O] = o
    def nothing: Maybe[Nothing] = null

    private[this] def unwrap[O](m: Maybe[O]): O = m.asInstanceOf[O]

    def (m: Maybe[O]) getOrElse[O](n: => O): O = {
      val x = unwrap(m)
      if x == null then
        n
      else
        x
    }
  }
  
}