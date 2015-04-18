package corla.rl

import corla.rl.v7._

import scalaz.Maybe

/**
  * Created by arya on 12/20/14.
  */
case class Next[S,A](s: S, as: Maybe[Actions[A]]) {
   def map[T](f: S => T): Next[T,A] = Next(f(s), as)
 }

object Next {
  def just[S,A](s: S, as: Actions[A]) = Next[S,A](s, Maybe.just(as))
}
