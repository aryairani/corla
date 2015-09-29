package corla.misc

import scalaz._
import scalaz.syntax.traverse._
import std.list._ // Applicative[List]

object MapUtil {
  implicit class MapHelper[A,B](m: Map[A,B]) {
    def mapKeysWith[C](f: A => C, f2: (B, B) => B): Map[C,B] =
      fromListWith[C, B](m.toList.map(x => (f(x._1), x._2)))(f2)

    def mapWithKey[C](f: (A, B) => C): Map[A,C] =
      m.map { case (a,b) => a -> f(a,b) }
  }

  def fromListWith[A, B](l: List[(A, B)])(f: (B, B) => B): Map[A,B] =
    l.map { case (k,v) => State.modify(
      (m: Map[A,B]) =>
        m.updated(k, m.get(k).map(o => f(v,o)).getOrElse(v))
    )}.sequenceU.exec(Map())


}
