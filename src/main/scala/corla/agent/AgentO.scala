package corla
package agent

import corla.memory.EmptyMemory3

import scalaz._

/** What is this actually for?
  *
  * Possible answer: Options that learn long-term?  Like agents?
  * But then why EmptyMemory?
  * May not need this class.... we'll see.
  */
sealed trait AgentO[S,A,P[_]] {
  type M
  def M: EmptyMemory3[M,S,A]
  def m: M
  def o: GenPolicy[S,Maybe[A],M,P]
  def om: S => P[Maybe[A]] = s => o(m,s)

  def contramap[T](f: T => S) = AgentO[T,A,M,P]((m,t) => o(m,f(t)), m)(M.contramap(f))
}

object AgentO {
  def apply[S,A,_M,P[_]](_o: GenPolicy[S,Maybe[A],_M,P], _m: _M)(implicit _M: EmptyMemory3[_M,S,A]): Aux[S,A,_M,P] =
    new Aux[S,A,_M,P] { def M = _M; def m = _m; def o = _o }

  trait Aux[S,A,_M,P[_]] extends AgentO[S,A,P] { type M = _M }
}
