package corla.rl.v7
package agent

import corla.memory.EmptyMemory3

import scalaz._

/** What is this actually for?
  *
  * Possible answer: Options that learn long-term?  Like agents?
  * But then why EmptyMemory?
  * May not need this class.... we'll see.
  */
sealed trait AbstractAgentO[S,A,P[_]] {
  type M
  def M: EmptyMemory3[M,S,A]
  def m: M
  def o: GenPolicy[S,Maybe[A],M,P]
  def om: S => P[Maybe[A]] = s => o(m,s)

  def contramap[T](f: T => S) = AgentO[T,A,M,P]((m,t) => o(m,f(t)), m)(M.contramap(f))
}

object AbstractAgentO {
  trait Aux[S,A,M0,P[_]] extends AbstractAgentO[S,A,P] {
    type M = M0
  }
}

case class AgentO[S,A,M0,P[_]](o: GenPolicy[S,Maybe[A],M0,P], m: M0)(implicit M0: EmptyMemory3[M0,S,A])
  extends AbstractAgentO.Aux[S,A,M0,P] { def M = M0 }
