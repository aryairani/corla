package corla.rl.v7
package agent

import corla.memory.Memory3

sealed trait AbstractAgent[S,A,P[_]] { self =>
  type M
  implicit def M: Memory3[M,S,A]
  def m: M
  def π: GenPolicy[S,A,M,P]
  def πm: S => P[A] = s => π(m,s)

  def replaceMem(newmem: M): AbstractAgent[S,A,P] = Agent(self.π, newmem)
  def contramap[T](f: T => S) = Agent[T,A,M,P]((m,t) => π(m,f(t)), m)(M.contramap(f))
}

object AbstractAgent {
  trait Aux[S,A,M0,P[_]] extends AbstractAgent[S,A,P] {
    type M = M0
  }
}

case class Agent[S,A,M0,P[_]](π: GenPolicy[S,A,M0,P], m: M0)(implicit M0:Memory3[M0,S,A])
  extends AbstractAgent.Aux[S,A,M0,P] {
  implicit def M = M0
}
