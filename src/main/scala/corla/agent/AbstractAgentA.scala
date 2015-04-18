package corla
package agent

import corla.memory.Memory3

sealed trait AbstractAgentA[S,A,P[_]] { self =>
  type M
  implicit def M: Memory3[M,S,A]
  def m: M
  def π: GenPolicyA[S,A,M,P]
  def πm: S => Actions[A] => P[A] = s => actions => π(m,s)(actions)

  def contramap[T](f: T => S) = AgentA[T,A,M,P]((m,t) => actions => π(m,f(t))(actions), m)(M.contramap(f))
  def replaceMem(newmem: M) = new AbstractAgentA[S,A,P] { type M = self.M; def M = self.M; def m = newmem; def π = self.π }
}

object AbstractAgentA {
  trait Aux[S,A,M0,P[_]] extends AbstractAgentA[S,A,P] {
    type M = M0
  }
}

case class AgentA[S,A,M0,P[_]](π: GenPolicyA[S,A,M0,P], m: M0)(implicit M0:Memory3[M0,S,A])
  extends AbstractAgentA.Aux[S,A,M0,P] {
  implicit def M = M0
}
