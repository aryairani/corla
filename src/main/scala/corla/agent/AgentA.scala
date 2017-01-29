package corla
package agent

import corla.memory.Memory3

sealed trait AgentA[S,A,P[_]] { self =>
  type M
  implicit def M: Memory3[M,S,A]
  def m: M
  def π: GenPolicyA[S,A,M,P]
  def πm: S => Actions[A] => P[A] = s => actions => π(m,s)(actions)

  def contramap[T](f: T => S) = AgentA[T,A,M,P]((m,t) => actions => π(m,f(t))(actions), m)(M.contramap(f))
  def replaceMem(newmem: M) = new AgentA[S,A,P] { type M = self.M; def M = self.M; def m = newmem; def π = self.π }
}

object AgentA {
  def apply[S,A,_M,P[_]](_π: GenPolicyA[S,A,_M,P], _m: _M)(implicit _M:Memory3[_M,S,A]) =
    new Aux[S,A,_M,P] { def M = _M; def m = _m; def π = _π }

  trait Aux[S,A,_M,P[_]] extends AgentA[S,A,P] { type M = _M }
}
