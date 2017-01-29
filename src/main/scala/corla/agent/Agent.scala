package corla
package agent

import corla.memory.Memory3

sealed trait Agent[S,A,P[_]] { self =>
  type M
  implicit def M: Memory3[M,S,A]
  def m: M
  def π: GenPolicy[S,A,M,P]
  def πm: S => P[A] = s => π(m,s)

  def replaceMem(newmem: M): Agent[S,A,P] = Agent(self.π, newmem)
  def contramap[T](f: T => S) = Agent[T,A,M,P]((m,t) => π(m,f(t)), m)(M.contramap(f))
}

object Agent {
  def apply[S,A,_M,P[_]](_π: GenPolicy[S,A,_M,P], _m: _M)(implicit _M:Memory3[_M,S,A]): Aux[S,A,_M,P] =
    new Aux[S,A,_M,P] { def M = _M; def m = _m; def π = _π }

  trait Aux[S,A,M0,P[_]] extends Agent[S,A,P] { type M = M0 }
}

