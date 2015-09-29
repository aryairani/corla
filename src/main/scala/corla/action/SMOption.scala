package corla.action

import corla._
import corla.memory.Memory3.Next
import corla.memory.{EmptyMemory3, Memory3, PrimitiveMemory}

import scalaz._, Scalaz._

trait AbstractSMOption[S,A,P[_]] {
  type M
  def M: EmptyMemory3[M,S,A]
  def o: GenPolicy[S,Maybe[A],M,P]
  def apply = o

  def contramap[T](f: T => S): AbstractSMOption[T,A,P] =
    SMOption[T,A,M,P]((m,t) => o(m,f(t)))(M.contramap(f))
}
object AbstractSMOption {
  trait Aux[S,A,M0,P[_]] extends AbstractSMOption[S,A,P] {
    type M = M0
  }
}

case class SMOption[S,A,M0,P[_]](o: GenPolicy[S,Maybe[A],M0,P])
                                (implicit M0:EmptyMemory3[M0,S,A]) extends AbstractSMOption.Aux[S,A,M0,P] {
  def M = M0
  def apply(m:M,s:S): P[Maybe[A]] = o(m,s)
}

object SMOption {

  def fromPrimitive[S,A,P[_]:Applicative](a: A): SMOption[S,A,PrimitiveMemory,P] =
    SMOption[S,A,PrimitiveMemory,P]((m,s) => m.isFirstAction.guard[Maybe](a).point[P])

  implicit def abstractAction[S,A,M,P[_]](implicit M:EmptyMemory3[M,S,A]): AbstractAction[S,A,SMOption[S,A,M,P]] =
    new SmoAbstractAction[S,A,M,P]

  protected class SmoAbstractAction[S,A,M,P[_]](implicit M0:EmptyMemory3[M,S,A]) extends AbstractAction.Aux[S,A,SMOption[S,A,M,P],M,P] {
    implicit val M: Memory3[M, S, A] = M0

    def experience[D[_[_],_,_], F[_]:Monad](domain: D[F, S, A], start: Next[S, A], action: SMOption[S, A, M, P])
                                           (implicit D: AbstractModel3[D, F, S, A], sample: P ~> F): F[(M, HistoryA[S, A], Next[S, A])] =
      runSimple.runOptionV6(start, action.o, M0.empty, domain) map { case (next, m, history) => (m, history, next) }

  }
}


