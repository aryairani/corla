package corla.action

import corla._
import corla.memory.Memory3

import scalaz._
import scalaz.syntax.monad._

/* In order to unify primitives with options as AbstractActions,
 * an AbstractAction must provide:
 *
 * - the ability to execute the action on the model
 * - the ability to apply the gained experience into the abstract action's memory
 *
 * Actions look like: A, but options look like (M,S) => P[A], so I guess
 * the AbstractAction must include M,S,P,A.
 *
 * Or maybe we should hold off on this, and simply lift primitives to options,
 * until we have more than just primitives and options to consider.
 */

/**
 * @tparam S state
 * @tparam A low-level action
 * @tparam AA action abstraction
 */
trait AbstractAction[S,A,AA] {
  type P[_]
  type M
  implicit val M: Memory3[M,S,A]
  def experience[D[_[_],_,_],F[_]:Monad](domain: D[F,S,A], start: Next[S,A], action: AA)
                                        (implicit D: AbstractModel3[D,F,S,A], sample: P ~> F): F[(M,HistoryA[S,A],Next[S,A])]
}

object AbstractAction {
  trait Aux[S,A,AA,M0,P[_]] extends Aux2[S,A,AA,P] {
    type M = M0
  }

  trait Aux2[S,A,AA,P0[_]] extends AbstractAction[S,A,AA] {
    type P[x] = P0[x]
  }

  implicit def primitive[S,A,P[_]] = new PrimitiveActionAbstraction[S,A,P]
  protected class PrimitiveActionAbstraction[S,A,P0[_]] extends AbstractAction.Aux[S,A,A,Unit,P0] {
    implicit val M: Memory3[M, S, A] = Memory3.markov

    def experience[D[_[_], _, _], F[_] : Monad](domain: D[F, S, A], start: Next[S, A], action: A)
                                                        (implicit D: AbstractModel3[D,F,S,A], sample: P ~> F): F[(M, HistoryA[S, A], Next[S, A])] =
      runSimple.experienceAction(domain, ())(start.s)(action) map {
        case (exp@(_,_,_,next),()) => ((), Vector(exp), next)
      }
  }
}
