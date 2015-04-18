package corla.rl.v7

import corla.memory.Memory3
import corla.rl.{Reward, Next}

import scalaz._
import syntax.monad._
import std.vector._

object runSimple {
  /** `_s` is only used to construct Experience;
    * it is not used by the ExternalModel, which
    * tracks its own state. */
  def experienceAction[D[_[_],_,_],S,A,M,F[_]:Functor](d: D[F,S,A], m: M)
                                                      (_s: S)
                                                      (a: A)
                                                      (implicit
                                                       M: Memory3[M,S,A],
                                                       D: AbstractModel3[D,F,S,A]
                                                        ): F[(ExperienceA[S,A],M)] =
    D.act(d, _s, a) map {
      case (r,nextA) =>
        val e: (S, A, Reward, Next[S, A]) = (_s, a, r, nextA)
        val m2 = M.addExperience(e)(m)
        (e,m2)
    }

  /**
   * @param loop given (next s/as, current memory, and current history),
   *             return an effect that acts in the simulator and returns
   *             the NEXT s/as, updated memory, and updated history
   * @param history the current history
   * @tparam F the effect system
   * @return
   */
  def addToHistoryAndRecurse[S,A,M,F[_]:Applicative](loop: ((Next[S,A],M,HistoryA[S,A]) => F[(Next[S,A],M,HistoryA[S,A])]))(history: HistoryA[S,A]): ((ExperienceA[S,A],M)) => F[(Next[S,A],M,HistoryA[S,A])] = {
    case (e @ (_, _, _, next2 @ Next(_, as)), m2) =>
      val result = (next2, m2, history :+ e)
      if (as.isEmpty) result.point[F]
      else loop.tupled(result)
  }

  def runOptionV6[D[_[_],_,_],S,A,M,P[_],F[_]:Monad](next: Next[S,A], o: GenPolicy[S,Maybe[A],M,P], m: M, d: D[F,S,A])(implicit D: AbstractModel3[D,F,S,A], M:Memory3[M,S,A], sample: P ~> F): F[(Next[S,A],M,HistoryA[S,A])] =
  {
    def loop: (Next[S,A], M, HistoryA[S,A]) => F[(Next[S,A],M,HistoryA[S,A])] = {
      case in @ (Next(s, mas), m, history) =>
        sample(o(m, s))
          .map { ma => (mas |@| ma)(checkAction(_)(_)) }
          .flatMap {
             _.cata(experienceAction(d, m)(s)(_)
              .flatMap(addToHistoryAndRecurse(loop)(history)), in.point[F])
          }
    }
    loop(next, m, Vector.empty[ExperienceA[S, A]])
  }

  /*
  def runOption[D[_[_],_,_],S,A,M,P[_],F[_]:Monad](next: Next[S,A], o: AbstractSMOption.Aux[S,A,M,P], m: M, d: D[F,S,A])(implicit D: AbstractModel3[D,F,S,A], M: Memory3[M,S,A], sample: P ~> F): F[(Next[S,A],M,History[S,A])] = runOptionV6(next,o.o,m,d)

  def runTrialPolicyA[D[_[_],_,_],S,A,M,P[_],F[_]:Monad](next: Next[S,A], π: GenPolicyA[S,A,M,P], m: M, d: D[F,S,A])(implicit D: AbstractModel3[D,F,S,A], M:Memory3[M,S,A],sample: P ~> F): F[(Next[S,A],M,History[S,A])] =
  {
    def loop: (Next[S,A], M, History[S,A]) => F[(Next[S,A],M,History[S,A])] = {
      case in @ (Next(s,actions), m, history) =>
        actions.map[F[(Next[S,A],M,History[S,A])]](as =>
          sample(π(m,s)(as))
            .map(checkAction(as))
            .flatMap(experienceAction(d,m)(s)(_))
            .flatMap(addToHistoryAndRecurse(loop)(history))
        ) getOrElse in.point[F]
    }
    loop(next,m,Vector.empty[Experience[S,A]])
  }

  def runTrialPolicy[D[_[_],_,_],S,A,M,P[_],F[_]:Monad](next: Next[S,A], π: GenPolicy[S,A,M,P], m: M, d: D[F,S,A])(implicit D: AbstractModel3[D,F,S,A], M:Memory3[M,S,A], sample: P ~> F): F[(Next[S,A],M,History[S,A])] = runTrialPolicyA(next, new gpsyntax(π).policyA, m, d)

  def runTrialAgentA[D[_[_],_,_],S,A,M,P[_],F[_]:Monad](next: Next[S,A], agent: AgentA[S,A,M,P], d: D[F,S,A])(implicit D: AbstractModel3[D,F,S,A], sample: P ~> F): F[(Next[S,A],AbstractAgentA[S,A,P],History[S,A])] = {
    import agent.M
    runTrialPolicyA(next, agent.π, agent.m, d) map replaceMemoryUsingResultA(agent)
  }

  def replaceMemoryUsingResultA[S,A,M,P[_]](agent: AbstractAgentA.Aux[S,A,M,P]): ((Next[S,A],M,History[S,A])) => (Next[S,A],AbstractAgentA[S,A,P],History[S,A]) =
    { case (next, m, history) => (next, agent.replaceMem(m), history)}
  */
}

object runAbstract {

  /**
   *
   * @param next current state, and legal actions
   * @param π policy over abstract actions
   * @param m agent memory
   * @param d domain model
   * @param AA action abstraction
   * @param D domain
   * @param M memory value
   * @param sample produces distribution-sampling effects
   * @tparam D domain
   * @tparam S state
   * @tparam A primitive action
   * @tparam AA action abstraction
   * @tparam M memory implementation
   * @tparam P probability distribution
   * @tparam F simulator effect system
   * @return an effect that produces the final state, agent memory, and execution trace
   */
  def runTrialPolicyA[D[_[_],_,_],S,A,AA,M,P[_],F[_]:Monad](next: Next[S,A],
                                                            π: GenPolicyAA[S,A,AA,M,P],
                                                            m: M,
                                                            d: D[F,S,A])
                                                           (implicit
                                                            AA: AbstractAction.Aux2[S,A,AA,P],
                                                            D: AbstractModel3[D,F,S,A],
                                                            M: Memory3[M,S,A],
                                                            sample: P ~> F
                                                             ): F[(Next[S,A],M,HistoryA[S,A])] =
  {
    // apply recent experience to agent memory and to trialHistory, then continue loop
    def appendAndRecurse: (M, HistoryA[S,A]) => ((AA.M, HistoryA[S,A], Next[S,A])) => F[(Next[S,A],M,HistoryA[S,A])] = {
      case (m, trialHistory) => {
        case (_, aaHistory, next2) =>
          val m2: M = M.addBatchExperience[Vector].apply(aaHistory)(m)
          loop.apply(next2, m2, trialHistory ++ aaHistory)
      }
    }

    // if not terminal state (empty actions), sample policy, apply AA experience to memory, update history, recurse
    def loop : (Next[S,A], M, HistoryA[S,A]) => F[(Next[S,A],M,HistoryA[S,A])] = {
      case in @ (n @ Next(s, actions), m, history) =>
        actions.map(as =>
          sample(π(m,s)(as))
            .flatMap(aa => AA.experience(d,n,aa)) // returns (AA.M, HistoryA[S,A], Next[S,A])
            .flatMap(appendAndRecurse(m,history))
          ) getOrElse in.point[F]
    }

    loop(next,m,Vector.empty[ExperienceA[S,A]])
  }
}
