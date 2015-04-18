package corla

import corla.misc.PDF
import corla.misc.NESet.argmaxesBy1
import corla.qlearning.QLookup

import scalaz._, Scalaz._
/**
 * Created by arya on 12/20/14.
 */
package object policy {
  // a deterministic policy which takes a greedy action
  def greedyDeterministicPolicy[S,A,M,P[_]](implicit q: QLookup[M,S,A], P:Applicative[P]): GenPolicyA[S,A,M,P] =
    (m,s) => actions => P.point(actions.maximumBy1(q.qlookup(m)(s)))

  // a stochastic policy which chooses a greedy action uniformly
  def greedyUniformPolicy[S,A,M,P[_]](implicit q: QLookup[M,S,A], P:PDF[P]): GenPolicyA[S,A,M,P] =
    (m,s) => actions => P.uniform(argmaxesBy1(actions)(q.qlookup(m)(s)))

  def uniformPolicy[S,A,M,P[_]](implicit P:PDF[P]): GenPolicyA[S,A,M,P] =
    (m,s) => actions => P.uniform(actions)

  def epsilonGreedy[S,A,M,P[_]](ε: Probability)(implicit q: QLookup[M,S,A], P:PDF[P]): GenPolicyA[S,A,M,P] = {
    assert(ε >= 0 && ε <= 1, s"0 <= ε=$ε <= 1")
    mixPolicies(P.weightedUnsafe(Map(uniformPolicy -> ε, greedyUniformPolicy -> (1 - ε))))
    // weightedUnsafe is ok because I know that ε + (1-ε) > 0
  }

  def mixPolicies[S,A,M,P[_]](policies: P[GenPolicyA[S,A,M,P]])(implicit P:Bind[P]): GenPolicyA[S,A,M,P] =
    (m,s) => actions => P.bind(policies)(π => π(m,s)(actions))

  def liftPolicy[S,Sf,A,M,P[_]](π: GenPolicyA[Sf,A,M,P])(f: S => Sf): GenPolicyA[S,A,M,P] =
    (m,s) => π(m,f(s))
}
