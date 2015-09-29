package corla

import corla.memory.Memory3.Next
import corla.misc.NESet._

import scalaz.Maybe

trait types {
  type Actions[A] = NESet[A]

  type GenPolicy[S,A,M,P[_]] = (M,S) => P[A]
  type GenPolicyA[S,A,M,P[_]] = (M,S) => Actions[A] => P[A]
  type GenPolicyAA[S,A,AA,M,P[_]] = (M,S) => Actions[A] => P[AA]

  type MaybePolicy[S,A,M,P[_]] = (M,S) => Maybe[P[A]]
  type MaybePolicyA[S,A,M,P[_]] = (M,S) => Actions[A] => Maybe[P[A]]
  type GenOption[S,A,M,P[_]] = (M,S) => P[Maybe[A]]

  type Experience[S,A] = (S,A,Reward,S)
  type ExperienceA[S,A] = (S,A,Reward,Next[S,A])

  type History[S,A] = Vector[Experience[S,A]]
  type HistoryA[S,A] = Vector[ExperienceA[S,A]]

  type PrefS[S] = S => Probability
  type PrefSA[S,A] = (S,A) => Probability

  type Probability = Double
  type Reward = Double
  type Utility = Reward

}
