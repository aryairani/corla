package corla

import corla.memory.EmptyMemory2
import corla.misc.PDF, PDF.pdfSyntax
import ConstraintDecision._

import scalaz._, Maybe._
import scalaz.syntax.monad._

trait mixfunctions {

  def constrainOption[S,A,M,P[_]:Functor](c: ConstraintSA[S,A], o: GenOption[S,A,M,P])
                                         (implicit M:EmptyMemory2[M,S,A]): GenOption[S,A,M,P] =
    (m,s) => o(m,s).map {
      case Just(a) => if (c(s,a) == Avoid) Empty() else Just(a)
      case Empty() => Empty()
    }

  def biasPolicy[S,A,M,P[_]:PDF](pref: PrefSA[S,A], π: GenPolicy[S,A,M,P]): MaybePolicy[S,A,M,P] =
    (m,s) => π(m,s).reweight { case (a,p) => p * pref(s,a) }

  def biasPolicyA[S,A,M,P[_]:PDF](pref: PrefSA[S,A], π: GenPolicyA[S,A,M,P]): MaybePolicyA[S,A,M,P] =
    (m,s) => as => π(m,s)(as).reweight { case (a,p) => p * pref(s,a) }

  def mixPolicies[S,A,M,P[_]:Bind](policies: P[GenPolicy[S,A,M,P]]): GenPolicy[S,A,M,P] =
    (m,s) => policies.flatMap(π => π(m,s))

  def mixPoliciesA[S,A,M,P[_]:Bind](policies: P[GenPolicyA[S,A,M,P]]): GenPolicyA[S,A,M,P] =
    (m,s) => actions => policies.flatMap(π => π(m,s)(actions))
}
