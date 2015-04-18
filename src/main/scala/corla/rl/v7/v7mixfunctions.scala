package corla.rl.v7

import corla.misc.PDF
import PDF.pdfSyntax
import corla.rl.PrefSA

import scalaz._, syntax.monad._

trait v7mixfunctions {

  //  def constrainOption[S,A,M,P[_]:Functor](c: ConstraintSA[S,A], o: GenOption[S,A,M,P])
  //                                         (implicit M:EmptyMemory2[M,S,A]): GenOption[S,A,M,P] =
  //    GenOption[S,A,M,P]((m,s) => o(m,s).map {
  //      case Just(a) => if (c(s,a) == Avoid) Empty() else Just(a)
  //      case Empty() => Empty()
  //    })

  def biasPolicy[S,A,M,P[_]:PDF](pref: PrefSA[S,A], π: GenPolicy[S,A,M,P]): MaybePolicy[S,A,M,P] =
    (m,s) => π(m,s).reweight { case (a,p) => p * pref(s,a) }

  def biasPolicyA[S,A,M,P[_]:PDF](pref: PrefSA[S,A], π: GenPolicyA[S,A,M,P]): MaybePolicyA[S,A,M,P] =
    (m,s) => as => π(m,s)(as).reweight { case (a,p) => p * pref(s,a) }

  def mixPolicies[S,A,M,P[_]:Bind](policies: P[GenPolicy[S,A,M,P]]): GenPolicy[S,A,M,P] =
    (m,s) => policies.flatMap(π => π(m,s))

  def mixPoliciesA[S,A,M,P[_]:Bind](policies: P[GenPolicyA[S,A,M,P]]): GenPolicyA[S,A,M,P] =
    (m,s) => actions => policies.flatMap(π => π(m,s)(actions))


}
