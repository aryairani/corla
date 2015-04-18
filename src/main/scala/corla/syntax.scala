package corla

import corla.misc.PDF

import scalaz.Maybe._
import scalaz._

trait syntax {

  implicit class gpsyntax[S,A,M,P[_]](π: GenPolicy[S,A,M,P]) {
    def policyA: GenPolicyA[S,A,M,P] = (m,s) => _ => π(m,s)
    def mpolicy: MaybePolicy[S,A,M,P] = (m,s) => just(π(m,s))
    def mpolicyA: MaybePolicyA[S,A,M,P] = (m,s) => _ => just(π(m,s))
  }
  implicit class mpsyntax[S,A,M,P[_]:PDF](f: MaybePolicy[S,A,M,P]) {
    def orElse(g: MaybePolicy[S,A,M,P]): MaybePolicy[S,A,M,P] =
      (m,s) => f(m,s) orElse g(m,s)

    def getOrElse(g: GenPolicy[S,A,M,P]): GenPolicy[S,A,M,P] =
      (m,s) => f(m,s) getOrElse g(m,s)

    //    def getOrTerminate(implicit M: EmptyMemory2[M,S,A]): GenOption[S,A,M,P] =
    //      GenOption[S,A,M,P]((m,s) => f(m,s) map (_.map(just)) getOrElse empty[A].point[P])
  }

  implicit class experienceSyntax[S,A](e: ExperienceA[S,A]) {
    def map[T](f: (S) => T): ExperienceA[T,A] = e match { case (s,a,r,next) => (f(s),a,r,next.map(f)) }
  }

}
