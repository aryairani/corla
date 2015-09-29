package corla.misc

import corla.Probability
import corla.misc.NESet._

import scalaz._

trait PDF[P[_]] extends Monad[P] {
	def pdf[A](p: P[A])(a: A): Probability
	def uniform[A](values: NESet[A]): P[A]
	def weighted[A](weights: Map[A,Double]): Maybe[P[A]] = {
		Maybe.fromTryCatchNonFatal(weightedUnsafe(weights))
	}
	def reweight[A](p: P[A])(f: (A,Probability) => Probability): Maybe[P[A]]

	def weightedUnsafe[A](weights: Map[A,Double]): P[A]

	def toNES[A](p: P[A]): NESet[(A,Probability)]
	def toNEL[A](p: P[A]): NonEmptyList[(A,Probability)] = toNES(p).toNEL
	def toSeq[A](p: P[A]): Seq[(A,Probability)] = toNES(p).toList.toSeq
}

object PDF {
	def apply[P[_]:PDF] = implicitly[PDF[P]]
	implicit class pdfSyntax[P[_],A](p:P[A])(implicit P:PDF[P]) {
		def apply(a: A) = P.pdf(p)(a)
		def reweight(f: (A,Probability) => Probability) = P.reweight(p)(f)
	}
}
