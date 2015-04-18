package corla.misc

import corla.Probability

import scalaz._

trait UnsafePDF[P[_]] extends Monad[P] {
	def pdf[A](p: P[A])(a: A): Probability
	def uniform[A](values: Set[A]): P[A]
	def weighted[A](values: Map[A,Probability]): P[A]
	def reweight[A](p: P[A])(f: (A,Probability) => Probability): Maybe[P[A]]
}
object UnsafePDF {
	def apply[P[_]:UnsafePDF] = implicitly[UnsafePDF[P]]
}

