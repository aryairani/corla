package corla.misc

import corla.Probability
import corla.misc.NEOSet._
import scalaz._
/**
 * Created by arya on 12/8/14.
 */
object OrderedPDF {
   def apply[P[_]:OrderedPDF] = implicitly[OrderedPDF[P]]
 }

trait OrderedPDF[P[_]] extends Pointed[P] {
   def pdf[A:Order](p: P[A])(a: A): Probability
   def uniform[A:Order](values: NEOSet[A]): P[A]
   def weightedUnsafe[A:Order](weights: A ==>> Double): P[A]
   def reweight[A:Order](p: P[A])(f: (A,Probability) => Probability): Maybe[P[A]]

   def weighted[A:Order](weights: A ==>> Double): Maybe[P[A]] =
     Maybe.fromTryCatchNonFatal(weightedUnsafe(weights))
 }
