package corla.misc

import corla.Probability

import scalaz._, Maybe._

/**
 * Created by arya on 12/6/14.
 */
/** Discrete distribution backed by ordered set */
class DiscreteO[A](private val pdf: A ==>> Probability) {

  // marginal distribution
  def marginal[B:Order](f: A => B) = map(f)
  def partition[B:Order](f: A => B) = map(f)
  def map[B:Order](f: A => B): DiscreteO[B] =
    new DiscreteO(pdf.mapKeysWith(f, _ + _))

  // joint distribution
  def flatMap[B:Order](f: A => DiscreteO[B]): DiscreteO[B] =
    new DiscreteO(
      ==>>.fromListWith(
        pdf.toList.map { case (k,p) => f(k).pdf.map(p*).toList }.flatten
      )(_ + _)
    )

  // conditional distribution
  def filter(f: A => Boolean)(implicit o: Order[A]): Maybe[DiscreteO[A]] =
    DiscreteO.weighted(pdf.filterWithKey { case (a,p) => f(a) })

  def expectedVal(f: A => Double): Double =
    pdf.toList.map({ case (a, p) => p * f(a) }).sum
}

object DiscreteO {
  import NEOSet._

  def point[A](a: A): DiscreteO[A] =
    new DiscreteO(==>> singleton (a, 1))

  def uniformUnsafe[A:Order](values: ISet[A]): DiscreteO[A] = {
    import scalaz.std.anyVal._
    import scalaz.std.tuple._
    val p = 1.0 / values.size
    new DiscreteO(==>> fromFoldable values.map( _ -> p ))
  }

  def uniform[A:Order](values: NEOSet[A]): DiscreteO[A] = {
    import scalaz.std.anyVal._
    import scalaz.std.tuple._
    val p = 1.0 / values.size
    new DiscreteO(==>> fromFoldable values.map( _ -> p ).toISet )
  }

  def weighted[A:Order](weights: A ==>> Double): Maybe[DiscreteO[A]] =
    fromTryCatchNonFatal(weightedUnsafe(weights))

  def weightedUnsafe[A:Order](weights: A ==>> Double): DiscreteO[A] = {
    val noZeros = weights.filter(_ > 0)
    val weightSum = noZeros.values.sum
    require(weightSum > 0)
    weightedNoChecks(noZeros, weightSum)
  }

  private def weightedNoChecks[A](values: A ==>> Double, weightSum: Double): DiscreteO[A] =
    new DiscreteO( values.map( _ / weightSum ) )

  def reweight[A:Order](p: DiscreteO[A])
                       (f: (A,Probability) => Probability): Maybe[DiscreteO[A]] =
    weighted(p.pdf.mapWithKey(f))

  implicit val pdfinstance = new OrderedPDF[DiscreteO] {
    def pdf[A:Order](p: DiscreteO[A])(a: A): Probability =
      p.pdf.lookup(a) getOrElse 0

    def reweight[A:Order](p: DiscreteO[A])(f: (A, Probability) => Probability): Maybe[DiscreteO[A]] =
      DiscreteO.reweight(p)(f)

    def uniform[A:Order](values: NEOSet[A]): DiscreteO[A] =
      DiscreteO.uniform(values)

    def weightedUnsafe[A:Order](weights: A ==>> Double): DiscreteO[A] =
      DiscreteO.weightedUnsafe(weights)

    def point[A](a: A): DiscreteO[A] =
      DiscreteO.point(a)
  }
}
