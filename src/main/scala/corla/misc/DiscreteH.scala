package corla.misc

import corla.{findFirstSt_, random, Probability}
import corla.misc.NESet._

import scalaz._
import std.set._
import scalaz.effect.IO

/** Discrete distribution backed by hashset */
class DiscreteH[A](private val pdf: Map[A,Probability]) {
  import MapUtil.MapHelper

  // marginal distribution
  def marginal[B](f: A => B) = map(f)
  def partition[B](f: A => B) = map(f)
  def map[B](f: A => B): DiscreteH[B] =
    new DiscreteH(pdf.mapKeysWith(f, _ + _))

  def flatMap[B](f: A => DiscreteH[B]): DiscreteH[B] =
    new DiscreteH(
      MapUtil.fromListWith(
        pdf.toList.map { case (k,p) => f(k).pdf.mapValues(p*).toList }.flatten
      )(_ + _)
    )

  // pretty sure this is safe
  def toNES: NESet[(A, Probability)] = NESet(pdf.head, pdf.tail.toSet)


  val total = pdf.values.sum
  def sampleIO(): IO[A] = {

    def checkSt = (p: Probability) => State[Double,Boolean] {
      choice =>
        val diff = choice - p
        (diff, diff < 0)
    }

    //just in case our sum is not exactly 1, scale our random choice so we always pick within our range of values
    random map (_ * total) map {
      // get each entry's probability mass and decrement the random sample until we hit a match
      findFirstSt_(toNES)(checkSt compose (_._2))(_)
        .getOrElse(sys.error(s"sampleIO2 in $this / $total"))
        ._1
    }
  }
}

object DiscreteH {
  import MapUtil.MapHelper

  def uniform_[A](first: A, rest: A*) = uniform[A](NESet.apply(first,rest: _*))
  def uniform[A](values: NESet[A]): DiscreteH[A] = {
    val p = 1.0 / values.size
    new DiscreteH(Map(values.toList.map( _ -> p ): _*))
  }

  def weighted[A](weights: Map[A,Double]): Maybe[DiscreteH[A]] =
    Maybe.fromTryCatchNonFatal(weightedUnsafe(weights))

  def weightedUnsafe[A](weights: Map[A,Double]): DiscreteH[A] = {
    val noZeros = weights.filter(_._2 > 0)
    val weightSum = noZeros.values.sum
    require(weightSum > 0)
    weightedNoChecks(noZeros, weightSum)
  }

  private def weightedNoChecks[A](values: Map[A,Double], weightSum: Double): DiscreteH[A] =
    new DiscreteH( values.mapValues( _ / weightSum ) )

  def reweight[A](p: DiscreteH[A])(f: (A,Probability) => Probability): Maybe[DiscreteH[A]] =
    weighted(p.pdf.mapWithKey(f))


  implicit val discretePDF = new PDF[DiscreteH] {
    def pdf[A](p: DiscreteH[A])(a: A): Probability = p.pdf.getOrElse(a, 0)

    def reweight[A](p: DiscreteH[A])(f: (A, Probability) => Probability): Maybe[DiscreteH[A]] =
      DiscreteH.reweight(p)(f)

    def uniform[A](values: NESet[A]): DiscreteH[A] = DiscreteH.uniform(values)

    def weightedUnsafe[A](weights: Map[A, Double]): DiscreteH[A] = DiscreteH.weightedUnsafe(weights)

    def point[A](a: => A): DiscreteH[A] = new DiscreteH(Map(a -> 1))

    def bind[A, B](fa: DiscreteH[A])(f: (A) => DiscreteH[B]): DiscreteH[B] = fa.flatMap(f)

    // pretty sure this is safe
    def toNES[A](p: DiscreteH[A]): NESet[(A, Probability)] = p.toNES
  }

  implicit val sampleIO: DiscreteH ~> IO = new NaturalTransformation[DiscreteH,IO] {
    def apply[A](fa: DiscreteH[A]): IO[A] = fa.sampleIO()
  }
}
