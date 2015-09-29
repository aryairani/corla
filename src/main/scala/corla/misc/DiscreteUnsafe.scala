package corla.misc

import corla.{probabilityAdditiveMonoid, Probability}

import scalaz._, Maybe._
import syntax.traverse._
import std.map._
import std.iterable._

class DiscreteUnsafe[A](private val pdfMap: Map[A,Probability]) {

  def map[B](f: A => B): DiscreteUnsafe[B] = new DiscreteUnsafe(
    pdfMap.groupBy(f compose (_._1)).map({
      case (k,m) => k -> m.map({ case (_,d) => d }).sum
    })
  )

  def flatMap[B](f: A => DiscreteUnsafe[B]): DiscreteUnsafe[B] = {
    val subDistributions: Iterable[Map[B, Probability]] =
      pdfMap.map({ case (k, p) => f(k).pdfMap.mapValues(p * _)})

    new DiscreteUnsafe(subDistributions.suml)
  }
}

object DiscreteUnsafe {

  def uniform[A](values: Set[A]): DiscreteUnsafe[A] = {
    val p = 1.0 / values.size
    new DiscreteUnsafe(values.map( _ -> p ).toMap)
  }

  def weighted[A](values: Map[A, Double]): Maybe[DiscreteUnsafe[A]] = {
    val noZeros = values.filter { case (_,p) => p > 0 }
    val weightSum = noZeros.values.sum

    if (weightSum <= 0) empty
    else just(weightedNoChecks(noZeros, weightSum))
  }

  def weightedUnsafe[A](values: Map[A, Double]): DiscreteUnsafe[A] = {
    val noZeros = values.filter { case (_,p) => p > 0 }
    val weightSum = noZeros.values.sum
    require(weightSum > 0)
    weightedNoChecks(noZeros, weightSum)
  }

  private def weightedNoChecks[A](values: Map[A, Double], weightSum: Double): DiscreteUnsafe[A] =
    new DiscreteUnsafe( values.mapValues( _ / weightSum ) )


  implicit val discreteInstance = new UnsafePDF[DiscreteUnsafe] {
    def pdf[A](p: DiscreteUnsafe[A])(a: A): Probability = p.pdfMap.getOrElse(a,0)

    def uniform[A](values: Set[A]) = DiscreteUnsafe.uniform(values)

    def weighted[A](values: Map[A, Probability]) = DiscreteUnsafe.weightedUnsafe(values)

    def reweight[A](p: DiscreteUnsafe[A])(f: (A, Probability) => Probability) =
      DiscreteUnsafe.weighted(p.pdfMap.map { case (a, p) => a -> f(a,p) })

    def bind[A, B](fa: DiscreteUnsafe[A])(f: (A) => DiscreteUnsafe[B]): DiscreteUnsafe[B] = fa.flatMap(f)

    def point[A](a: => A): DiscreteUnsafe[A] = new DiscreteUnsafe(Map(a -> 1))
  }
}
