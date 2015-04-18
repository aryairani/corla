import scalaz._, Ordering._, Maybe._
import monocle.Lens

/**
 * Created by arya on 12/8/14.
 */
package object corla {
  type Probability = Double
  implicit val probabilityAdditiveMonoid = new Monoid[Probability] {
    def zero: Probability = 0

    def append(f1: Probability, f2: => Probability): Probability = f1 + f2
  }

  def argmaxesBy[F[_],A,B](fa: F[A])(f: A => B)(implicit F: Foldable[F], ord: math.Ordering[B]): Set[A] =
    F.foldl[A,(Set[A],Option[B])](fa,(Set(),None)) {
      case (_,None) =>
        a => Set(a) -> Some(f(a))

      case maxes @ (as, bs @ Some(b0)) =>
        a =>
          val b = f(a)
          fromInt(ord.compare(b,b0)) match {
            case GT => // a > max
              Set(a) -> Some(b)
            case EQ => // a = max
              (as + a, bs)
            case LT => // a < max
              maxes
          }
    } ._1

  /** [0,1) */
  def random = effect.IO { scala.math.random }

  // todo: rewrite this to short-circuit
  def findFirstSt[F[_]:Foldable,S,A0](fa: F[A0], st: A0 => State[S,Boolean], s: S): (S,Maybe[A0]) =
    Foldable[F].foldl(fa,(s,empty[A0])) {
      case done @ (_,Just(a)) => a => done
      case looking @ (s,Empty()) => a =>
        val (s2,done) = st(a)(s)
        if (done) (s2,just(a)) else (s2,empty)
    }

  def findFirstSt_[F[_]:Foldable,S,A0](fa: F[A0])(st: A0 => State[S,Boolean])(s: S): Maybe[A0] =
    findFirstSt(fa,st,s)._2

  def findFirstStOrDefault[F[_]:Foldable,S,A0](fa: F[A0], st: A0 => State[S,Boolean], s: S, default: A0): A0 =
    findFirstSt(fa,st,s)._2.getOrElse(default)

  /** map helpers, copied from util/package */
  def mapWith[A,B](default:B) = Map[A,B]().withDefaultValue(default)
  def mapLensDefault[K, V](k: K, default: V) =
    Lens[Map[K, V],V](_.getOrElse(k,default))(v => _.updated(k,v).withDefaultValue(default))
}

