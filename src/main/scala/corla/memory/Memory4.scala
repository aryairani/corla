package corla
package memory

import scalaz.{Functor, Traverse}
import scalaz.syntax.functor._
import Memory4.mapExperience

/** Abstract over `Next[S]`, the response from the environment */
// todo: fold `Reward` into Next?
trait Memory4[M, S, A, Next[_]] { outer =>
  implicit val N: Functor[Next]
  def addExperience: ((S, A, Reward, Next[S])) => M => M
  def addBatchExperience[F[_]:Traverse]: F[(S, A, Reward, Next[S])] => M => M

  def contramap[T](f: T => S) = new Memory4[M, T, A, Next] {
    implicit val N = outer.N
    def addExperience = outer.addExperience compose mapExperience(f)
    def addBatchExperience[F[_]:Traverse]: F[(T, A, Reward, Next[T])] => M => M =
      outer.addBatchExperience[F] compose (_ map mapExperience(f))
  }
}

object Memory4 {
  def mapExperience[U, V, A, Next[_]: Functor](f: U => V): ((U, A, Reward, Next[U])) => (V, A, Reward, Next[V]) = {
    case (u, a, r, next) => (f(u), a, r, next map f)
  }
  // todo def fromMemory3
  // todo def fromMemory2
}

trait EmptyMemory4[M, S, A, Next[_]] extends Memory4[M, S, A, Next] { outer =>
  def empty: M
  override def contramap[T](f: T => S): EmptyMemory4[M,T,A,Next] = new EmptyMemory4[M,T,A,Next] {
    implicit val N = outer.N
    def empty: M = outer.empty
    def addExperience = outer.addExperience compose mapExperience(f)
    def addBatchExperience[F[_]:Traverse] = outer.addBatchExperience[F] compose (_.map(mapExperience(f)))
  }
}

object EmptyMemory4 {
}