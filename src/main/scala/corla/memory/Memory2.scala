package corla.memory

import corla.Reward

import scalaz._

/** Receives next `s` when adding experience */
trait Memory2[M,S,A] { self =>
  def addExperience: ((S,A,Reward,S)) => M => M
  def addBatchExperience[F[_]:Traverse]: F[(S,A,Reward,S)] => M => M

  def contramap[T](f: T => S) = new Memory2[M,T,A] {
    import Memory2.mapExperience

    import scalaz.syntax.functor._

    def addExperience: ((T, A, Reward, T)) => (M) => M =
      self.addExperience compose mapExperience(f)

    def addBatchExperience[F[_]:Traverse]: (F[(T, A, Reward, T)]) => (M) => M =
      self.addBatchExperience[F] compose (_.map(mapExperience(f)))
  }

}

object Memory2 {
  def mapExperience[S,T,A](f: S => T): ((S,A,Reward,S)) => (T,A,Reward,T) = {
    case (s1,a,r,s2) => (f(s1),a,r,f(s2))
  }


  implicit def primitive[S,A] = new EmptyMemory2[Unit,S,A] {
    def empty: Unit = ()
    def addExperience: ((S, A, Reward, S)) => (Unit) => Unit =
      _ => _ => ()

    def addBatchExperience[F[_] : Traverse]: (F[(S, A, Reward, S)]) => (Unit) => Unit =
      _ => _ => ()
  }
}

trait EmptyMemory2[M,S,A] extends Memory2[M,S,A] {
  def empty: M
}

/** delegates addBatchExperience to addExperience */
trait NativeSingle[M,S,A] extends Memory2[M,S,A] {
  import syntax.traverse._
  final def addBatchExperience[F[_]:Traverse]: F[(S,A,Reward,S)] => M => M =
    _.traverseU(addExperience andThen Endo.apply).run
}

/** delegates addExperience to addBatchExperience */
trait NativeBatch[M,S,A] extends Memory2[M,S,A] {
  final def addExperience: ((S,A,Reward,S)) => M => M = {
    case e => addBatchExperience.apply(e)
  }
}
