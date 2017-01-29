package corla.memory

import corla._
import corla.memory.Memory3.mapExperience2
import corla.memory.Memory3.Next

import scalaz._
import scalaz.syntax.traverse._

/** Receives Next s/as when adding experience */
trait Memory3[M,S,A] { self =>
  def addExperience: ((S,A,Reward,Next[S,A])) => M => M
  def addBatchExperience[F[_]:Traverse]: F[(S,A,Reward,Next[S,A])] => M => M

  def contramap[T](f: T => S) = new Memory3[M,T,A] {
    def addExperience = self.addExperience compose mapExperience2[T,S,A](f)
    def addBatchExperience[F[_]:Traverse] = self.addBatchExperience[F] compose (_.map(mapExperience2(f)))
  }
}

/** Mix-in to delegate addExperience to addBatchExperience */
trait NativeBatch3[M,S,A] extends Memory3[M,S,A] {
  def addExperience: ((S,A,Reward,Next[S,A])) => M => M =
    addBatchExperience.apply
}

/** Mix-in to delegate addBatchExperience to addExperience */
trait NativeSingle3[M,S,A] extends Memory3[M,S,A] {
  /** applies the updates in reverse order -- should be more efficient than reverse order */
  def addBatchExperience[F[_]:Traverse]: F[(S,A,Reward,Next[S,A])] => M => M =
    _.traverseU(addExperience andThen Endo.apply).run
}

object Memory3 {
  def mapExperience2[S,T,A](f: S => T)(e: ExperienceA[S,A]): ExperienceA[T,A] = e.map(f)

  implicit def markov[S,A] = new EmptyMemory3[Unit,S,A] {
    def empty: Unit = ()
    def addExperience = _ => _ => ()
    def addBatchExperience[F[_] : Traverse] = _ => _ => ()
  }

  implicit def fromMemory2[M,S,A](implicit M: Memory2[M,S,A]) = new Memory3[M,S,A] {
    def removeNextActions: ((S,A,Reward,Next[S,A])) => (S,A,Reward,S) = {
      case (s,a,r,Next(s2,_)) => (s,a,r,s2)
    }

    def addExperience: ((S,A,Reward,Next[S,A])) => M => M =
      M.addExperience compose removeNextActions

    def addBatchExperience[F[_]:Traverse]: F[(S,A,Reward,Next[S,A])] => M => M =
      M.addBatchExperience[F] compose (_.map(removeNextActions))
  }

  case class Next[S,A](s: S, as: Maybe[Actions[A]]) {
    def map[T](f: S => T): Next[T,A] = Next(f(s), as)
  }

  object Next {
    def just[S,A](s: S, as: Actions[A]) = Next[S,A](s, Maybe.just(as))
  }

}

/** Memory types that implement Empty can be instantiated without extra parameters */
trait EmptyMemory3[M,S,A] extends Memory3[M,S,A] { self =>
  def empty: M
  override def contramap[T](f: T => S): EmptyMemory3[M,T,A] = new EmptyMemory3[M,T,A] {
    def empty: M = self.empty
    def addExperience = self.addExperience compose mapExperience2[T,S,A](f)
    def addBatchExperience[F[_]:Traverse] = self.addBatchExperience[F] compose (_.map(mapExperience2(f)))
  }
}
object EmptyMemory3 {
  implicit def fromEmptyMemory2[M,S,A](implicit M2: EmptyMemory2[M,S,A], M3: Memory3[M,S,A]) = new EmptyMemory3[M,S,A] {
    def empty: M = M2.empty
    def addExperience = M3.addExperience
    def addBatchExperience[F[_] : Traverse] = M3.addBatchExperience[F]
  }
}
