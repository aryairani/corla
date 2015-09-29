package corla.qlearning

import corla.memory.{NativeSingle3, Memory3}
import corla.Utility

trait QLookup[M,S,A] {
  def qlookup: M => S => A => Utility
}

trait QLearner[M,S,A] extends Memory3[M,S,A] with QLookup[M,S,A] { self =>
  override def contramap[T](f: T => S): QLearner[M,T,A] =
    new QLearner[M,T,A] with NativeSingle3[M,T,A] {
      def addExperience = self.addExperience compose Memory3.mapExperience2[T,S,A](f)
      def qlookup = m => t => a => self.qlookup(m)(f(t))(a)
    }
}

