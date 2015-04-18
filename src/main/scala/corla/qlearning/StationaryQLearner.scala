package corla.qlearning

import corla.{Reward, Utility, Next, mapLensDefault}
import corla.memory.{EmptyMemory3, NativeSingle3, Memory3}
import corla.misc.{NESet, ScalazLensCompatibility}
import monocle.macros.Lenses
import NESet._

import scalaz._, Scalaz._
import ScalazLensCompatibility._
/**
* Created by arya on 12/20/14.
*/

/** Memory structure for a stationary q-learner.
  * Includes sparse q-table // todo could be made sparser
  * N_sa table for weighting new samples
  * discount factor
  * default q value for unknown transitions
  */
@Lenses
case class StationaryQLearner[S,A](qtable: Map[(S,A),Utility],
                                   nsa: Map[(S,A),Long],
                                   discount: Double,
                                   default: Utility)
object StationaryQLearner
  extends StationaryQLearnerInstances
  with StationaryQLearnerFunctions


trait StationaryQLearnerLenses {
  import monocle._
  import StationaryQLearner._
  type SQL[S,A] = StationaryQLearner[S,A]
  type QTable[S,A] = Map[(S,A),Utility]
  type Nsa[S,A] = Map[(S,A),Long]

  def q[S,A](s:S)(a:A) = Lens[SQL[S,A],Utility](
    t => t.qtable.getOrElse(s -> a, t.default)
  )(q => t => t.copy(qtable = t.qtable.updated(s -> a, q)))

  def nsaValue[S,A](s:S,a:A) = nsa[S,A] ^|-> mapLensDefault(s -> a, 0)

  def qlookup[S,A](t: SQL[S,A])(s:S)(a:A): Utility = q(s)(a).get(t)
}

trait StationaryQLearnerFunctions extends StationaryQLearnerLenses {
  import StationaryQLearner._
  def empty[S,A](discount: Double, default: Utility): StationaryQLearner[S,A] =
    StationaryQLearner[S,A](Map(), Map(), discount, default)

  def addExperience[S,A]: (S, A, Reward, Next[S, A]) => (StationaryQLearner[S, A]) => StationaryQLearner[S, A] =
    (s,a,r,next) =>
      (for {
        newN <- nsaValue(s,a).mods(1+_)
        α = 1.0 / newN
        qlookup <- State.gets(qlookup[S,A])
        γ <- State.gets(discount[S,A].get)
        defaultQ <- State.gets(default[S,A].get)
        Next(s2,maybeActions) = next
        maxNextQ = maybeActions.map(_.map(qlookup(s2)).maximum1).getOrElse(defaultQ)
        newQ = r + γ * maxNextQ // a terminal state is one with no actions?
        _ <- q(s)(a).mods(oldQ => (1-α) * oldQ + α * newQ)
      } yield ()).exec(_)
}

trait StationaryQLearnerInstances {
  implicit def qlearner[S,A] = new StationaryQLearner0[S,A] {}

  def emptyqlearner[S,A](_discount: Double, _default: Utility): EmptyStationaryQLearner[S,S,A] =
    new StationaryQLearner0[S,A] with EmptyStationaryQLearner[S,S,A] {
      def discount: Double = _discount
      def default: Utility = _default
    }
}

trait StationaryQLearner0[S,A]
  extends StationaryQLearner1[S,S,A]
  with NativeSingle3[StationaryQLearner[S,A],S,A]
{
  def addExperience = StationaryQLearner.addExperience[S,A].tupled
  def qlookup = StationaryQLearner.qlookup[S,A]
}

trait StationaryQLearner1[SM,S,A]
  extends QLearner[StationaryQLearner[SM,A],S,A]
  with Memory3[StationaryQLearner[SM,A],S,A]

trait EmptyStationaryQLearner[SM,S,A]
  extends StationaryQLearner1[SM,S,A]
  with EmptyMemory3[StationaryQLearner[SM,A],S,A] { self =>

  def discount: Double
  def default: Utility
  def empty: StationaryQLearner[SM, A] = StationaryQLearner.empty(discount,default)

  override def contramap[T](f: T => S): EmptyStationaryQLearner[SM,T,A] =
    new EmptyStationaryQLearner[SM,T,A] {
      def discount: Double = self.discount
      def default: Utility = self.default
      override def empty = self.empty
      import Memory3.mapExperience2
      def addExperience = self.addExperience compose mapExperience2[T,S,A](f)
      def addBatchExperience[F[_]:Traverse] = self.addBatchExperience[F] compose (_.map(mapExperience2(f)))
      def qlookup = m => t => a => self.qlookup(m)(f(t))(a)
    }
}
