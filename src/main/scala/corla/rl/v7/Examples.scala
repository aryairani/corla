package corla.rl.v7

import corla.misc.{NESet, DiscreteH}
import corla.rl.{Reward, Next}
import corla.rl.v7.agent.AgentA
import qlearning.{QLearner, StationaryQLearner}

import scalaz._, Id._, Maybe._
import scalaz.effect.IO

/**
 * Created by arya on 12/22/14.
 */
object Examples {
  type FullState = (Int,SimpleState)

  sealed trait SimpleState
  case object Start extends SimpleState
  case object Finish extends SimpleState

  sealed trait MyAction
  case object Left extends MyAction
  case object Right extends MyAction

  // Initialize a new Q-learner over `SimpleState`
  val approxQT = StationaryQLearner.empty[SimpleState,MyAction](.99999, 0)
  val featureExtractor: FullState => SimpleState = _._2

  val canLearnSimple: QLearner[StationaryQLearner[SimpleState,MyAction],SimpleState,MyAction] = implicitly

  // Adapt the Q-learner over `SimpleState` to learn over `FullState` using `featureExtractor`
  val canLearnFull: QLearner[StationaryQLearner[SimpleState,MyAction],FullState,MyAction] =
    canLearnSimple.contramap(featureExtractor)

  import scalaz.Maybe._
  val mySimpleOption =
    SMOption[SimpleState,MyAction,Unit,Id]((m,s) => if (s == Start) just(Right) else empty)
}

/** Toy problems from http://artint.info/html/ArtInt_262.html */
object ArtInt262 {
//  import Trampoline._

  object fig11_8 {

    sealed trait S
    case object S0 extends S
    case object S1 extends S
    case object S2 extends S
    case object S3 extends S
    case object S4 extends S
    case object S5 extends S

    sealed trait A
    sealed trait AA extends A
    case object UpC extends AA
    case object Right extends AA
    case object Left extends AA
    case object Up extends A
    val allActions = NESet[A](UpC,Right,Left,Up)

    /** Up is a nondeterministic action that is defined in terms of the other actions :-\  */
    def adjustActions: A => DiscreteH[(AA,Reward=>Reward)] = {
      case Up => DiscreteH.weightedUnsafe(Map(
        ((UpC, _ => 0.0), 0.8),
        ((Left, identity), 0.1),
        ((Right, identity), 0.1)
      ))
      case Right => DiscreteH.uniform_((Right,identity))
      case Left => DiscreteH.uniform_((Left,identity))
      case UpC => DiscreteH.uniform_((UpC,identity))
    }

    def move: AA => S => S = {
      case UpC => {
        case S4 => S4  case S5 => S5
        case S2 => S4  case S3 => S5
        case S0 => S2  case S1 => S3
      }
      case Right => {
        case S4 => S5
        case S2 => S3
        case S0 => S1
        case other => other
      }
      case Left => {
        case S5 => S4
        case S3 => S2
        case S1 => S0
        case S4 => S0
        case other => other
      }
    }
    def reward: AA => S => Reward = {
      case UpC => _ => -1.0
      case Right => {
        case S0|S2|S4 => 0
        case _ => -1
      }
      case Left => {
        case S1|S3|S5 => 0
        case S0 => -1
        case S2 => -100
        case S4 => 10
      }
    }

    val g = new GenerativeModel3[IO,S,A] {
      def sample(s: S, _a: A): IO[(Reward, Next[S, A])] =
        adjustActions(_a).sampleIO().map {
          case (aa, alterReward) => (reward(aa)(s), Next(move(aa)(s), just(allActions)))
        }
    }

    val memory = StationaryQLearner.empty[S,A](.9, 0)
    val egreedy = policy.epsilonGreedy[S,A,StationaryQLearner[S,A],DiscreteH](.2)
    val agent = AgentA(egreedy,memory)

    // this will never return, because the transition function isn't "proper" (isn't guaranteed to terminate)
    val startConfig = Next[S,A](S1, just(allActions))
//    runSimple.runTrialAgentA(startConfig, agent, g)
    runAbstract.runTrialPolicyA(startConfig, egreedy, memory, g)
  }

}
