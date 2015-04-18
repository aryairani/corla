package corla.rl.v7

import corla.rl.{Reward, Next}

trait GenerativeModel3[F[_],S,A] {
  def sample(s: S, a: A): F[(Reward,Next[S,A])]
}

trait ExternalModel3[F[_],S,A] {
  def getState: F[Next[S, A]]
  def act(a: A): F[(Reward, Next[S, A])]
}

trait ExternalLagModel3[F[_],S,A] extends ExternalModel3[F,S,A] {
  /**
   * submit an action `a`, and receive an updated value for current state
   * (in addition to the reward and next observed state)
   *
   * The newly observed, pre-action, current state may be different from from the previously observed
   * current state that the agent used to select an action.  This method is useful if the agent wants
   * to learn something about the lag in the environment
   * @param a
   * @return
   */
  def checkAndAct(a: A): F[(S,Reward,Next[S,A])]
}

trait AbstractModel3[Model[_[_],_,_],F[_],S,A] {
  def act(model: Model[F,S,A], stateHint: S, a: A): F[(Reward,Next[S,A])]
}

object AbstractModel3 {

  implicit def generativeModel[F[_],S,A] = new AbstractModel3[GenerativeModel3,F,S,A] {
    def act(model: GenerativeModel3[F, S, A], stateHint: S, a: A): F[(Reward, Next[S, A])] =
      model.sample(stateHint,a)
  }

  /** this instance *ignores* the stateHint, because the external model's state isn't necessarily known */
  implicit def externalModel[F[_],S,A] = new AbstractModel3[ExternalModel3,F,S,A] {
    def act(model: ExternalModel3[F, S, A], stateHint: S, a: A): F[(Reward, Next[S, A])] =
      model.act(a)
  }
}


