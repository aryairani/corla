package corla.rl

import ConstraintDecision._

/**
 * Created by arya on 12/8/14.
 */
sealed trait ConstraintDecision
object ConstraintDecision {
  case object Avoid extends ConstraintDecision
  case object Unspecified extends ConstraintDecision
}

case class ConstraintS[S](c: S => ConstraintDecision) {
  def toPref: PrefS[S] = s => if (c(s) == Avoid) 0 else 1
}
case class ConstraintSA[S,A](c: S => A => ConstraintDecision) {
  def apply(s:S,a:A) = c(s)(a)
  def toPref: PrefSA[S,A] = (s,a) => if (c(s)(a) == Avoid) 0 else 1
}

