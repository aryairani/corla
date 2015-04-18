package corla

/**
 * Created by arya on 12/8/14.
 */
sealed trait ConstraintDecision
object ConstraintDecision {
  case object Avoid extends ConstraintDecision
  case object Unspecified extends ConstraintDecision

  def toPref: ConstraintDecision => Double = {
    case Avoid => 0
    case Unspecified => 1
  }
}

case class ConstraintS[S](c: S => ConstraintDecision) {
  def toPref: PrefS[S] = s => ConstraintDecision.toPref(c(s))
}
case class ConstraintSA[S,A](c: S => A => ConstraintDecision) {
  def apply(s:S,a:A) = c(s)(a)
  def toPref: PrefSA[S,A] = (s,a) => ConstraintDecision.toPref(c(s)(a))
}

