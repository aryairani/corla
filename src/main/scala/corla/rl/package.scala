package corla

import scalaz._
import syntax.functor._
import syntax.foldable1._
import std.tuple._
import std.anyVal._

package object rl {
	type PrefS[S] = S => Probability
	type PrefSA[S,A] = (S,A) => Probability

	type Utility = Reward
	type Reward = Double

	def averagePrefs[F[_]:Foldable1,S,A](prefs: F[PrefSA[S,A]]): PrefSA[S,A] =
		(s,a) => prefs.foldMap1(_.apply(s,a) -> 1) match {
			case (sum,count) => sum / count
		}

	def prefFromConstraintEnsemble[F[_]:Functor:Foldable1,S,A](cs: F[ConstraintSA[S,A]]): PrefSA[S,A] =
		averagePrefs(cs.map(_.toPref))
}
