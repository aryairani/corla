package corla.rl

import corla.misc.{NESet, PDF}
import NESet._
import PDF.pdfSyntax

import scalaz.Maybe.empty

/**
 * v7 includes:
 * Actions provided to APolicy, Memory3
 *   Policies with and without available actions
 *    ('without' may be going away)
 *
 *   Memory updates that do provide next-state's available actions
 *
 * Created by arya on 12/20/14.
 */
package object v7
  extends v7types
  with v7syntax
  with v7mixfunctions
{

  def optionIsAvailable[S,A,M,P[_]:PDF](m: M, s: S)(o: GenOption[S,A,M,P]): Boolean =
    o(m,s)(empty) < 1

  def availableOptions[S,A,M,P[_]:PDF](m: M, s: S, allOptions: NESet[GenOption[S,A,M,P]]): Set[GenOption[S,A,M,P]] =
    allOptions.filter(optionIsAvailable(m,s))

  def checkAction[A](as: Actions[A])(a: A): A = { assert(as.contains(a)); a }


}
