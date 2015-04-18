package corla.rl.v7
package agent

import corla.memory.Memory3
import corla.misc.PDF
import corla.rl.{Reward, Next}

import scalaz._
import syntax.monad._

case class LinCombiAgentA[S,A,P[_]:PDF](m: P[AbstractAgentA[S,A,P]])
  extends AbstractAgentA.Aux[S,A,P[AbstractAgentA[S,A,P]],P] {
  val M = new Memory3[M,S,A] {
    override def addExperience: ((S, A, Reward, Next[S, A])) => (M) => M =
      e => m => m.map(agent => AgentA(agent.π, agent.M.addExperience.apply(e)(agent.m))(agent.M))

    override def addBatchExperience[F[_] : Traverse]: (F[(S, A, Reward, Next[S, A])]) => (M) => M =
      fe => m => m.map(a => AgentA(a.π, a.M.addBatchExperience[F].apply(fe)(a.m))(a.M))
  }

  override def π: GenPolicyA[S, A, M, P] = (m,s) => as => m.flatMap(a => a.π(a.m,s)(as))
}
