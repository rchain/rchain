package coop.rchain.casper

import cats.effect.concurrent.Ref
import cats.syntax.all._
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.rholang.interpreter.EvaluateResult

trait BlockExecutionTracker[F[_]] {
  def execStarted(d: DeployId): F[Unit]
  def execComplete(d: DeployId, res: EvaluateResult): F[Unit]
}

sealed trait DeployStatus
case object DeployStatusSuccess                    extends DeployStatus
final case class DeployStatusError(status: String) extends DeployStatus

final class ExecutionTracker[F[_]](state: Ref[F, Map[DeployId, Option[DeployStatus]]])
    extends BlockExecutionTracker[F] {
  override def execStarted(d: DeployId): F[Unit] = state.update(_ + (d -> none[DeployStatus]))
  override def execComplete(d: DeployId, res: EvaluateResult): F[Unit] = {
    val err    = res.errors.map(_.getMessage).mkString("\n")
    val status = if (err.isBlank) DeployStatusSuccess else DeployStatusError(err)
    state.update(_ + (d -> status.some))
  }
}

object ExecutionTracker {
  def apply[F[_]](state: Ref[F, Map[DeployId, Option[DeployStatus]]]) =
    new ExecutionTracker[F](state)
}
