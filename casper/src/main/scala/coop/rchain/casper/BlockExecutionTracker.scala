package coop.rchain.casper

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.syntax.all._
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.rholang.interpreter.EvaluateResult

trait BlockExecutionTracker[F[_]] {
  def execStarted(d: DeployId): F[Unit]
  def execComplete(d: DeployId, res: EvaluateResult): F[Unit]
}

sealed trait DeployStatus
case object DeployStatusStarted                    extends DeployStatus
final case class DeployStatusError(status: String) extends DeployStatus

final class StatefulExecutionTracker[F[_]: Sync] extends BlockExecutionTracker[F] {
  private val state = Ref.unsafe(Map.empty[DeployId, DeployStatus])

  override def execStarted(d: DeployId): F[Unit] = state.update(_ + (d -> DeployStatusStarted))
  override def execComplete(d: DeployId, res: EvaluateResult): F[Unit] =
    if (res.succeeded) {
      // When deploy succeeded, remove from tracking
      state.update(_ - d)
    } else {
      // If deploy fails update status with errors
      // TODO: error can have empty message
      val err = res.errors.map(_.getMessage).mkString("\n")
      state.update(_ + (d -> DeployStatusError(err)))
    }

  def findDeploy(d: DeployId): F[Option[DeployStatus]] = state.get.map(_.get(d))
}
