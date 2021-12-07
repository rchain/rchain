package coop.rchain.casper.merging

import cats.effect.Concurrent
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.ProcessedDeploy
import coop.rchain.rspace.merger.EventLogIndex

/** index of a single deploy */
final case class DeployIndex(deployId: ByteString, cost: Long, eventLogIndex: EventLogIndex)

object DeployIndex {
  def apply[F[_]: Concurrent](
      deploy: ProcessedDeploy,
      createEventLogIndex: (ProcessedDeploy) => F[EventLogIndex]
  ): F[DeployIndex] =
    for {
      eventLogIndex <- createEventLogIndex(deploy)
    } yield DeployIndex(deploy.deploy.sig, deploy.cost.cost, eventLogIndex)
}
