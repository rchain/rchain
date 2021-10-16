package coop.rchain.casper.merging

import cats.effect.Concurrent
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.Event
import coop.rchain.rspace.merger.EventLogIndex

/** index of a single deploy */
final case class DeployIndex(
    deployId: ByteString,
    cost: Long,
    eventLogIndex: EventLogIndex
)

object DeployIndex {
  // This cost is required because rejection option selection rule depends on how much branch costs.
  // For now system deploys do not have any weight, cost is 0.
  val SYS_SLASH_DEPLOY_COST       = 0L
  val SYS_CLOSE_BLOCK_DEPLOY_COST = 0L
  val SYS_EMPTY_DEPLOY_COST       = 0L
  // These are to be put in rejected set in blocks, so prefix format is defined for identification purposes.
  val SYS_SLASH_DEPLOY_ID       = ByteString.copyFrom(64.toByte +: new Array[Byte](31))  // 1000000xxx
  val SYS_CLOSE_BLOCK_DEPLOY_ID = ByteString.copyFrom(96.toByte +: new Array[Byte](31))  // 1100000xxx
  val SYS_EMPTY_DEPLOY_ID       = ByteString.copyFrom(112.toByte +: new Array[Byte](31)) // 1110000xxx

  def apply[F[_]: Concurrent](
      sig: ByteString,
      cost: Long,
      events: List[Event],
      createEventLogIndex: List[Event] => F[EventLogIndex]
  ): F[DeployIndex] = createEventLogIndex(events).map(DeployIndex(sig, cost, _))
}
