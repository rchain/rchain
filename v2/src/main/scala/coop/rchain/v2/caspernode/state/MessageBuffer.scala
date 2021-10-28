package coop.rchain.v2.caspernode.state
import scala.collection.immutable.Set

object MessageBuffer {

  /**
   * Buffer containing received messages with valid signatures waiting to be replayed.
   *
   * As Casper message can be replayed only when all dependencies are replayed, all incoming messages are residing in
   * a buffer until that moment. Buffer does not have any structure, as it is not guaranteed that messages in a buffer
   * belong to the DAG (shard) that node is following or they are not a malicious attempt to present some false future.
   */
  type MessageBuffer[M] = Map[M, MessageStatus]

  trait MessageStatus

  /**
   * Message is received but cannot be replayed yet due to missing dependencies.
   *
   * @param received dependencies that are received but not replayed yet
   * @param pending   dependencies that are not received
   */
  final case class AwaitingDependencies[M](pending: Set[M], wanted: Set[M]) extends MessageStatus

  /**
   * Message is requested but not available in buffer yet. Request these on node restart.
   */
  final case object Requested extends MessageStatus

  /**
   * Message is in progress of replay. Start replaying these after restart.
   */
  final case object ValidationInProgress extends MessageStatus
}
