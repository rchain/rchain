package coop.rchain.blockstorage.dag.state

import coop.rchain.models.BlockHash.BlockHash

import scala.collection.immutable.Set

object BlockDagBufferState {

  /** Buffer of messages to be validated. */
  type Buffer = Map[BlockHash, MessageStatus]

  trait MessageStatus
  final case object Requested                                         extends MessageStatus
  final case class AwaitingDependencies(dependencies: Set[BlockHash]) extends MessageStatus
  final case object ValidationInProgress                              extends MessageStatus
  def awaitingDependencies(d: Set[BlockHash]): MessageStatus = AwaitingDependencies(d)
}
