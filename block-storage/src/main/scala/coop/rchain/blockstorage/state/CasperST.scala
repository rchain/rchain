package coop.rchain.blockstorage.state

import coop.rchain.blockstorage.state.CasperST._
import coop.rchain.models.Validator.Validator

import scala.collection.immutable.{Set, SortedMap}

case class CasperST[M](
    // messages that Casper is aware but they are not validated yet
    private val d: Map[M, MessageStatus],
    // remaining below is the index of validated messages
    dagSet: Set[M],
    latestMessagesMap: Map[Validator, M],
    childMap: Map[M, Set[M]],
    heightMap: SortedMap[Long, Set[M]],
    invalidBlocksSet: Set[M],
    lastFinalizedBlock: (M, Long),
    finalizedBlocksSet: Set[M]
) {
  // accept message if it is valid
  def add(m: M, dependencies: Set[M]): (CasperST[M], Boolean, Set[M], Set[M]) = {
    val missingSet = dependencies.diff(dagSet)
    // whether all dependencies are validated, so message is ready for validation
    val ready = missingSet.isEmpty
    // messages that state is not aware of should be requested
    val toRequest = missingSet.filterNot(d.contains)
    val nextSt = {
      val mStatus = if (ready) ReceivedReady else ReceivedWaiting(missingSet)
      CasperST(
        toRequest.foldLeft((d + (m -> mStatus)))((st, mis) => st + (mis -> Requested)),
        dagSet,
        latestMessagesMap,
        childMap,
        heightMap,
        invalidBlocksSet,
        lastFinalizedBlock,
        finalizedBlocksSet
      )
    }

    (nextSt, ready, missingSet, toRequest)
  }

  // return new state and set of hashes that are free of dependencies after this block added
  def validated(m: M): (CasperST[M], Set[M]) = {
    require(
      d.contains(m),
      "Casper message processor state is inconsistent, calling 'done' on unknown message."
    )
    require(
      !dagSet.contains(m),
      "Casper message processor state is inconsistent, calling 'done' on validated message."
    )
    val (newReady, toAdjust) = d
      .collect {
        case (h, ReceivedWaiting(missing)) if missing.contains(h) =>
          h -> ReceivedWaiting(missing - h)
      }
      .partition { case (_, ReceivedWaiting(v)) => v.isEmpty }
    val newProcessingSet = (newReady.toIterator ++ toAdjust).foldLeft(d) {
      case (acc, (m, ReceivedWaiting(missing))) =>
        if (missing.isEmpty) acc + (m -> ReceivedReady)
        else acc + (m                 -> ReceivedWaiting(missing))
    }
    val newSt = this.copy(d = newProcessingSet, dagSet = dagSet + m)
    (newSt, newReady.map { case (m, _) => m }.toSet)
  }

  def known(m: M): Boolean                   = d.contains(m) || dagSet.contains(m)
  def beforeFinalized(height: Long): Boolean = lastFinalizedBlock._2 >= height
}

object CasperST {
  trait MessageStatus
  // requested from the network
  case object Requested extends MessageStatus
  // received and accepted by message processor
  case object ReceivedReady extends MessageStatus
  // cannot be processed as dependency messages are not processed
  case class ReceivedWaiting[M](dependencies: Set[M]) extends MessageStatus
  // processing is in progress
  case object Processing extends MessageStatus
}
