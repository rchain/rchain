package coop.rchain.sdk.block.state
import cats.syntax.all._
import coop.rchain.sdk.block.state.Requested._

import scala.concurrent.duration.FiniteDuration

/** State of block requester - blocks that node is aware of, but they are not received. */
final case class Requested[BId, Peer](st: RequestedState[BId, Peer]) {
  type ST = Requested[BId, Peer]

  /**
    * Record observation of a block.
    * @param bid        block id
    * @param sourceOpt  peer that sent block id (if provided)
    * @param now        timestamp
    * @return new state along with effects to be invoked
    */
  def add(bid: BId, sourceOpt: Option[Peer] = None, now: Long): (ST, RequestedEffect[Peer]) = {
    val curReqOpt = st.get(bid)
    val newReq = curReqOpt
      .map { curSt =>
        val curWaiting = curSt.waiting
        val newWaiting = sourceOpt.map(curWaiting + _).getOrElse(curWaiting)
        curSt.copy(waiting = newWaiting)
      }
      .getOrElse {
        val waiting = sourceOpt.map(Set(_)).getOrElse(Set.empty[Peer])
        RequestState(now, Set.empty[Peer], waiting)
      }
    val askPeerOpt = sourceOpt.flatMap { s =>
      curReqOpt.exists(_.waiting.nonEmpty).guard[Option].as(s)
    }
    // if no record for the block ID at all - search for peers that can provide the full block
    val searchForPeers = curReqOpt.isEmpty

    val newSt  = st.updated(bid, newReq)
    val effect = RequestedEffect(askPeerOpt, searchForPeers)
    (Requested(newSt), effect)
  }

  def ackBuffered(bid: BId): (ST, AckBufferedEffect) = {
    val newSt  = st - bid
    val effect = AckBufferedEffect()
    (Requested(newSt), effect)
  }

  /** Request all blocks that are timed out. */
  def requestAll(ageThreshold: FiniteDuration, now: Long): (ST, RequestAllEffect[BId, Peer]) = {
    val expired = st.filter { case (_, reqSt) => (now - reqSt.timestamp) > ageThreshold.toNanos }
    val adjusted = expired.mapValues { reqSt =>
      val nextPeerOpt = reqSt.waiting.headOption
      val newQueried  = nextPeerOpt.map(reqSt.queried + _).getOrElse(reqSt.queried)
      val newWaiting  = nextPeerOpt.map(_ => reqSt.waiting.tail).getOrElse(reqSt.waiting)
      (reqSt.copy(timestamp = now, queried = newQueried, newWaiting), nextPeerOpt)
    }
    val toRequest = adjusted.map { case (block, (_, nextPeerOpt)) => (block, nextPeerOpt) }.toList

    val newSt  = st ++ adjusted.mapValues(_._1)
    val effect = RequestAllEffect(toRequest)
    (Requested(newSt), effect)
  }

  def contains(bid: BId): Boolean = st.contains(bid)
}

object Requested {
  // State definition
  final case class RequestState[Peer](
      timestamp: Long,                // Last time block was requested
      queried: Set[Peer] = Set.empty, // Peers that were queried for this block
      waiting: Set[Peer] = Set.empty  // Peers that reportedly have block and are yet to be queried
  )
  type RequestedState[BId, Peer] = Map[BId, RequestState[Peer]]

  // Effects
  // adding block id to requested might trigger request of the full block from some peer,
  // or search for peers that have target block
  final case class RequestedEffect[Peer](askPeerOpt: Option[Peer], searchForPeers: Boolean)
  // no effects are required when acknowledging that block requested is buffered
  final case class AckBufferedEffect()
  // request blocks from some peers
  final case class RequestAllEffect[BId, Peer](toRequest: List[(BId, Option[Peer])])
}
