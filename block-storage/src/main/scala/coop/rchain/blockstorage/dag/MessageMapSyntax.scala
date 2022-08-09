package coop.rchain.blockstorage.dag

import cats.syntax.all._
import coop.rchain.sdk.syntax.all.mapSyntax

trait MessageMapSyntax {
  implicit def blockStorageSyntaxMessageMap[M, S](
      msgMap: Map[M, Message[M, S]]
  ): MessageMapSyntaxOps[M, S] =
    new MessageMapSyntaxOps[M, S](msgMap)
}

final class MessageMapSyntaxOps[M, S](private val msgMap: Map[M, Message[M, S]]) extends AnyVal {

  type Msg = Message[M, S]

  /**
    * Gets the slice of messages between upper and lower bound (including upper bound messages)
    */
  def between(upperBound: Set[Msg], lowerBound: Set[Msg]): Set[Msg] = {
    val upperSeen = upperBound.flatMap(_.seen.map(msgMap))
    val lowerSeen = lowerBound.flatMap(_.seen.map(msgMap))
    upperSeen -- lowerSeen
  }

  /**
    * Latest fringe seen from justifications
    * - can be empty which means first layer is the first message from each sender
    */
  // TODO: should fringes read bonds map from each round if multiple are finalized???
  def latestFringe(justifications: Set[Message[M, S]]): Set[Message[M, S]] =
    justifications.toList
      .maximumByOption(_.fringe.map(msgMap).toList.map(_.height).maximumOption.getOrElse(-1L))
      .map(_.fringe)
      .getOrElse(Set())
      .map(msgMap)

  /**
    * Lowest fringe for input messages
    */
  def lowestFringe(msgs: Set[Message[M, S]]): Set[Message[M, S]] =
    msgs.toList
      .minimumByOption(_.fringe.map(msgMap).toList.map(_.height).minimumOption.getOrElse(-1L))
      .map(_.fringe)
      .getOrElse(Set())
      .map(msgMap)

  /**
    * Finds a message with empty parents
    */
  def findWithEmptyParents: Option[Msg] = msgMap.values.find(_.parents.isEmpty)

  /**
    * The highest fringe that is not required for merging.
    */
  def pruneFringe(
      finalFringe: Set[M],
      childMap: Map[M, Set[M]]
  ): Set[Message[M, S]] = lowestFringe(finalFringe.flatMap(childMap).map(msgMap.getUnsafe))
}
