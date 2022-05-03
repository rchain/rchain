package coop.rchain.blockstorage.dag

import cats.syntax.all._

import scala.collection.compat.immutable.LazyList

/**
  * Message (view) with all necessary data for finalizer
  *
  * @param id message unique identifier
  * @param height message height (number)
  * @param sender message sender identifier
  * @param senderSeq sender's sequence number
  * @param bondsMap bonds map used to calculate next fringe
  * @param parents justification for the message
  * @param fringe fringe observed by the message
  * @param seen all previous messages seen by the message
  * @tparam M abstract type for identifier of the message
  * @tparam S abstract type for identifier of the sender
  */
final case class Message[M, S](
    id: M,
    height: Long,
    sender: S,
    senderSeq: Long,
    bondsMap: Map[S, Long],
    parents: Set[M],
    fringe: Set[M],
    // Cache of seen message ids
    seen: Set[M]
) {
  override def hashCode(): Int = this.id.hashCode()
}

/**
  * Multi-parent finalization implementation
  *
  * @param msgViewMap cache of all messages accessed by message id
  * @tparam M abstract type for identifier of the message
  * @tparam S abstract type for identifier of the sender
  */
final case class Finalizer[M, S](msgViewMap: Map[M, Message[M, S]]) {
  // Helper function to lazily unfold iterator function
  def unfold[A](init: A)(f: A => Iterator[A]): LazyList[A] =
    LazyList.unfold(f(init)) { iter =>
      if (!iter.hasNext) none
      else {
        val x = iter.next()
        Some((x, iter ++ f(x)))
      }
    }

  // Iterate self parent messages
  def selfParents(mv: Message[M, S], finalized: Set[Message[M, S]]): Seq[Message[M, S]] =
    unfold(mv) { m =>
      m.parents.map(msgViewMap).filter(x => x.sender == mv.sender && !finalized(x)).toIterator
    }

  /**
    * Checks if minimum messages are enough for next fringe calculation
    */
  def checkMinMessages(minMsgs: List[Message[M, S]], bondsMap: Map[S, Long]): Boolean =
    // TODO: add support for epoch changes, simple comparison for senders count is not enough
    minMsgs.size == bondsMap.size

  /**
    * Finds top messages referenced from minimum messages (next message from each sender)
    */
  def calculateNextLayer(minMsgs: List[Message[M, S]]): Map[S, Message[M, S]] = {
    val minMessagesMap = minMsgs.map(x => (x.sender, x)).toMap
    minMsgs
      .flatMap(_.parents.map(msgViewMap))
      .filter(x => minMessagesMap.keySet.contains(x.sender))
      .foldLeft(minMessagesMap) {
        case (acc, m) =>
          val currMin = acc(m.sender)
          val newMin =
            if (m.senderSeq > currMin.senderSeq) m
            else currMin
          acc + ((m.sender, newMin))
      }
  }

  /**
    * Creates supported Map (senders see each other on next layer) for next fringe for each justification (sender)
    */
  def calculateNextFringeSupportMap(
      parents: Set[Message[M, S]],
      nextLayer: Map[S, Message[M, S]],
      finalized: Set[Message[M, S]]
  ): Map[S, Map[S, Set[S]]] =
    parents.map { mv =>
      val seenBy = nextLayer
        .map {
          case (s, minMsg) =>
            // Search parents of parent if not part of next layer
            val parentsOfParent = mv.parents -- nextLayer.values.map(_.id)
            val seeMinMsg = parentsOfParent
              .map(msgViewMap)
              .map { p =>
                // Find if next layer message is seen from any parent message
                val selfMsgs     = p +: selfParents(p, finalized)
                val seenByParent = selfMsgs.exists(_.seen.contains(minMsg.id))
                (p.sender, seenByParent)
              }
              .filter(_._2)
              .toMap
              .keySet
            (s, seeMinMsg)
        }
        .filter(_._2.nonEmpty)
      (mv.sender, seenBy)
    }.toMap

  /**
    * Calculate next finalization fringe based on next fringe supported Map (senders see each other on next layer)
    */
  def calculateFringe(
      nextFringeSupportMap: Map[S, Map[S, Set[S]]],
      bondsMap: Map[S, Long]
  ): Boolean = {
    // Calculate stake supporting full partition
    val bondedSenders = bondsMap.keySet
    val seeFullPartition = nextFringeSupportMap
      .mapValues(x => x.nonEmpty && x.values.forall(_ == bondedSenders))
      .filter(_._2)
      .keys
    val fullPartitionStake = seeFullPartition.toSeq.map(bondsMap).sum
    // Total stake
    val totalStake = bondsMap.values.toSeq.sum
    // Calculate if 2/3 of stake supporting next layer
    fullPartitionStake.toDouble / totalStake > 2d / 3
  }

  /**
    * Finalization logic
    *
    * @param justifications justification seen by the new message
    * @param bondsMap       bonds map used to validate messages built on parent fringe
    * @return fringe from joined justifications and a new detected fringe
    */
  def calculateFinalization(
      justifications: Set[Message[M, S]],
      bondsMap: Map[S, Long]
  ): (Set[Message[M, S]], Option[Set[Message[M, S]]]) = {
    // Calculate next fringe from previous fringe
    def nextFringe(prevFringe: Set[Message[M, S]]): Option[Set[Message[M, S]]] =
      for {
        // Find minimum message from each sender from justifications
        minMsgs <- justifications.toList.traverse(p => (p +: selfParents(p, prevFringe)).lastOption)

        // Check if min messages satisfy requirements (senders in bonds map)
        _ <- checkMinMessages(minMsgs, bondsMap).guard[Option]

        // Include ancestors of minimum messages as next layer
        nextLayer = calculateNextLayer(minMsgs)

        // Create next fringe support Map map for each justification (sender)
        fringeSupportMap = calculateNextFringeSupportMap(justifications, nextLayer, prevFringe)

        // Calculate partition and resulting finalization fringe
        fringeFound = calculateFringe(fringeSupportMap, bondsMap)

        // Use next layer messages if fringe is detected
        fringe <- fringeFound.guard[Option].as(nextLayer.values.toSet)
      } yield fringe

    // Latest fringe seen from justifications
    // - can be empty which means first layer is first message from each sender
    val parentFringe =
      justifications.toList
        .maximumByOption(_.fringe.map(msgViewMap).toList.map(_.height).maximumOption.getOrElse(-1L))
        .map(_.fringe)
        .getOrElse(Set())
        .map(msgViewMap)

    // Find top most fringe
    // - multiple fringes can be finalized at once
    val newFringeOpt = LazyList.unfold(parentFringe)(nextFringe(_).map(nf => (nf, nf))).lastOption

    (parentFringe, newFringeOpt)
  }
}
