package coop.rchain.blockstorage.casper
import cats.effect.Sync
//import cats.effect.Sync
//import cats.syntax.all._
//
///**
//  * Casper protocol with merge on finalization.
//  * Has one public method [[run]].
//  * @param curFringe current finalization fringe - latest finalized message for all bonded senders.
//  * @param closestChildren function returning closest children (lowest message for a sender that uses target as justification)
//  * @param sender sender of a message
//  */
final case class CasperProtocol[F[_]: Sync, M, S](
    curFringe: Map[S, M],
    closestChildren: M => F[List[M]],
    sender: M => S,
    seqNum: M => Long
) {
//
//  /** Senders that recognize sender of message as part of their partition. */
////  private def partition(m: M): F[Option[Set[S]]] =
////    (List(m), curFringe.keySet, 0).tailRecM {
////      case (lvl, partitionImplied, safetyN) =>
////        for {
////          nextLvl <- lvl.flatTraverse(closestChildren)
////        } yield {
////          val nextLvlSenders = nextLvl.map(sender).toSet
////          // if next level is smaller then current, drop safety counter to 0, start seeking for partition again.
////          val (nextSafetyN, nextPartitionImplied) =
////            if (partitionImplied == nextLvlSenders) (safetyN + 1, partitionImplied)
////            else (0, nextLvlSenders)
////          // when 3 levels contain exactly the same senders - declare partition
////          val done = nextSafetyN == 3
////          // once 3 levels found of the same senders found, this is partition.
////          if (done) partitionImplied.some.asRight[(List[M], Set[S], Int)]
////          // if not and no more children - declare absence of any partition
////          else if (nextLvl.isEmpty) none[Set[S]].asRight[(List[M], Set[S], Int)]
////          // else continue traversing
////          else
////            (nextLvl, nextPartitionImplied, nextSafetyN).asLeft[Option[Set[S]]]
////        }
////    }
//
//  /**
//    * Partition view of a message.
//    * @param view justifications if the message that define message's view.
//    * @param finalizationFringe current finalization fringe.
//    */
//  private def partition(view: Map[S, M], finalizationFringe: Map[S, M])(
//      parents: M => F[List[M]],
//      seqNum: M => Long,
//      sender: M => S
//  ): F[Option[Set[S]]] = {
//    val stoppers = finalizationFringe.mapValues(seqNum)
//    val start    = view.valuesIterator.toList
//    // how many layers should members of partition build on top of each other for partition to be declared
//    val safetyInterval = 3
//
//    (start, start.map(sender), safetyInterval).tailRecM {
//      case (lvl, partitionSenders, lvlsLeft) =>
//        val newLvlsLeft = lvlsLeft - 1
//        // once all 3 levels traversed - find which senders are still there on 3rd level
//        if (lvlsLeft == 0)
//          partitionSenders.toSet.some.asRight[(List[M], List[S], Int)].pure[F]
//        else {
//          val nextLvlF = lvl.flatTraverse(
//            parents(_).map(
//              // don't go under finalization fringe
//              _.filter(m => seqNum(m) > stoppers.getOrElse(sender(m), Long.MinValue))
//            )
//          )
//          nextLvlF.map { nextLvl =>
//            // if nothing to traverse but safety interval is not exhausted - no partition is found
//            if (nextLvl.isEmpty) none[Set[S]].asRight[(List[M], List[S], Int)]
//            else {
//              // if still there are parents to traverse and safety interval not exhausted - proceed
//              val newPartitionSenders = nextLvl.map(sender)
//              (nextLvl, newPartitionSenders, newLvlsLeft).asLeft[Option[Set[S]]]
//            }
//          }
//        }
//    }
//  }
////
////  /**
////    * Messages that can be finalized.
////    * @return list of sets of messages. Each set of messages belong to some partition, so conflict resolution should be
////    *         done inside partition first, and then amongst different partitions, if they are of the same level, or
////    *         between newly found set and already existing finalized set at the same level. .
////    */
////  def run: F[List[Set[M]]] = {
////    val curPartialFringe =
////      curFringe.iterator
////        .map { case (s, m) => (seqNum(m), (s, m)) }
////        .foldLeft(Map.empty[Long, Map[S, M]]) {
////          case (acc, (idx, (s, m))) => acc.updated(idx, acc.getOrElse(idx, Map()) + (s, m))
////        }
////    for {
////      fc <- fringeCandidate[F, M, S](curPartialFringe)(closestChildren, sender)
////      // fringe with None items filtered out
////      fringePresent = fc.collect { case (s, Some(v))               => (s, v) }
////      v             <- fringePresent.toList.traverse { case (s, m) => partition(m).map((s, _)) }
////      // partitions found
////      partitions = v.collect { case (_, Some(partition)) => partition.map(fringePresent) }
////    } yield partitions
////  }
//}
//
//object CasperProtocol {
////
////  /** Whether fringe is full fringe (goes through all senders) */
////  def isShardFringe[M, S](v: PartialFringe[M, S], bondedSenders: Set[S]): Boolean =
////    v.flatMap(_._2.keySet) == bondedSenders
////
////  final case class Justification[M, S](sender: S, message: M, reportedPartition: Set[S])
////
////  /**
////    * Candidate messages for advancement of the finalization fringe.
////    * @param curFringe current fringe
////    */
////  def fringeCandidate[F[_], M, S](
////      curFringe: PartialFringe[M, S],
////      latestMessagesLevels: Map[S, Long]
////  )(children: M => F[List[M]], sender: M => S)(implicit a: Sync[F]): F[PartialFringe[M, S]] =
////    curFringe.toList
////      .flatMap { case (lvlIdx, lvl) => lvl.map { case (sender, m) => (sender, (m, lvlIdx)) } }
////      // only messages that are in the view defined by latest messages are of interest
////      .filter { case (s, (_, lvlIdx)) => latestMessagesLevels.getOrElse(s, Long.MinValue) > lvlIdx }
////      .traverse {
////        case (s, (m, lvlIdx)) =>
////          val selfChildOptF = children(m).map(_.find(sender(_) == s))
////          selfChildOptF.map(_.map((lvlIdx + 1, s, _)))
////      }
////      .map(_.collect { case Some((lvlIdx, s, m)) => (lvlIdx, (s, m)) })
////      .map(_.toMap)
////
////  /**
////    * Whether it is safe to finalize and merge set of messages that are the next fringe advancement.
////    * @param nextFringe     the next messages to finalize
////    * @param latestMessages latest messages that report partitions detected by senders of the shard.
////    * @return
////    */
////  def safeToFinalize[M, S](
////      nextFringe: PartialFringe[M, S],
////      latestMessages: List[Justification[M, S]]
////  ): Boolean = {
////    val partitions = latestMessages.map(_.reportedPartition)
////    val agreed     = partitions.distinct.size == 1
////    agreed && {
////      // next fringe have to correspond to partition detected
////      assert(
////        partitions.head == nextFringe.flatMap(_._2.keySet),
////        "Fringe advancement does not correspond to partition."
////      )
////      true
////    }
////  }
}
