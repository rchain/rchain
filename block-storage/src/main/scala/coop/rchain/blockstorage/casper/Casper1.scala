package coop.rchain.blockstorage.casper
import cats.Show
import cats.syntax.all._
import coop.rchain.shared.syntax._
import cats.effect.Sync
import cats.effect.concurrent.Ref
import coop.rchain.blockstorage.casper.DependencyGraph.zipStreamList
import fs2.Stream

object Casper1 {

//  /** Potential configuration of the fringe. */
//  final case class FringeOption[M, S](items: Map[S, Option[M]]) extends AnyVal {
//    // Fringe has to go across all senders. Either there is a message for sender, or a whole.
//    def isValid(bondsMap: Map[S, Long]): Boolean = items.keySet == bondsMap.keySet
//    // All messages of the fringe
//    def messages: Iterator[M]    = items.iterator.collect { case (_, Some(m)) => m }
//    def lateSenders: Iterator[S] = items.iterator.collect { case (s, None)    => s }
//
//    /** Fringe can be considered final if more more the 50% of the stake has been already merged.
//      * Final means state is irreversible. */
//    def isFinal(bondsMap: BondsMap[S]): Boolean =
//      items.keySet.map(bondsMap.v).sum * 2 > bondsMap.totalStake
//
//    /** Finge is complete when there is one message for each sender */
//    def isComplete(bondsMap: BondsMap[S]): Boolean =
//      items.keysIterator sameElements bondsMap.activeSenders
//  }
//
//  /** There are limited number of possible fringe configurations. */
//  type FringeIdx           = Long
//  type FringeOptions[M, S] = Map[FringeIdx, FringeOption[M, S]]
//
//  /** Messages that witness some configuration of the next fringe. */
//  type FringeWitnesses[M] = Map[FringeIdx, Set[M]]
//
//  /** Ancestors from each sender for a message. */
//  type AncestorsMap[M, S] = Map[M, Set[M]]
//
//  final case class ConflictSetState[M, S](
//      witnesses: Map[S, FringeWitnesses[M]],
//      // map for each message in conflict scope: connections with messages of the next fringe
//      pathsMap: Map[M, SeeMap[M, S]]
//  ) {
//    def superDescs = ???
//  }
//
//  /** Bonds map, representing*/
//  final case class Bonds[S](v: Map[S, Long]) extends AnyVal {
//    def totalStake: Long           = v.valuesIterator.sum
//    def activeSenders: Iterator[S] = v.filter { case (_, w) => w > 0 }.keysIterator
//    def checkSupermajority(senders: Set[S]) =
//      activeSenders.filter(senders.contains).map(v).sum * 3 > totalStake * 2
//  }
//
//  final case class SeeMap[M, S](v: Map[M, Set[S]]) {
//    // Message M is seen through supermaority
//    def isSuperDesc(m: M, bondsMap: BondsMap[S]) = v.getOrElse(m, Set()).map(bondsMap.v)
//  }
//
//  /**
//    * Traverse through closest children, accumulate:
//    * 1. View of each messae on finalization fringe
//    * 2. Cumulative paths to all messages in finalization fringe
//    * @param m
//    * @param dg
//    * @tparam F
//    * @tparam M
//    * @tparam S
//    * @return
//    */
////  // Closest (lowest) child for each sender.
////  def closestChildren[F[_,]](m: M): F[List[M]] =
////    dg.children(m)
////      .map(_.groupBy(dg.sender).map { case (_, msgs) => msgs.minBy(dg.seqNum) }.toList)
////
////  def late
//
//  def partition[F[_], M, S](m: M, dg: DependencyGraph[F, M, S]) = ???
//  final case class BondsMap[S](v: Map[S, Long]) extends AnyVal {
//    def totalStake: Long           = v.valuesIterator.sum
//    def activeSenders: Iterator[S] = v.filter { case (_, w) => w > 0 }.keysIterator
//    def checkSupermajority(senders: Set[S]) =
//      activeSenders.filter(senders.contains).map(v).sum * 3 > totalStake * 2
//  }
//  // Whether messages represent supermajority.
//  def isSupermajority[M, S](messages: Iterator[M], bm: BondsMap[S])(sender: M => S): Boolean =
//    messages.map(sender).map(bm.v).sum * 3 > bm.totalStake * 2
//

}
