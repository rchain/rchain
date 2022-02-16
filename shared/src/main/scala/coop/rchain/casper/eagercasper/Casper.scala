package coop.rchain.casper.eagercasper

import cats.Monad
import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.casper.eagercasper.safety.SimpleProtocol
import coop.rchain.shared.syntax._

/** Local Casper - finalize local views of messages. */
object Casper {

  /**
    * Partial fringe - map from sender to a message.
    * Represents fringe advancement, consisting of messages of the same height.
    */
  type Fringe[M, S] = Map[S, M]

  /**
    * Result of reconciliation of several [[LevelView]]
    * @param newView    new finality view
    * @param reconciled Partial fringes for which conflict resolution has to be run again.
    */
  final case class LevelReconciliation[M, S](
      newView: LevelView[M, S],
      reconciled: Vector[Fringe[M, S]]
  )

  /**
    * Result of reconciliation of several [[FinalityView]].
    * @param newView  new finality View
    * @param reconciled Reconciliations one on different levels.
    *                   This is an input for conflict resolver.
    *                   List is ordered down top according to block height, which means rejections computed
    *                   for element N have to be respected on level N + 1.
    */
  final case class FinalityReconciliation[M, S](
      newView: FinalityView[M, S],
      reconciled: Vector[LevelReconciliation[M, S]]
  )

  /**
    * Set of timely messages across target.
    * Timely message - message that do not have reference to other messages in the target set.
    */
  def timelySet[F[_]: Monad, M, S](
      target: List[M]
  )(justificationsF: M => F[List[M]]): F[List[M]] =
    target.filterA(m => justificationsF(m).map(_.exists(target.contains)).not)

  /** Candidate for fringe advancement. */
  def fringeCandidate[F[_]: Monad, M, S](base: Fringe[M, S])(
      witnessesF: M => F[Map[S, M]],
      justificationsF: M => F[List[M]],
      sender: M => S
  ): F[Fringe[M, S]] = {
    val selfChildrenF = base.toList
      .traverse { case (s, m) => witnessesF(m).map(_.find { case (ws, _) => ws == s }) }
      .map(_.collect { case Some((_, m)) => m })

    val resultF = selfChildrenF.flatMap(timelySet(_)(justificationsF)).flatMap { timely =>
      // fringe advancements in partitions size of less then 2 are not possible - in this case add witnesses
      if (timely.size == 1) {
        val single = timely.head
        witnessesF(single).map(_ + (sender(single) -> single))
      } else timely.map(m => sender(m) -> m).toMap.pure[F]
    }

    resultF.map(r => (r.size != 1).guard[Option].as(r).getOrElse(Map()))
  }

  /** Find finalization fringe advancement. */
  def finalize[F[_]: Sync, M, S](base: Fringe[M, S], fullBonds: Map[S, Long])(
      witnessesF: M => F[Map[S, M]],
      justificationsF: M => F[Map[S, M]],
      sender: M => S
  ): F[Option[Fringe[M, S]]] = {
    val jsListF = justificationsF(_: M).map(_.valuesIterator.toList)
    fringeCandidate(base)(witnessesF, jsListF, sender)
      .flatMap { partitionFringe =>
        if (partitionFringe.isEmpty) none[Fringe[M, S]].pure[F]
        else {
          val partitionBondsMap = fullBonds.filter {
            case (s, _) => partitionFringe.keySet.contains(s)
          }

          // Option 1. Non overlapping partitions
//          val algo1 = {
//            val isSafe = {
//              val partitionSenders = partitionBondsMap.keySet
//              val partitionWits =
//                witnessesF(_: M).map(_.filter { case (s, _) => partitionSenders.contains(s) })
//              partitionFringe.toList.existsM {
//                case (_, m) =>
//                  SafetyOracle
//                    .findPartition[F, M, S](m)(
//                      partitionWits,
//                      justificationsF(_).map(_.map(m => sender(m) -> m).toMap)
//                    )(sender)
//                    .map(_.isDefined)
//              }
//            }
//            isSafe.map(_.guard[Option].as(partitionFringe))
//          }

          // Option 2. Allow overlapping partitions
          val algo2 = SimpleProtocol
            .isSafe(partitionFringe, partitionBondsMap, witnessesF, sender)
            .map(_.guard[Option].as(partitionFringe))

          algo2
        }
      }
  }
}
