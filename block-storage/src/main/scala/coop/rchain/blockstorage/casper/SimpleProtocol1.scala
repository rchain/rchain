package coop.rchain.blockstorage.casper

import cats.effect.Concurrent
import cats.syntax.all._
import coop.rchain.shared.syntax._

object SimpleProtocol1 {

  /**
    * Path in the DAG representing a walk from some starting point to a sink message
    * @param sink Ending message
    * @param sendersSeen Senders seen on the path
    */
  final case class DagPath[M, S](sink: M, sendersSeen: Set[S])

  /**
    * Whether it is safe to finalize the next fringe candidate.
    * @param fringeCandidate Candidate for finalization.
    * @param bondsMap        Bonds map of a partition.
    * @param closestChildren The lowest children. Message can be used as justification by many children,
    *                        only the first child per sender is needed.
    * @param sender          Sender of a message.
    */
  def run[F[_], M, S](
      fringeCandidate: Map[S, M],
      bondsMap: Map[S, Long],
      closestChildren: M => F[List[M]],
      sender: M => S
  )(implicit c: Concurrent[F]): F[Boolean] = {
    require(
      bondsMap.keySet == fringeCandidate.keySet,
      "Simple Casper protocol requires target fringe candidate to have one block for each sender in the the bonds map. "
    )

    // Total stake bonded
    val totalStake = bondsMap.valuesIterator.sum

    /** Whether senders represent supermajority. */
    def isSupermajority(senders: Iterable[S]): Boolean =
      senders.nonEmpty &&
        // toIterator here to handle the case of equal stakes
        senders.toIterator.map(bondsMap).sum * 3 > totalStake * 2

    /** Whether supermajority seen the message. */
    def underSupermajority(m: M): F[Boolean] =
      closestChildren(m).map(ch => isSupermajority(ch.map(sender)))

    /** Whether message is a covering. */
    def isCovering(message: M, pathsMap: Map[M, Set[S]]): F[Boolean] = {
      val seeSupermajorityWitnessing =
        Concurrent[F].delay(isSupermajority(pathsMap.getOrElse(message, Set())))
      seeSupermajorityWitnessing &&^ underSupermajority(message)
    }

    /** Whether coverings of the message are supermajority. */
    def coveredBySupermajority(message: M): F[Boolean] = {
      val initPaths           = List(DagPath(message, Set(sender(message))))
      val initAcc             = Map.empty[M, Set[S]] // Accumulator of paths from message to descendants
      val initSendersCovering = Set.empty[S] // Accumulator of senders that cover the message
      (initAcc, initSendersCovering, initPaths).tailRecM {
        case (accPaths, accSendersCovering, lvl) =>
          val nextLvlF = lvl.flatTraverse { path =>
            for {
              children <- closestChildren(path.sink)
              // No need to visit the child if no new senders will be added to the view map for this child.
              next = children.filter { c =>
                (path.sendersSeen -- accPaths.getOrElse(c, Set())).nonEmpty
              }
            } yield next.map(DagPath(_, path.sendersSeen + sender(path.sink)))
          }

          for {
            r <- lvl.foldLeftM((accPaths, accSendersCovering)) {
                  case ((paths, sendersCovering), DagPath(sink, senderSeen)) =>
                    val s = sender(sink)
                    // Add senders on current path to senders accumulated already
                    val newPaths = paths.updated(sink, paths.getOrElse(sink, Set()) ++ senderSeen)
                    val shouldAddCovering = (!sendersCovering.contains(s)).pure[F] &&^
                      isCovering(sink, newPaths)
                    // If sink is a covering - add sender to coverings accumulator
                    val newSendersCoveringF = shouldAddCovering.map {
                      if (_) sendersCovering + s else sendersCovering
                    }
                    newSendersCoveringF.map((newPaths, _))
                }
            (newAccPaths, newAccSendersCovering) = r
            done                                 = isSupermajority(newAccSendersCovering)
            r <- if (done) true.asRight[(Map[M, Set[S]], Set[S], List[DagPath[M, S]])].pure[F]
                else
                  nextLvlF.map {
                    case Nil  => false.asRight[(Map[M, Set[S]], Set[S], List[DagPath[M, S]])]
                    case next => (newAccPaths, newAccSendersCovering, next).asLeft[Boolean]
                  }
          } yield r
      }
    }

    // Each item in the fringe has to have coverings from supermajority.
    // In other words, for each message in the fringe supermajority has to witness that
    // supermajority accepted the message. And these witnesses have to be under supermajority.
    fringeCandidate.valuesIterator.toList.existsM(coveredBySupermajority(_).not).not
  }

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
}
