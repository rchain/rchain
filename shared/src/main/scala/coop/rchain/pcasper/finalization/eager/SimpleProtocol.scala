package coop.rchain.pcasper.finalization.eager

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.pcasper.finalization.Fringe
import coop.rchain.shared.syntax._

/** Protocol to define safety inside a partition. */
object SimpleProtocol {

  /**
    * Path in the DAG representing a walk from some starting point to a sink message
    * @param sink Ending message
    * @param sendersSeen Senders seen on the path
    */
  final case class DagPath[M, S](sink: M, sendersSeen: Set[S])

  def isSafe[F[_]: Sync, M, S](
      target: Fringe[M, S],
      bondsMap: Map[S, Long],
      witnessesF: M => F[Map[S, M]],
      sender: M => S
  ): F[Boolean] = {

    require(
      target.keySet == bondsMap.keySet,
      "Simple protocol requires target fringe and bonds map having the same senders."
    )

    /** Witnesses that are inside partition. */
    def partitionWitnessesF: M => F[Map[S, M]] =
      witnessesF(_: M).map(_.filter { case (s, _) => bondsMap.contains(s) })

    /** Whether senders represent supermajority. */
    def isSupermajority(senders: Iterable[S]): Boolean =
      senders.nonEmpty && {
        val totalStake   = bondsMap.keysIterator.map(bondsMap).sum
        val sendersStake = senders.toIterator.map(bondsMap).sum
        sendersStake * 3 >= totalStake * 2
      }

    /** Whether supermajority seen the message. */
    def underSupermajority(m: M): F[Boolean] =
      partitionWitnessesF(m).map(wits => isSupermajority(wits.keySet))

    /** Whether message is a covering of keys in paths map. */
    def isCovering(message: M, pathsMap: Map[M, Set[S]]): F[Boolean] = {
      val seeSupermajorityWitnessing =
        Sync[F].delay(isSupermajority(pathsMap.getOrElse(message, Set())))
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
              children <- partitionWitnessesF(path.sink).map(_.valuesIterator.toList)
              // No need to visit the child if no new senders will be added to the view map for this child.
              next = children.filter { c =>
                (path.sendersSeen -- accPaths.getOrElse(c, Set())).nonEmpty
              }
            } yield next.map(DagPath(_, path.sendersSeen + sender(path.sink)))
          }

          val step = lvl.foldLeftM((accPaths, accSendersCovering)) {
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

          for {
            r                                    <- step
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

    // Each message in the target has to be covered by supermajority
    target.toList.existsM { case (_, m) => coveredBySupermajority(m).not }.not
  }
}
