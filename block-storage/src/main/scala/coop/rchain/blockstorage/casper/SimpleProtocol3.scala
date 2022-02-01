package coop.rchain.blockstorage.casper

import cats.effect.Concurrent
import cats.syntax.all._
import coop.rchain.shared.syntax._

/** Detector of a partition. */
object SimpleProtocol3 {

  /**
    * Path in the DAG representing a walk from some starting point to a sink message
    * @param sink Ending message
    * @param sendersSeen Senders seen on the path
    */
  final case class DagPath[M, S](sink: M, sendersSeen: Set[S])

  def run[F[_]: Concurrent, M, S](
      currentFringe: Map[S, M],
      bondsMap: Map[S, Long],
      childrenF: M => F[List[M]],
      sender: M => S
  ): F[Map[S, M]] = {

    // Total stake bonded
    val totalStake = bondsMap.valuesIterator.sum

    /** Whether senders represent supermajority. */
    def isSupermajority(senders: Iterable[S]): Boolean =
      senders.nonEmpty &&
        // toIterator here to handle the case of equal stakes
        senders.toIterator.map(bondsMap).sum * 3 > totalStake * 2

    /** Whether supermajority seen the message. */
    def underSupermajority(m: M): F[Boolean] =
      childrenF(m).map(ch => isSupermajority(ch.map(sender)))

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
              children <- childrenF(path.sink)
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

    for {
      // find next messages after current fringe from all senders
      nextLvl <- currentFringe.toList.traverse {
                  case (s, m) => childrenF(m).map(_.find(sender(_) == s)).map(v => (s, v))
                }

      // Each item in the fringe has to have coverings from supermajority.
      // In other words, for each message in the fringe supermajority has to witness that
      // supermajority accepted the message. And these witnesses have to be under supermajority.
      r <- nextLvl
            .collect { case (s, Some(m)) => (s, m) }
            .filterA { case (_, m) => coveredBySupermajority(m).not }
    } yield r.toMap
  }
}
