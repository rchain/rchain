package coop.rchain.blockstorage.casper

import cats.effect.Concurrent
import cats.effect.concurrent.Deferred
import cats.syntax.all._
import coop.rchain.shared.syntax._
import fs2.Stream

object SimpleProtocol2 {

//  final case class ConflictSetState[M, S](
//      // Each item in the fringe has to have coverings from supermajority.
//      // In other words, for each message in the fringe supermajority has to witness that
//      // supermajority accepted the message. And these witnesses have to be under supermajority.
//      viewsMap: Map[M, Map[M, Set[S]]]
//  )
//
//  /**
//    * Whether it is safe to finalize the next fringe candidate.
//    * @param fringeCandidate Candidate for finalization.
//    * @param bondsMap        Bonds map of a partition.
//    * @param closestChildren The lowest children. Message can be used as justification by many children,
//    *                        only the first child per sender is needed.
//    * @param sender          Sender of a message.
//    */
//  def run[F[_], M, S](
//      fringeCandidate: Map[S, M],
//      bondsMap: Map[S, Long],
//      closestChildren: M => F[List[M]],
//      sender: M => S
//  )(implicit c: Concurrent[F]): F[Boolean] = {
//    require(
//      bondsMap.keySet == fringeCandidate.keySet,
//      "Simple Casper protocol requires target fringe candidate to have one block for each sender in the the bonds map. "
//    )
//
//    // Total stake bonded
//    val totalStake = bondsMap.valuesIterator.sum
//
//    /** Whether senders represent supermajority. */
//    def isSupermajority(senders: Iterable[S]): Boolean =
//      senders.nonEmpty &&
//        // toIterator here to handle the case of equal stakes
//        senders.toIterator.map(bondsMap).sum * 3 > totalStake * 2
//
//    /** Whether supermajority seen the message. */
//    def underSupermajority(m: M): F[Boolean] =
//      closestChildren(m).map(ch => isSupermajority(ch.map(sender)))
//
//    /** Whether message is a covering. */
//    def isCovering(message: M, pathsMap: Map[M, Set[S]]): F[Boolean] = {
//      val seeSupermajorityWitnessing =
//        Concurrent[F].delay(isSupermajority(pathsMap.getOrElse(message, Set())))
//      seeSupermajorityWitnessing &&^ underSupermajority(message)
//    }
//
//    // Init views of each fringe item
//    val initViews = fringeCandidate.iterator.map { case (s, m) => Views(m, Map(m -> Set(s))) }.toList
//    // Accumulator of paths from message to descendants
//    val initAcc = initViews
//    // Accumulator of senders that cover the message
//    val initSendersCovering = Map.empty[M, Set[S]]
//    (initAcc, initSendersCovering, initViews).tailRecM {
//      case (accPaths, accSendersCovering, lvl) =>
//        val nextLvlF = lvl.flatTraverse { path =>
//          for {
//            children <- closestChildren(path.sink)
//            // No need to visit the child if no new senders will be added to the view map for this child.
//            next = children.filter { c =>
//              (path.sendersSeen -- accPaths.getOrElse(c, Set())).nonEmpty
//            }
//          } yield next.map(DagPath(_, path.sendersSeen + sender(path.sink)))
//        }
//
//        for {
//          r <- lvl.foldLeftM((accPaths, accSendersCovering)) {
//                case ((paths, sendersCovering), DagPath(sink, senderSeen)) =>
//                  val s = sender(sink)
//                  // Add senders on current path to senders accumulated already
//                  val newPaths = paths.updated(sink, paths.getOrElse(sink, Set()) ++ senderSeen)
//                  val shouldAddCovering = (!sendersCovering.contains(s)).pure[F] &&^
//                    isCovering(sink, newPaths)
//                  // If sink is a covering - add sender to coverings accumulator
//                  val newSendersCoveringF = shouldAddCovering.map {
//                    if (_) sendersCovering + s else sendersCovering
//                  }
//                  newSendersCoveringF.map((newPaths, _))
//              }
//          (newAccPaths, newAccSendersCovering) = r
//          done                                 = isSupermajority(newAccSendersCovering)
//          r <- if (done) true.asRight[(Map[M, Set[S]], Set[S], List[DagPath[M, S]])].pure[F]
//              else
//                nextLvlF.map {
//                  case Nil  => false.asRight[(Map[M, Set[S]], Set[S], List[DagPath[M, S]])]
//                  case next => (newAccPaths, newAccSendersCovering, next).asLeft[Boolean]
//                }
//        } yield r
//    }
//  }

}
