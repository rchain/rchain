package coop.rchain.blockstorage.casper
import cats.Show
import cats.data.OptionT
import cats.effect.Sync
import cats.effect.concurrent.Ref
import coop.rchain.shared.syntax._
import cats.syntax.all._
import coop.rchain.blockstorage.casper.DependencyGraph.{zipStreamList, LatestMessages}
import coop.rchain.models.BlockMetadata
import fs2.{Chunk, Stream}
import syntax.all._

trait Casper[F[_], M, S] {

  /**
    * Threshold for finalization.
    */
  def faultToleranceThreshold: Float

  /** Constraints Casper scope to limit complexity. */
  val maxDepth: Long
}

object Casper {

  /** Fringe candidate has multiple next messages for some senders.*/
  type FringeCandidate[M, S] = Map[S, Option[M]]

  final case class DagPath[M, S](source: M, sink: M, sendersSeen: Set[S])

  /** Next messages to be finalized. */
  def nextFinalizationRound[F[_], M, S](
      fringeCandidate: Map[S, M],
      dg: DependencyGraph[F, M, S],
      bondsMap: Map[S, Long]
  )(implicit a: Sync[F], ordering: Ordering[M]): F[Option[Set[M]]] = {
    val totalStake = bondsMap.valuesIterator.sum

//    implicit val log = coop.rchain.shared.Log

    // Closest (lowest) child for each sender. Should be sorted.
    def closestChildren(m: M): F[List[M]] =
      dg.children(m)
        .map(_.groupBy(dg.sender).map { case (_, msgs) => msgs.minBy(dg.seqNum) }.toList)
        .map(_.sorted)

    // Whether supermajority agreed on a message.
    def underSupermajority(m: M): F[Boolean] =
      dg.children(m)
        .map { children =>
          children
            .groupBy(dg.sender)
            .map { case (sender, _) => bondsMap(sender) }
            .sum * 3 > totalStake * 2
        }

    // Whether messages represent supermajority.
    def isSupermajority(messages: Iterator[M]): Boolean =
      messages.map(dg.sender).map(bondsMap).sum * 3 > bondsMap.values.sum * 2

    /** Paths stream from message */
    def pathsStream(
        fringeMessage: M
    ): Stream[F, DagPath[M, S]] = {
      // Traversal relation. No reason to traverse through blocks that have been met already via other paths.
      val traversalRelation =
        (path: DagPath[M, S], visited: Map[M, Set[S]]) =>
          closestChildren(path.sink)
            .map { js =>
              js.filter { j =>
                // no need to visit message if no new senders added to the cumulative path
                (path.sendersSeen -- visited.getOrElse(j, Set())).nonEmpty
              }
            }

      val initPaths = List(DagPath(fringeMessage, fringeMessage, Set(dg.sender(fringeMessage))))
      Stream.eval(Ref.of(Map.empty[M, Set[S]])).flatMap { visitCacheRef =>
        Stream
          .unfoldLoopEval(initPaths) { lvl =>
            lvl
              .flatTraverse { p =>
                val m       = p.sink
                val mSender = dg.sender(m)
                // update visit cache
                visitCacheRef
                  .updateAndGet(s => s.updated(m, s.getOrElse(m, Set()) + dg.sender(p.sink)))
                  .flatMap { visitCache =>
                    // proceed to next level
                    traversalRelation(p, visitCache).map { nextLvl =>
                      nextLvl.map { nextSink =>
                        p.copy(sink = nextSink, sendersSeen = p.sendersSeen + mSender)
                      }
                    }
                  }
              }
              .map(next => (next, next.nonEmpty.guard[Option].as(next)))
          }
          .flatMap(Stream.emits)
      }
    }

    val paths: List[Stream[F, DagPath[M, S]]] = fringeCandidate.map {
      case (_, fringeMessage) => pathsStream(fringeMessage)
    }.toList

    val initSeeMap = Map.empty[M, Map[M, Set[S]]]
    zipStreamList(paths)
      .mapAccumulate(initSeeMap) {
        case (acc, DagPath(fringeM, sink, sendersSeen)) =>
          val curV = acc.getOrElse(sink, Map())
          val newV = curV.updated(fringeM, curV.getOrElse(fringeM, Set()) ++ sendersSeen)
          (acc.updated(sink, newV), sink -> newV)
      }
      // accumulate lowest coverings. Map message -> covering base. Base have to be no less then supermajority.
      .evalScan(Map.empty[M, Set[M]]) {
        case (acc, (_, (witness, pathMap))) =>
          // fringe items that are strongly seen by witness
          val base = pathMap.filter {
            case (_, stakeSeen) => stakeSeen.toList.map(bondsMap).sum * 3 > totalStake * 2
          }.keySet
          // coverings that strongly see supermajority of fringe candidates are of interest
          val isCovering = Sync[F].delay(isSupermajority(base.iterator))
          // if the same base is covered and message from the same sender - do not update covering record
          val duplicate = Sync[F].delay {
            acc.exists {
              case (curW, curBase) => dg.sender(curW) == dg.sender(witness) && curBase == base
            }
          }
          val doRecord = duplicate.not &&^ isCovering &&^ underSupermajority(witness)
          doRecord.map(if (_) acc + (witness -> base) else acc)
      }
//      .evalTap(v => log.log.info(s"${v.keySet.map(_.show).mkString(";")}"))
      // we need supermajority of safe coverings seeing the same base
      .map { coverings =>
        coverings
          .groupBy { case (_, base) => base }
          .find {
            case (_, sameBaseCoverings) => isSupermajority(sameBaseCoverings.keysIterator)
          }
          .map { case (_, v) => v.head._2 }
      }
      .unNone
      .head
      .compile
      .last
  }

//  /** Find covering base for a message. */
//  def coveringBase[F[_], M, S](
//      message: M,
//      targetFringe: Map[S, M],
//      curFringe: Map[S, M],
//      dg: DependencyGraph[F, M, S],
//      bondsMap: Map[S, Long],
//      computedCoveringsMap: mutable.TreeMap[S, Set[M]]
//  )(implicit a: Sync[F], ordering: Ordering[M]): F[Set[M]] = {
//    val fringeMsgs = targetFringe.values.toSet
//    val totalStake = bondsMap.valuesIterator.sum
//
//    // when covering is already found by sender of message in some ancestor
//    // this is not just an optimisation, but also a logic requirement. For computing the base for covering
//    // issued by a sender - the lowest covering has to be provided.
//    val coveringFromSender: OptionT[F, Set[M]] = OptionT.fromOption(
//      computedCoveringsMap.get(dg.sender(message))
//    )
//
//    // when self parent is not a covering - compute
//    val compute = {
//      // message see some another through set of senders on all paths
//      val visited = mutable.TreeMap.empty[M, Set[S]]
//      // Traversal relation. No reason to traverse through blocks that have been met already via other paths.
//      val traversalRelation = (path: DagPath[M, S]) =>
//        dg.parents(path.sink)
//          .map { js =>
//            js.filter { j =>
//              val curSendersSeen = visited.getOrElse(j, Set())
//              // no need to visit message if no new senders added to the cumulative path
//              (path.sendersSeen -- curSendersSeen).nonEmpty &&
//              dg.seqNum(j) > dg.seqNum(curFringe(dg.sender(j)))
//            }
//          }
//      Stream
//        .unfoldLoopEval(List(DagPath(message, message, Set(dg.sender(message))))) { lvl =>
//          lvl
//            .flatTraverse { p =>
//              traversalRelation(p).map { nextLvl =>
//                nextLvl.map { m =>
//                  visited.update(m, visited.getOrElse(m, Set()) + dg.sender(p.sink))
//                  p.copy(sink = m, sendersSeen = p.sendersSeen + dg.sender(p.sink))
//                }
//              }
//            }
//            .map(next => (next, next.nonEmpty.guard[Option].as(next)))
//        }
//        .flatMap(Stream.emits)
//        // this outputs paths from message to messages in a fringe
//        .filter(path => fringeMsgs.contains(path.sink))
//        .scan(Map.empty[M, Long]) {
//          case (acc, DagPath(_, fm, stakeSeen)) =>
//            acc.updated(fm, acc.getOrElse(fm, 0L) + stakeSeen.map(bondsMap).sum)
//        }
//        .filter { pathsStakeMap =>
//          val supermajoritiesWitnessed = pathsStakeMap.toIterator
//            .filter { case (_, stake) => stake * 3 > totalStake * 2 }
//            .map { case (m, _) => m }
//          val coveringBaseStake =
//            supermajoritiesWitnessed.map { smm =>
//              targetFringe.filter { case (_, m) => m == smm }.keysIterator.map(bondsMap).sum
//            }.sum
//          coveringBaseStake * 3 > totalStake * 2
//        }
//        .flatMap(v => Stream.emits(v.keysIterator.toList))
//    }
//
//    coveringFromSender.getOrElseF(compute.compile.to(Set))
//  }

  /** Highest finalized messages from all known senders. Defines finalized state of the network. */
  type FinalizationFringe[S, M] = LatestMessages[S, M]

  /** All messages above the finalization fringe. */
  final case class ConflictScope[M](v: Set[M])

  /**
    * Scope of the message.
    * @param finalizationFringe  [[FinalizationFringe]] that represents finalized state of the network.
    *                            Fringe can be merged or used as a key to get access to actual state.
    * @param conflictScope       [[ConflictScope]]
    * @tparam M                  Type of the message.
    */
  final case class MessageScope[S, M](
      finalizationFringe: FinalizationFringe[S, M],
      conflictScope: ConflictScope[M]
  )

  /**
    * Can be thrown in two cases:
    * 1. Genesis ceremony is in progress.
    *   Validators are producing blocks on top of the genesis, but there are not enough blocks exchanged in the network,
    *   so no finalization fringe can be found. In this case max complexity of safety oracle puts a constraint on
    *   length of genesis ceremony. If genesis is out of the view of latest messages and no finalization fringe found,
    *   this is a reason to declare genesis ceremony failed.
    * 2. Silent validator. Safety oracle complexity puts a constraint on how long Casper can tolerate validator not
    *   producing blocks. If validator is silent for long enough - at some point his latest message becomes out of the
    *   view of other latest messages, therefore cannot be finalized. This means finalization fringe cannot be completed.
    *   This might cause a network halt, or some reaction to such silent validator.
    *
    * For now, as in general traversing through justifications should not be heavy task, and can be cached,
    * it is suggested to just relax maxDepth, up tp Long.MaxValue.
    */
  val noFringeMsg = "Unable to find Finalization Fringe."
  case object NoFinalizationFringe extends Exception(noFringeMsg)

  /**
    * Thrown when conflict scope of the message is empty.
    */
  val emptyConflictScopeMsg = "Conflict scope is empty."
  case object EmptyConflictScope extends Exception(emptyConflictScopeMsg)
}
