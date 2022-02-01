package coop.rchain.blockstorage.casper
import cats.effect.Sync
import cats.syntax.all._
import cats.effect.concurrent.Ref
import coop.rchain.blockstorage.casper.DependencyGraph.zipStreamList
import coop.rchain.shared.syntax._
import fs2.Stream

import scala.collection.mutable

object SimpleProtocol {

  /**
    * Path in the DAG.
    * @param source Starting message
    * @param sink Ending message
    * @param sendersSeen Senders seen on the path
    */
  final case class DagPath[M, S](source: M, sink: M, sendersSeen: Set[S])

  /** Whether it is safe to finalize the next fringe candidate. */
  def run[F[_], M, S](
      fringeCandidate: Map[S, M],
      bondsMap: Map[S, Long],
      dg: DependencyGraph[F, M, S]
  )(implicit a: Sync[F]): F[Boolean] = {
    require(
      bondsMap.keySet == fringeCandidate.keySet,
      "Simple Casper protocol requires target fringe candidate to have one block for each sender in the the bonds map. "
    )

    // Total stake bonded
    val totalStake = bondsMap.valuesIterator.sum

    // Whether set of messages represent supermajority.
    def isSupermajority(messages: Set[M]): Boolean =
      messages.nonEmpty &&
        // toIterator here to handle the case of equal stakes
        messages.map(dg.sender).toIterator.map(bondsMap).sum * 3 > totalStake * 2

    // Whether supermajority agreed on a message.
    def underSupermajority(m: M): F[Boolean] = dg.children(m).map(_.toSet).map(isSupermajority)

    // Paths stream starting from the message.
    def pathsStream(
        fringeMessage: M,
        alreadySeen: M => F[Set[S]]
    ): Stream[F, DagPath[M, S]] = {

      /**
        * Relation for DAG traversal. Traversal is expressed in terms of [[DagPath]]'s. Each path
        * unfolds to List of children paths.
        * @param path path for which the next level should be computed
        * @param visited set of senders which has been already met on all paths from fringeMessage to target
        * @return
        */
      def traversalRelation(path: DagPath[M, S]): F[List[DagPath[M, S]]] =
        for {
          children <- dg.children(path.sink)
          // No need to visit message if no new senders will be added to the cumulative path as a result.
          // Without this filter complexity increases dramatically on rather small conflict sets (50x speedup were observed)
          next <- children.filterA { ch =>
                   alreadySeen(ch).map(v => (path.sendersSeen -- v).nonEmpty)
                 }
        } yield next.map { nextSink =>
          path.copy(sink = nextSink, sendersSeen = path.sendersSeen + dg.sender(path.sink))
        }

      // Start from path starting and ending at fringeMessage and seeing sender of fringeMessage
      val initPaths = List(DagPath(fringeMessage, fringeMessage, Set(dg.sender(fringeMessage))))
      Stream
        .unfoldLoopEval(initPaths) { lvl =>
          lvl
            .flatTraverse(traversalRelation)
            .map(next => (next, next.nonEmpty.guard[Option].as(next)))
        }
        .flatMap(Stream.emits)
    }

    Stream
      .eval(Ref.of(Map.empty[M, Map[M, Set[S]]]))
      .flatMap { seeMapRef =>
        def alreadySeenF(source: M, sink: M): F[Set[S]] =
          seeMapRef.get.map(_.getOrElse(sink, Map()).getOrElse(source, Set()))
        // Zip streams to preserve notion of distance.
        zipStreamList(
          fringeCandidate.valuesIterator.map(m => pathsStream(m, alreadySeenF(m, _))).toList
        )
        // Accumulate map descendant -> (fringe item, senders see through all paths)
          .evalMap {
            case DagPath(fringeM, sink, sendersSeen) =>
              seeMapRef
                .modify { s =>
                  val curV = s.getOrElse(sink, Map())
                  val newV = curV.updated(fringeM, curV.getOrElse(fringeM, Set()) ++ sendersSeen)
                  (s.updated(sink, newV), newV)
                }
                // output only current sink which is a potential covering
                .map(sink -> _)
          }
      }
      // Accumulate coverings under supermajority. Map covering -> base.
      .evalScan(Map.empty[S, (M, Set[M])]) {
        case (coveringsAcc, (witness, pathMap)) =>
          val sWitness = dg.sender(witness)
          // Fringe items that are strongly seen by witness (all paths cross supermajority of senders).
          val base = pathMap.filter {
            case (_, stakeSeen) => stakeSeen.toList.map(bondsMap).sum * 3 > totalStake * 2
          }.keySet

          // Whether at least one fringe item is covered.
          val isCovering = Sync[F].delay(base.nonEmpty)

          val doRecord = isCovering &&^ underSupermajority(witness)

          doRecord.map(if (_) coveringsAcc + (sWitness -> (witness -> base)) else coveringsAcc)
      }
//      .evalTap { v =>
//        coop.rchain.shared.Log.log.info(s"${v.map { case (c, base) => (c.show, base._2.size) }}")
//      }
      // find whether coverings under supermajority cover the whole target fringe
      .exists(_.values.flatMap(_._2).toSet == fringeCandidate.valuesIterator.toSet)
      .compile
      .lastOrError
  }
}
