package coop.rchain.casper

import java.util.concurrent.ConcurrentHashMap

import cats.effect.concurrent.Ref
import cats.effect.{Concurrent, Sync}
import cats.implicits._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.BlockDagRepresentation
import coop.rchain.casper.protocol.ProcessedDeploy
import coop.rchain.casper.protocol.Event
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.{DagOperations, EventConverter}
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockMetadata
import coop.rchain.rspace.history.HistoryRepository
import coop.rchain.rspace.syntax._
import coop.rchain.rspace.{Blake2b256Hash, HistoryReader}
import coop.rchain.rspace.trace.{COMM, Consume, EventGroup, Produce, Event => RSpaceEvent}
import coop.rchain.rspace.trace.Event.{
  allChannels,
  containConflictingEvents,
  extractJoinedChannels,
  extractRSpaceEventGroup,
  Conflict,
  NonConflict
}
import coop.rchain.shared.{Log, Serialize}

import scala.collection.concurrent.TrieMap
import scala.collection.{mutable, BitSet}
import scala.collection.immutable.Set

object EstimatorHelper {

  private[this] val ChooseNonConflictingMetricsSource =
    Metrics.Source(CasperMetricsSource, "choose-non-conflicting")

  def chooseNonConflicting[F[_]: Sync: Log: BlockStore](
      blockHashes: Seq[BlockHash],
      dag: BlockDagRepresentation[F]
  )(implicit spanF: Span[F]): F[Seq[BlockMetadata]] =
    spanF.trace(ChooseNonConflictingMetricsSource) {
      def nonConflicting(b: BlockMetadata): BlockMetadata => F[Boolean] =
        conflicts[F](_, b, dag).map(b => !b)

      for {
        blocks <- blockHashes.toList.traverse(dag.lookupUnsafe)
        result <- blocks
                   .foldM(List.empty[BlockMetadata]) {
                     case (acc, b) =>
                       acc
                         .forallM(nonConflicting(b))
                         .ifM(
                           (b :: acc).pure[F],
                           acc.pure[F]
                         )
                   }
                   .map(_.reverse)
      } yield result
    }

  private[casper] def conflicts[F[_]: Sync: Log: BlockStore](
      b1: BlockMetadata,
      b2: BlockMetadata,
      dag: BlockDagRepresentation[F]
  ): F[Boolean] =
    for {
      uncommonAncestorsMap             <- DagOperations.uncommonAncestors[F](Vector(b1, b2), dag)
      (b1AncestorsMap, b2AncestorsMap) = uncommonAncestorsMap.partition(_._2 == BitSet(0))
      b1Events                         <- extractBlockEvents[F](b1AncestorsMap.keys.toList)
      b2Events                         <- extractBlockEvents[F](b2AncestorsMap.keys.toList)
      conflictsBecauseOfJoins = extractJoinedChannels(b1Events)
        .intersect(allChannels(b2Events))
        .nonEmpty || extractJoinedChannels(b2Events).intersect(allChannels(b1Events)).nonEmpty
      conflicts = conflictsBecauseOfJoins || containConflictingEvents(b1Events, b2Events).nonEmpty
      _ <- if (conflicts) {
            Log[F].info(
              s"Blocks ${PrettyPrinter.buildString(b1.blockHash)} and ${PrettyPrinter
                .buildString(b2.blockHash)} conflict."
            )
          } else {
            Log[F].info(
              s"Blocks ${PrettyPrinter
                .buildString(b1.blockHash)} and ${PrettyPrinter
                .buildString(b2.blockHash)} don't conflict."
            )
          }
    } yield conflicts

//  final case class MergeChanges(
//      validDeploys: List[ProcessedDeploy],
//      rejectedDeploys: List[ProcessedDeploy],
//      validEventLogs: Seq[RSpaceEvent]
//  )

  private[this] def extractBlockEvents[F[_]: Sync: BlockStore](
      blockAncestorsMeta: List[BlockMetadata]
  ): F[EventGroup] =
    for {
      ancestors <- blockAncestorsMeta.traverse(
                    blockAncestorMeta => BlockStore[F].getUnsafe(blockAncestorMeta.blockHash)
                  )
      events = ancestors
        .flatMap(_.body.deploys.flatMap(_.deployLog))
      eventGroup = extractEventGroup(events)
    } yield eventGroup

  /**
    * @return EventGroup contains all the events which is not volatile in the scope
    */
  private[this] def extractEventGroup(events: List[Event]): EventGroup = {
    val rspaceEvents = events
      .map(EventConverter.toRspaceEvent)
    extractRSpaceEventGroup(rspaceEvents)
  }

}
