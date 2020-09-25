package coop.rchain.casper

import cats.Monad
import cats.effect.Sync
import cats.implicits._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.BlockDagRepresentation
import coop.rchain.casper.protocol.ProcessedDeploy
import coop.rchain.casper.protocol.Event
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.{DagOperations, EventConverter, ProtoUtil}
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockMetadata
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.trace.{COMM, Consume, Produce}
import coop.rchain.shared.Log

import scala.collection.BitSet

final case class EventGroup(produces: Set[Produce], consumes: Set[Consume], comms: Set[COMM])

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
      conflicts = conflictsBecauseOfJoins || containConflictingEvents(b1Events, b2Events)
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

  def isConflict(d1: ProcessedDeploy, d2: ProcessedDeploy): Boolean = {
    val eventGroup1 = extractEventGroup(d1.deployLog)
    val eventGroup2 = extractEventGroup(d2.deployLog)
    extractJoinedChannels(eventGroup1).intersect(allChannels(eventGroup2)).nonEmpty ||
    extractJoinedChannels(eventGroup2).intersect(allChannels(eventGroup1)).nonEmpty ||
    containConflictingEvents(eventGroup1, eventGroup2)
  }

  private[this] def containConflictingEvents(
      b1Events: EventGroup,
      b2Events: EventGroup
  ): Boolean = {
    def channelConflicts(
        b1Events: Set[TuplespaceEvent],
        b2Events: Set[TuplespaceEvent]
    ): Boolean =
      (for {
        b1  <- b1Events
        b2  <- b2Events
        res = b1.conflicts(b2)
        // TODO: fail fast
      } yield (res)).contains(true)

    val b1Ops = tuplespaceEventsPerChannel(b1Events)
    val b2Ops = tuplespaceEventsPerChannel(b2Events)
    val conflictPerChannel = b1Ops
      .map {
        case (channel, v) =>
          (channel, channelConflicts(v, b2Ops.getOrElse(channel, Set.empty)))
      }
    conflictPerChannel
      .filter { case (_, conflicts) => conflicts }
      .keys
      .nonEmpty
  }

  private[this] def isVolatile(comm: COMM, consumes: Set[Consume], produces: Set[Produce]) =
    !comm.consume.persistent && comm.peeks.isEmpty && consumes.contains(comm.consume) && comm.produces
      .forall(
        produce => !produce.persistent && produces.contains(produce)
      )

  private[this] def allChannels(events: EventGroup) =
    events.produces.map(_.channelsHash).toSet ++ events.consumes
      .flatMap(_.channelsHashes)
      .toSet ++ events.comms.flatMap { comm =>
      comm.consume.channelsHashes ++ comm.produces.map(_.channelsHash)
    }.toSet

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

  private[this] def extractEventGroup(events: List[Event]): EventGroup = {
    val rspaceEvents = events
      .map(EventConverter.toRspaceEvent)
      .toSet
    val allProduceEvents = rspaceEvents.collect { case p: Produce => p }
    val allConsumeEvents = rspaceEvents.collect { case c: Consume => c }
    val allCommEvents    = rspaceEvents.collect { case c: COMM    => c }
    val (volatileCommEvents, nonVolatileCommEvents) = allCommEvents
      .partition(isVolatile(_, allConsumeEvents, allProduceEvents))
    val producesInVolatileCommEvents = volatileCommEvents.flatMap(_.produces)
    val consumesInVolatileCommEvents = volatileCommEvents.map(_.consume)
    val produceEvents                = allProduceEvents.filterNot(producesInVolatileCommEvents.contains)
    val consumeEvents                = allConsumeEvents.filterNot(consumesInVolatileCommEvents.contains)
    EventGroup(produceEvents, consumeEvents, nonVolatileCommEvents)
  }

  private[this] def extractJoinedChannels(b: EventGroup): Set[Blake2b256Hash] = {

    def joinedChannels(consumes: Set[Consume]) =
      consumes.withFilter(Consume.hasJoins).flatMap(_.channelsHashes)

    joinedChannels(b.consumes) ++ joinedChannels(b.comms.map(_.consume))

  }

  private[this] def tuplespaceEventsPerChannel(
      b: EventGroup
  ): Map[Blake2b256Hash, Set[TuplespaceEvent]] = {
    val nonPersistentProducesInComms = b.comms.flatMap(_.produces).filterNot(_.persistent)
    val nonPersistentConsumesInComms = b.comms.map(_.consume).filterNot(_.persistent)
    val peekedProducesInCommsHashes =
      b.comms.withFilter(_.peeks.nonEmpty).flatMap(_.produces).map(_.hash)

    val freeProduces = b.produces
      .diff(nonPersistentProducesInComms)
      .filterNot(p => peekedProducesInCommsHashes.contains(p.hash) && !p.persistent)

    val freeConsumes = b.consumes.diff(nonPersistentConsumesInComms)

    val produceEvents = freeProduces.map(TuplespaceEvent.from(_))
    val consumeEvents = freeConsumes.flatMap(TuplespaceEvent.from(_))
    val commEvents    = b.comms.flatMap(TuplespaceEvent.from(_, b.consumes))

    (produceEvents ++ consumeEvents ++ commEvents)
      .groupBy(_._1)
      .mapValues[Set[TuplespaceEvent]](_.map(_._2))
  }

}
