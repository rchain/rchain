package coop.rchain.casper

import cats.effect.Sync
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
import coop.rchain.rspace.{Blake2b256Hash, StableHashProvider}
import coop.rchain.rspace.trace.{COMM, Consume, EventGroup, Produce}
import coop.rchain.rspace.trace.{Event => RSpaceEvent}
import coop.rchain.shared.{Log, Serialize}

import scala.collection.BitSet

/**
  * `Scope` is widely used in descriptions in this file. To better understand `scope` here. I'd like to present
  * some examples to demonstrate that.
  *
  * B2  Rho("for (_ <<- @1) { 0 }")
  * |
  * B1  Rho("@1!(0)")
  *
  * I'd like to use `Volatile` event here to demonstrate the `scope` here.
  *
  * Within the scope which Scope(B2, B3)
  *
  * B2  Rho("for (_ <<- @1) { 0 }") ----+
  * |                                   |---Scope(B2, B1)
  * B1  Rho("@1!(0)")               ----+
  *
  * If you look at within the scope of Scope(B2, B1) like above, all the comm events happened in B1 and B2 are Volatile.
  *
  *
  * B2  Rho("for (_ <<- @1) { 0 }") ---- Scope(B2)
  * |
  * B1  Rho("@1!(0)")
  *
  * But if you look at within only the scope os Scope(B2) like above, the comm and consume events in the B2
  * are not Volatile.
  */
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

  def isConflict(d1: ProcessedDeploy, d2: ProcessedDeploy): Boolean = {
    val eventGroup1 = extractEventGroup(d1.deployLog)
    val eventGroup2 = extractEventGroup(d2.deployLog)
    extractJoinedChannels(eventGroup1).intersect(allChannels(eventGroup2)).nonEmpty ||
    extractJoinedChannels(eventGroup2).intersect(allChannels(eventGroup1)).nonEmpty ||
    containConflictingEvents(eventGroup1, eventGroup2).nonEmpty
  }

  trait IsConflict
  final case class NonConflict(leftEvents: EventGroup, rightEvents: EventGroup) extends IsConflict
  final case class Conflict(
      leftEvents: EventGroup,
      rightEvents: EventGroup,
      conflictSet: Set[Blake2b256Hash]
  ) extends IsConflict

  def isConflict[F[_]: Sync, C, P, A, K](
      historyRepo: HistoryRepository[F, C, P, A, K],
      baseState: Blake2b256Hash,
      leftEvents: List[Event],
      rightEvents: List[Event]
  )(implicit sc: Serialize[C]): F[IsConflict] = {
    val leftEventGroup  = extractEventGroup(leftEvents)
    val rightEventGroup = extractEventGroup(rightEvents)
    val conflictJoinInLeft =
      extractJoinedChannels(leftEventGroup).intersect(produceChannels(rightEventGroup))
    val conflictJoinInRight =
      extractJoinedChannels(rightEventGroup).intersect(produceChannels(leftEventGroup))
    val otherConflict          = containConflictingEvents(leftEventGroup, rightEventGroup)
    val normalConflictChannels = conflictJoinInLeft ++ conflictJoinInRight ++ otherConflict
    val nonconflictRightProduceChannels =
      rightEventGroup.produces.filter(p => !normalConflictChannels.contains(p.channelsHash))
    for {
      rightJoins <- nonconflictRightProduceChannels.toList.traverse { produce =>
                     for {
                       joins <- historyRepo.getJoinsFromChannelHash(
                                 baseState,
                                 produce.channelsHash
                               )
                       joinM = joins.filter(_.length > 1)
                     } yield (produce, joinM)
                   }
      leftProduceChannel = leftEventGroup.produces.map(_.channelsHash).toSet
      conflictJoinChannels = rightJoins
        .filter {
          case (produce, joins) => {
            val joinsChannelHashes  = joins.map(_.map(StableHashProvider.hash(_)(sc))).flatten
            val joinsWithoutProduce = joinsChannelHashes diff Seq(produce.channelsHash)
            joinsWithoutProduce.exists(p => leftProduceChannel.contains(p))
          }
        }
        .map(_._1.channelsHash)
        .toSet
    } yield
      if (normalConflictChannels.isEmpty && conflictJoinChannels.isEmpty) {
        NonConflict(leftEventGroup, rightEventGroup)
      } else {
        Conflict(
          leftEventGroup,
          rightEventGroup,
          normalConflictChannels ++ conflictJoinChannels
        )
      }
  }

  final case class MergeChanges(
      validDeploys: List[ProcessedDeploy],
      rejectedDeploys: List[ProcessedDeploy],
      validEventLogs: Seq[RSpaceEvent]
  )

  def isDeploysConflict[F[_]: Sync, C, P, A, K](
      historyRepo: HistoryRepository[F, C, P, A, K],
      baseState: Blake2b256Hash,
      leftDeploys: List[ProcessedDeploy],
      rightDeploys: List[ProcessedDeploy]
  )(implicit sc: Serialize[C]) = {
    // errored deploy is always non-conflict
    val leftNonErrDeploys  = leftDeploys.filter(!_.isFailed)
    val rightNonErrDeploys = rightDeploys.filter(!_.isFailed)
    isConflict(
      historyRepo,
      baseState,
      leftNonErrDeploys.flatMap(_.deployLog),
      rightNonErrDeploys.flatMap(_.deployLog)
    ).map { conflictCase =>
      conflictCase match {
        case NonConflict(_, rightEvents) =>
          MergeChanges(rightDeploys, List.empty[ProcessedDeploy], rightEvents.events)
        case Conflict(_, _, conflicts) => {
          val (conflictDeploys, nonConflictDeploys) = rightDeploys.partition(
            d =>
              d.deployLog.forall(
                e =>
                  EventConverter.toRspaceEvent(e) match {
                    case Produce(channelsHash, _, _) => conflicts.contains(channelsHash)
                    case Consume(channelsHasees, _, _) =>
                      channelsHasees.exists(conflicts.contains(_))
                    case COMM(consume, produces, _, _) =>
                      consume.channelsHashes.exists(conflicts.contains(_)) || produces.exists(
                        p => conflicts.contains(p.channelsHash)
                      )
                  }
              )
          )
          MergeChanges(
            nonConflictDeploys,
            conflictDeploys,
            extractEventGroup(nonConflictDeploys.flatMap(_.deployLog)).events
          )
        }
      }
    }
  }

  private[this] def containConflictingEvents(
      b1Events: EventGroup,
      b2Events: EventGroup
  ): Set[Blake2b256Hash] = {
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
      .toSet
  }

  /**
    * `Volatile` means that the produce event and the consume event match within the scope.
    * This produce and consume events within this comm event would not leave any thing left in the
    * tuplespace within the scope.
    */
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

  private[this] def produceChannels(events: EventGroup) =
    events.produces.map(_.channelsHash).toSet ++ events.comms.flatMap { comm =>
      comm.produces.map(_.channelsHash)
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

  /**
    * @return EventGroup contains all the events which is not volatile in the scope
    */
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
