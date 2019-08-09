package coop.rchain.casper

import cats.Monad
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.{BlockDagRepresentation, BlockStore}
import coop.rchain.casper.protocol.{Event => CasperEvent, _}
import coop.rchain.casper.util.{DagOperations, EventConverter, ProtoUtil}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockMetadata
import coop.rchain.shared.Log
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.trace._

import scala.collection.BitSet

final case class BlockEvents(produces: Set[Produce], consumes: Set[Consume], comms: Set[COMM])

object EstimatorHelper {

  def chooseNonConflicting[F[_]: Monad: Log: BlockStore](
      blockHashes: Seq[BlockHash],
      dag: BlockDagRepresentation[F]
  ): F[Seq[BlockMessage]] = {
    def nonConflicting(b: BlockMessage): BlockMessage => F[Boolean] =
      conflicts[F](_, b, dag).map(b => !b)

    for {
      blocks <- blockHashes.toList.traverse(hash => ProtoUtil.unsafeGetBlock[F](hash))
      result <- blocks
                 .foldM(List.empty[BlockMessage]) {
                   case (acc, b) =>
                     Monad[F].ifM(acc.forallM(nonConflicting(b)))(
                       (b :: acc).pure[F],
                       acc.pure[F]
                     )
                 }
                 .map(_.reverse)
    } yield result
  }

  private[casper] def conflicts[F[_]: Monad: Log: BlockStore](
      b1: BlockMessage,
      b2: BlockMessage,
      dag: BlockDagRepresentation[F]
  ): F[Boolean] =
    dag.deriveOrdering(0L).flatMap { implicit ordering =>
      for {
        b1MetaDataOpt <- dag.lookup(b1.blockHash)
        b2MetaDataOpt <- dag.lookup(b2.blockHash)
        uncommonAncestorsMap <- DagOperations.uncommonAncestors[F](
                                 Vector(b1MetaDataOpt.get, b2MetaDataOpt.get),
                                 dag
                               )
        (b1AncestorsMap, b2AncestorsMap) = uncommonAncestorsMap.partition {
          case (_, bitSet) => bitSet == BitSet(0)
        }
        b1Events <- extractBlockEvents[F](b1AncestorsMap.keys.toList)
        b2Events <- extractBlockEvents[F](b2AncestorsMap.keys.toList)
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
    }

  private[this] def containConflictingEvents(
      b1Events: BlockEvents,
      b2Events: BlockEvents
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
          (channel, channelConflicts(v, b2Ops.get(channel).getOrElse(Set.empty)))
      }
    conflictPerChannel
      .filter { case (_, conflicts) => conflicts }
      .keys
      .nonEmpty
  }

  private[this] def isVolatile(comm: COMM, consumes: Set[Consume], produces: Set[Produce]) =
    !comm.consume.persistent && consumes.contains(comm.consume) && comm.produces.forall(
      produce => !produce.persistent && produces.contains(produce)
    )

  private[this] def allChannels(events: BlockEvents) =
    events.produces.map(_.channelsHash).toSet ++ events.consumes
      .flatMap(_.channelsHashes)
      .toSet ++ events.comms.flatMap { comm =>
      comm.consume.channelsHashes ++ comm.produces.map(_.channelsHash)
    }.toSet

  private[this] def extractBlockEvents[F[_]: Monad: BlockStore](
      blockAncestorsMeta: List[BlockMetadata]
  ): F[BlockEvents] =
    for {
      maybeAncestors <- blockAncestorsMeta.traverse(
                         blockAncestorMeta => BlockStore[F].get(blockAncestorMeta.blockHash)
                       )
      ancestors = maybeAncestors.flatten
      ancestorEvents = (ancestors.flatMap(_.getBody.deploys.flatMap(_.deployLog)) ++
        ancestors.flatMap(_.getBody.deploys.flatMap(_.paymentLog)))
        .map(EventConverter.toRspaceEvent)
        .toSet

      allProduceEvents = ancestorEvents.collect { case p: Produce => p }
      allConsumeEvents = ancestorEvents.collect { case c: Consume => c }
      allCommEvents    = ancestorEvents.collect { case c: COMM    => c }
      (volatileCommEvents, nonVolatileCommEvents) = allCommEvents
        .partition(isVolatile(_, allConsumeEvents, allProduceEvents))
      producesInVolatileCommEvents = volatileCommEvents.flatMap(_.produces)
      consumesInVolatileCommEvents = volatileCommEvents.map(_.consume)
      produceEvents                = allProduceEvents.filterNot(producesInVolatileCommEvents.contains(_))
      consumeEvents                = allConsumeEvents.filterNot(consumesInVolatileCommEvents.contains(_))
    } yield BlockEvents(produceEvents, consumeEvents, nonVolatileCommEvents)

  private[this] def extractJoinedChannels(b: BlockEvents): Set[Blake2b256Hash] = {

    def joinedChannels(consumes: Set[Consume]) =
      consumes.withFilter(Consume.hasJoins).flatMap(_.channelsHashes)

    joinedChannels(b.consumes) ++ joinedChannels(b.comms.map(_.consume))

  }

  private[this] def tuplespaceEventsPerChannel(
      b: BlockEvents
  ): Map[Blake2b256Hash, Set[TuplespaceEvent]] = {
    val nonPersistentProducesInComms = b.comms.flatMap(_.produces).filterNot(_.persistent)
    val nonPersistentConsumesInComms = b.comms.map(_.consume).filterNot(_.persistent)
    val freeProduces                 = b.produces.diff(nonPersistentProducesInComms)
    val freeConsumes                 = b.consumes.diff(nonPersistentConsumesInComms)

    val produceEvents = freeProduces.map(TuplespaceEvent.from(_))
    val consumeEvents = freeConsumes.flatMap(TuplespaceEvent.from(_))
    val commEvents    = b.comms.flatMap(TuplespaceEvent.from(_, b.produces))

    (produceEvents ++ consumeEvents ++ commEvents)
      .groupBy(_._1)
      .mapValues[Set[TuplespaceEvent]](_.map(_._2))
  }

}
