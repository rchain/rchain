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

    val b2Ops = tuplespaceEventsPerChannel(b2Events)
    tuplespaceEventsPerChannel(b1Events)
      .map {
        case (k, v) =>
          (k, channelConflicts(v, b2Ops.get(k).getOrElse(Set.empty)))
      }
      .filter { case (_, v) => v }
      .keys
      .nonEmpty
  }

  private[this] def isVolatile(comm: COMM, consumes: Set[Consume], produces: Set[Produce]) =
    !comm.consume.persistent && consumes.contains(comm.consume) && comm.produces.forall(
      produce => !produce.persistent && produces.contains(produce)
    )

  type BlockEvents = (Set[Produce], Set[Consume], Set[COMM])

  private[this] def allChannels(events: BlockEvents) = events match {
    case (freeProduceEvents, freeConsumeEvents, nonVolatileCommEvents) =>
      freeProduceEvents.map(_.channelsHash).toSet ++ freeConsumeEvents
        .flatMap(_.channelsHashes)
        .toSet ++ nonVolatileCommEvents.flatMap { comm =>
        comm.consume.channelsHashes ++ comm.produces.map(_.channelsHash)
      }.toSet
  }

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
    } yield (produceEvents, consumeEvents, nonVolatileCommEvents)

  private[this] def extractJoinedChannels(b: BlockEvents): Set[Blake2b256Hash] = {
    def joinedChannels(consumes: Set[Consume]) =
      consumes.withFilter(Consume.hasJoins).flatMap(_.channelsHashes)
    b match {
      case (_, consumes, comms) =>
        joinedChannels(consumes) ++ joinedChannels(comms.map(_.consume))
    }
  }

  private[this] def tuplespaceEventsPerChannel(
      b: BlockEvents
  ): Map[Blake2b256Hash, Set[TuplespaceEvent]] =
    b match {
      case (produces, consumes, comms) =>
        val nonPersistentProducesInComms = comms.flatMap(_.produces).filterNot(_.persistent)
        val nonPersistentConsumesInComms = comms.map(_.consume).filterNot(_.persistent)
        val freeProduces                 = produces.filterNot(nonPersistentProducesInComms.contains)
        val freeConsumes                 = consumes.filterNot(nonPersistentConsumesInComms.contains)
        val produceEvents = freeProduces
          .map(TuplespaceEvent.from(_))

        val consumeEvents = freeConsumes
          .flatMap(TuplespaceEvent.from(_))

        val commEvents = comms
          .flatMap(TuplespaceEvent.from(_, produces))

        (produceEvents
          .combine(consumeEvents)
          .combine(commEvents))
          .groupBy(_._1)
          .mapValues[Set[TuplespaceEvent]](_.map(_._2))
    }

  final case class TuplespaceEvent(
      incoming: TuplespaceOperation,
      matched: Option[TuplespaceOperation]
  )
  final case class TuplespaceOperation(
      polarity: Polarity,
      cardinality: Cardinality,
      eventHash: Blake2b256Hash
  )

  trait Polarity
  case object Send    extends Polarity
  case object Receive extends Polarity

  trait Cardinality
  case object Linear    extends Cardinality
  case object NonLinear extends Cardinality

  object TuplespaceEvent {

    implicit private[this] def liftProduce(produce: Produce): TuplespaceOperation =
      TuplespaceOperation(Send, if (produce.persistent) NonLinear else Linear, produce.hash)

    implicit private[this] def liftConsume(consume: Consume): TuplespaceOperation =
      TuplespaceOperation(Receive, if (consume.persistent) NonLinear else Linear, consume.hash)

    def from(produce: Produce): (Blake2b256Hash, TuplespaceEvent) =
      produce.channelsHash -> TuplespaceEvent(
        produce,
        None
      )

    def from(consume: Consume): Option[(Blake2b256Hash, TuplespaceEvent)] = consume match {
      case Consume(singleChannelHash :: Nil, _, _, _) =>
        Some(
          singleChannelHash -> TuplespaceEvent(
            consume,
            None
          )
        )
      case _ => None
    }

    def from(
        comm: COMM,
        produces: Set[Produce]
    ): Option[(Blake2b256Hash, TuplespaceEvent)] =
      comm match {
        case COMM(consume, produce :: Nil, _) => {
          val incoming: TuplespaceOperation =
            if (produces.contains(produce)) produce else consume
          val matched: Option[TuplespaceOperation] = Some(
            if (incoming == liftProduce(produce)) consume else produce
          )
          Some(
            produce.channelsHash -> TuplespaceEvent(
              incoming,
              matched
            )
          )
        }
        case _ => None
      }
  }

  implicit class TuplespaceEventOps(val ev: TuplespaceEvent) extends AnyVal {

    private[casper] def conflicts(other: TuplespaceEvent): Boolean =
      if (ev.incoming.polarity == other.incoming.polarity)
        (for {
          leftMatched  <- ev.matched
          rightMatched <- other.matched
        } yield leftMatched == rightMatched && rightMatched.cardinality == Linear).getOrElse(false)
      else
        !(
          ev.incoming.cardinality == Linear && other.incoming.cardinality == Linear && (ev.matched != None || other.matched != None) ||

            ev.incoming.cardinality == NonLinear && other.matched != None ||
            other.incoming.cardinality == NonLinear && ev.matched != None
        )
  }
}
