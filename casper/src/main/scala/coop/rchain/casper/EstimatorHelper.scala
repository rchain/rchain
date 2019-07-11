package coop.rchain.casper

import cats.Monad
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.{BlockDagRepresentation, BlockStore}
import coop.rchain.casper.protocol.Event.EventInstance.{Comm, Consume, Produce}
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.{DagOperations, ProtoUtil}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockMetadata
import coop.rchain.shared.Log

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

  /*
   * Block b1 conflicts with b2 if any of b1's ancestors contains a replay log entry that
   * touches a channel that any of b2's ancestors' (that are not common with b1's ancestors)
   * replay log entries touch.
   */
  private[casper] def conflicts[F[_]: Monad: Log: BlockStore](
      b1: BlockMessage,
      b2: BlockMessage,
      dag: BlockDagRepresentation[F]
  ): F[Boolean] =
    dag.deriveOrdering(0L).flatMap { // TODO: Replace with something meaningful
      implicit ordering =>
        for {
          b1MetaDataOpt        <- dag.lookup(b1.blockHash)
          b2MetaDataOpt        <- dag.lookup(b2.blockHash)
          blockMetaDataSeq     = Vector(b1MetaDataOpt.get, b2MetaDataOpt.get)
          uncommonAncestorsMap <- DagOperations.uncommonAncestors[F](blockMetaDataSeq, dag)
          (b1AncestorsMap, b2AncestorsMap) = uncommonAncestorsMap.partition {
            case (_, bitSet) => bitSet == BitSet(0)
          }
          b1AncestorsMeta    = b1AncestorsMap.keys
          b2AncestorsMeta    = b2AncestorsMap.keys
          b1AncestorChannels <- buildBlockAncestorChannels[F](b1AncestorsMeta.toList)
          b2AncestorChannels <- buildBlockAncestorChannels[F](b2AncestorsMeta.toList)
          conflicts          = b1AncestorChannels.intersect(b2AncestorChannels).nonEmpty
          _ <- if (conflicts) {
                Log[F].info(
                  s"Block ${PrettyPrinter.buildString(b1.blockHash)} and ${PrettyPrinter
                    .buildString(b2.blockHash)} conflicts."
                )
              } else {
                Log[F].info(
                  s"Block ${PrettyPrinter
                    .buildString(b1.blockHash)}'s channels ${b1AncestorChannels.map(PrettyPrinter.buildString).mkString(",")} and block ${PrettyPrinter
                    .buildString(b2.blockHash)}'s channels ${b2AncestorChannels.map(PrettyPrinter.buildString).mkString(",")} don't intersect."
                )
              }
        } yield conflicts
    }

  val extractProduce: Event => Option[ProduceEvent] = {
    case Event(Produce(produce: ProduceEvent)) => Some(produce)
    case _                                     => None
  }

  val extractConsume: Event => Option[ConsumeEvent] = {
    case Event(Consume(consume: ConsumeEvent)) => Some(consume)
    case _                                     => None
  }

  val extractComm: Event => Option[CommEvent] = {
    case Event(Comm(commEvent: CommEvent)) => Some(commEvent)
    case _                                 => None
  }

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  private def buildBlockAncestorChannels[F[_]: Monad: BlockStore](
      blockAncestorsMeta: List[BlockMetadata]
  ): F[Set[ByteString]] =
    for {
      maybeAncestors <- blockAncestorsMeta.traverse(
                         blockAncestorMeta => BlockStore[F].get(blockAncestorMeta.blockHash)
                       )
      ancestors = maybeAncestors.flatten
      ancestorEvents = ancestors.flatMap(_.getBody.deploys.flatMap(_.deployLog)) ++
        ancestors.flatMap(_.getBody.deploys.flatMap(_.paymentLog))

      produceEvents = ancestorEvents.map(extractProduce).flatten
      consumeEvents = ancestorEvents.map(extractConsume).flatten
      commEvents    = ancestorEvents.map(extractComm).flatten

      producesInCommEvents = commEvents.flatMap(_.produces)
      consumeInCommEvents = commEvents.flatMap(_.consume)

      ancestorProduceChannels = ancestorEvents
        .map(extractProduce)
        .flatten
        .filterNot(producesInCommEvents.contains(_))
        .map(p => p.channelsHash)
        .toSet

      ancestorConsumeChannels = ancestorEvents
        .map(extractConsume)
        .flatten
        .filterNot(consumeInCommEvents.contains(_))
        .flatMap(c => c.channelsHashes)
        .toSet

      ancestorCommChannels = ancestorEvents
        .map(extractComm)
        .flatten
        .filterNot {
          case CommEvent(Some(consume: ConsumeEvent), produces) =>
            consumeEvents.contains(consume) && produces.forall(
              produce => produceEvents.contains(produce)
            )
          case _ => false
        }
        .flatMap {
          case CommEvent(Some(consume: ConsumeEvent), produces) =>
            consume.channelsHashes ++ produces.map(_.channelsHash)
          case e @ CommEvent(None, _) =>
            throw new RuntimeException(s"Unexpected comm event $e")
        }
        .toSet

      ancestorChannels = ancestorProduceChannels ++ ancestorConsumeChannels ++ ancestorCommChannels
    } yield ancestorChannels
}
