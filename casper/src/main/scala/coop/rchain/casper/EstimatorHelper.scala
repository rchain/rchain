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

  private def isVolatile(comm: COMM, consumes: Seq[Consume], produces: Seq[Produce]) =
    !comm.consume.persistent && consumes.contains(comm.consume) && comm.produces.forall(
      produce => !produce.persistent && produces.contains(produce)
    )

  private def buildBlockAncestorChannels[F[_]: Monad: BlockStore](
      blockAncestorsMeta: List[BlockMetadata]
  ): F[Set[ByteString]] =
    for {
      maybeAncestors <- blockAncestorsMeta.traverse(
                         blockAncestorMeta => BlockStore[F].get(blockAncestorMeta.blockHash)
                       )
      ancestors = maybeAncestors.flatten
      ancestorEvents = (ancestors.flatMap(_.getBody.deploys.flatMap(_.deployLog)) ++
        ancestors.flatMap(_.getBody.deploys.flatMap(_.paymentLog)))
        .map(EventConverter.toRspaceEvent)

      produceEvents = ancestorEvents.collect { case p: Produce => p }
      consumeEvents = ancestorEvents.collect { case c: Consume => c }
      commEvents    = ancestorEvents.collect { case c: COMM    => c }

      producesInCommEvents = commEvents.flatMap(_.produces)
      consumeInCommEvents  = commEvents.map(_.consume)

      ancestorProduceChannels = produceEvents
        .filterNot(producesInCommEvents.contains(_))
        .map(_.channelsHash)
        .toSet

      ancestorConsumeChannels = consumeEvents
        .filterNot(consumeInCommEvents.contains(_))
        .flatMap(_.channelsHashes)
        .toSet

      ancestorCommChannels = commEvents
        .filterNot(isVolatile(_, consumeEvents, produceEvents))
        .flatMap { comm =>
          comm.consume.channelsHashes ++ comm.produces.map(_.channelsHash)
        }
        .toSet

      ancestorChannels = ancestorProduceChannels ++ ancestorConsumeChannels ++ ancestorCommChannels
      res              = ancestorChannels.map(_.toByteString)
    } yield res
}
