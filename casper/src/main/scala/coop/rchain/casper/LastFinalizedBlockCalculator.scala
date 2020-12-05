package coop.rchain.casper

import cats.effect.{Concurrent, Sync}
import cats.implicits._
import coop.rchain.blockstorage.dag.{BlockDagRepresentation, BlockDagStorage}
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.deploy.DeployStorage
import coop.rchain.casper.syntax._
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.shared.Log
import coop.rchain.store.KeyValueTypedStore

import coop.rchain.blockstorage.finality.LastFinalizedStorage
import coop.rchain.casper.util.DagOperations
import coop.rchain.models.BlockMetadata

final class LastFinalizedBlockCalculator[F[_]: Sync: Log: Concurrent: BlockStore: BlockDagStorage: SafetyOracle: DeployStorage](
    faultToleranceThreshold: Float,
    finalizedBlocksStore: KeyValueTypedStore[F, BlockHash, Unit]
) {
  def run(dag: BlockDagRepresentation[F], lastFinalizedBlockHash: BlockHash): F[BlockHash] =
    for {
      maybeChildrenHashes <- dag.children(lastFinalizedBlockHash)
      childrenHashes      = maybeChildrenHashes.getOrElse(Set.empty[BlockHash]).toList
      maybeFinalizedChild <- childrenHashes.findM(isGreaterThanFaultToleranceThreshold(dag, _))
      newFinalizedBlock <- maybeFinalizedChild match {
                            case Some(finalizedChild) =>
                              removeDeploysInFinalizedBlock(finalizedChild) >> storeFinalizedBlock(
                                finalizedChild
                              ) >> run(
                                dag,
                                finalizedChild
                              )
                            case None => lastFinalizedBlockHash.pure[F]
                          }
    } yield newFinalizedBlock

  private def storeFinalizedBlock(blockHash: BlockHash) =
    LastFinalizedBlockCalculator.storeFinalizedBlock(finalizedBlocksStore)(List(blockHash))

  private def removeDeploysInFinalizedBlock(
      finalizedChildHash: BlockHash
  ): F[Unit] =
    for {
      block          <- BlockStore[F].getUnsafe(finalizedChildHash)
      deploys        = block.body.deploys.map(_.deploy)
      deploysRemoved <- DeployStorage[F].remove(deploys)
      _ <- Log[F].info(
            s"Removed $deploysRemoved deploys from deploy history as we finalized block ${PrettyPrinter
              .buildString(finalizedChildHash)}."
          )
    } yield ()

  /*
   * On the first pass, block B is finalized if B's main parent block is finalized
   * and the safety oracle says B's normalized fault tolerance is above the threshold.
   * On the second pass, block B is finalized if any of B's children blocks are finalized.
   *
   * TODO: Implement the second pass in BlockAPI
   */
  private def isGreaterThanFaultToleranceThreshold(
      dag: BlockDagRepresentation[F],
      blockHash: BlockHash
  ): F[Boolean] =
    for {
      faultTolerance <- SafetyOracle[F].normalizedFaultTolerance(dag, blockHash)
      _ <- Log[F].info(
            s"Fault tolerance for block ${PrettyPrinter.buildString(blockHash)} is $faultTolerance."
          )
    } yield faultTolerance > faultToleranceThreshold

}

object LastFinalizedBlockCalculator {
  def apply[F[_]](implicit ev: LastFinalizedBlockCalculator[F]): LastFinalizedBlockCalculator[F] =
    ev

  def apply[F[_]: Sync: Log: Concurrent: BlockStore: BlockDagStorage: SafetyOracle: DeployStorage](
      faultToleranceThreshold: Float,
      finalizedBlocksStore: KeyValueTypedStore[F, BlockHash, Unit]
  ): LastFinalizedBlockCalculator[F] =
    new LastFinalizedBlockCalculator[F](faultToleranceThreshold, finalizedBlocksStore)

  def storeFinalizedBlock[F[_]: Sync](
      finalizedBlocksStore: KeyValueTypedStore[F, BlockHash, Unit]
  )(blockHash: List[BlockHash]): F[Unit] =
    for {
      _ <- finalizedBlocksStore.put(blockHash.map(b => (b, ())))
    } yield ()

  def storeHistoryFinalizedBlocks[F[_]: Sync: Log](
      finalizedBlocksStore: KeyValueTypedStore[F, BlockHash, Unit],
      blockStore: BlockStore[F],
      blockDagStore: BlockDagStorage[F],
      lastFinalizedStore: LastFinalizedStorage[F]
  ): F[Unit] =
    for {
      dag              <- blockDagStore.getRepresentation
      approvedBlockOpt <- blockStore.getApprovedBlock
      _ <- approvedBlockOpt match {
            case Some(approvedBlock) =>
              for {
                lastFinalizedHash  <- lastFinalizedStore.get(approvedBlock.candidate.block)
                lastFinalizedBlock <- blockStore.getUnsafe(lastFinalizedHash)
                lastFinalizedMeta  = BlockMetadata.fromBlock(lastFinalizedBlock, false)
                finalizedBlocks <- DagOperations
                                    .bfTraverseF[F, BlockMetadata](List(lastFinalizedMeta))(
                                      b =>
                                        for {
                                          parents <- b.parents.traverse(dag.lookupUnsafe)
                                        } yield parents
                                    )
                                    .map(_.blockHash)
                                    .toList
                _ <- storeFinalizedBlock(finalizedBlocksStore)(finalizedBlocks)
              } yield ()
            case None =>
              Log[F].info(
                "No valid data in storage.Won't construct FinalizedBlockHash cache store."
              )
          }

    } yield ()

}
