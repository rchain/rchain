package coop.rchain.casper

import cats.effect.{Concurrent, Sync}
import cats.implicits._
import coop.rchain.blockstorage.dag.{BlockDagRepresentation, BlockDagStorage}
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.CasperState.CasperStateCell
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.catscontrib.ListContrib
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.shared.Log

final class LastFinalizedBlockCalculator[F[_]: Sync: Log: Concurrent: BlockStore: BlockDagStorage: SafetyOracle](
    faultToleranceThreshold: Float
) {
  def run(dag: BlockDagRepresentation[F], lastFinalizedBlockHash: BlockHash)(
      implicit state: CasperStateCell[F]
  ): F[BlockHash] =
    for {
      maybeChildrenHashes <- dag.children(lastFinalizedBlockHash)
      childrenHashes      = maybeChildrenHashes.getOrElse(Set.empty[BlockHash]).toList
      maybeFinalizedChild <- ListContrib.findM(
                              childrenHashes,
                              (blockHash: BlockHash) =>
                                isGreaterThanFaultToleranceThreshold(dag, blockHash)
                            )
      newFinalizedBlock <- maybeFinalizedChild match {
                            case Some(finalizedChild) =>
                              removeDeploysInFinalizedBlock(finalizedChild) >> run(
                                dag,
                                finalizedChild
                              )
                            case None => lastFinalizedBlockHash.pure[F]
                          }
    } yield newFinalizedBlock

  private def removeDeploysInFinalizedBlock(
      finalizedChildHash: BlockHash
  )(implicit state: CasperStateCell[F]): F[Unit] =
    for {
      block              <- ProtoUtil.getBlock[F](finalizedChildHash)
      deploys            = block.body.deploys.map(_.deploy)
      stateBefore        <- state.read
      initialHistorySize = stateBefore.deployHistory.size
      _ <- state.modify { s =>
            s.copy(deployHistory = s.deployHistory -- deploys)
          }
      stateAfter     <- state.read
      deploysRemoved = initialHistorySize - stateAfter.deployHistory.size
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

  def apply[F[_]: Sync: Log: Concurrent: BlockStore: BlockDagStorage: SafetyOracle](
      faultToleranceThreshold: Float
  ): LastFinalizedBlockCalculator[F] =
    new LastFinalizedBlockCalculator[F](faultToleranceThreshold)
}
