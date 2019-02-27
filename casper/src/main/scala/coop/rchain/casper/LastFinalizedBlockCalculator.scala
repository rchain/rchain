package coop.rchain.casper

import cats.Monad
import cats.effect.{Concurrent, Sync}
import cats.implicits._
import coop.rchain.blockstorage.{BlockDagRepresentation, BlockDagStorage, BlockStore}
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.shared.{Cell, Log}
import coop.rchain.catscontrib.ListContrib

object LastFinalizedBlockCalculator {
  // TODO: Extract hardcoded fault tolerance threshold
  private val faultToleranceThreshold = 0f

  def run[F[_]: Monad: Log: Sync: Concurrent: BlockStore: BlockDagStorage: SafetyOracle](
      dag: BlockDagRepresentation[F],
      lastFinalizedBlockHash: BlockHash
  )(implicit state: Cell[F, CasperState]): F[BlockHash] =
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
                              removeDeploysInFinalizedBlock[F](finalizedChild) *> run[F](
                                dag,
                                finalizedChild
                              )
                            case None => lastFinalizedBlockHash.pure[F]
                          }
    } yield newFinalizedBlock

  private def removeDeploysInFinalizedBlock[F[_]: Monad: Log: Sync: Concurrent: BlockStore: BlockDagStorage: SafetyOracle](
      finalizedChildHash: BlockHash
  )(implicit state: Cell[F, CasperState]): F[Unit] =
    for {
      block              <- ProtoUtil.unsafeGetBlock[F](finalizedChildHash)
      deploys            = block.body.get.deploys.map(_.deploy.get).toList
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
  private def isGreaterThanFaultToleranceThreshold[F[_]: Monad: Log: Sync: Concurrent: BlockStore: BlockDagStorage: SafetyOracle](
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
