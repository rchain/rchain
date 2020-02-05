package coop.rchain.casper

import cats.Monad
import cats.effect.Sync
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.BlockDagRepresentation
import coop.rchain.blockstorage.deploy.DeployStorage
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil._
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.casper.util.rholang.costacc.{CloseBlockDeploy, SlashDeploy}
import coop.rchain.casper.util.rholang.{SystemDeploy, _}
import coop.rchain.casper.util.{DagOperations, ProtoUtil}
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.signatures.Signed
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockMetadata
import coop.rchain.models.Validator.Validator
import coop.rchain.rholang.interpreter.Runtime.BlockData
import coop.rchain.shared.{Cell, Log, Time}

object BlockCreator {
  private[this] val CreateBlockMetricsSource =
    Metrics.Source(CasperMetricsSource, "create-block")
  private[this] val ProcessDeploysAndCreateBlockMetricsSource =
    Metrics.Source(CasperMetricsSource, "process-deploys-and-create-block")

  /*
   * Overview of createBlock
   *
   *  1. Rank each of the block DAG's latest messages (blocks) via the LMD GHOST estimator.
   *  2. Let each latest message have a score of 2^(-i) where i is the index of that latest message in the ranking.
   *     Take a subset S of the latest messages such that the sum of scores is the greatest and
   *     none of the blocks in S conflicts with each other. S will become the parents of the
   *     about-to-be-created block.
   *  3. Extract all valid deploys that aren't already in all ancestors of S (the parents).
   *  4. Create a new block that contains the deploys from the previous step.
   */
  def createBlock[F[_]: Sync: Log: Time: BlockStore: SynchronyConstraintChecker: LastFinalizedHeightConstraintChecker: Estimator: DeployStorage: Metrics](
      dag: BlockDagRepresentation[F],
      genesis: BlockMessage,
      validatorIdentity: ValidatorIdentity,
      shardId: String,
      version: Long,
      expirationThreshold: Int,
      runtimeManager: RuntimeManager[F]
  )(implicit spanF: Span[F]): F[CreateBlockStatus] =
    spanF.trace(CreateBlockMetricsSource) {
      import cats.instances.list._
      for {
        tipHashes             <- Estimator[F].tips(dag, genesis)
        _                     <- spanF.mark("after-estimator")
        parentMetadatas       <- EstimatorHelper.chooseNonConflicting(tipHashes, dag)
        maxBlockNumber        = ProtoUtil.maxBlockNumberMetadata(parentMetadatas)
        invalidLatestMessages <- ProtoUtil.invalidLatestMessages(dag)
        // TODO: Add `slashingDeploys` to DeployStorage
        slashingDeploys = invalidLatestMessages.values.toList.map(
          invalidBlockHash =>
            // TODO: Do something useful with the result of "slash".
            SlashDeploy(
              invalidBlockHash,
              validatorIdentity.publicKey,
              Tools.rng(invalidBlockHash.toByteArray)
            )
        )
        deploys          <- extractDeploys(dag, parentMetadatas, maxBlockNumber, expirationThreshold)
        parents          <- parentMetadatas.toList.traverse(p => ProtoUtil.getBlock(p.blockHash))
        justifications   <- computeJustifications(dag, parents)
        now              <- Time[F].currentMillis
        invalidBlocksSet <- dag.invalidBlocks
        invalidBlocks    = invalidBlocksSet.map(block => (block.blockHash, block.sender)).toMap
        // make sure closeBlock is the last system Deploy
        systemDeploys = slashingDeploys :+ CloseBlockDeploy(
          Tools.rng(parents.head.blockHash.toByteArray)
        )
        unsignedBlock <- if (deploys.nonEmpty || slashingDeploys.nonEmpty) {
                          processDeploysAndCreateBlock(
                            dag,
                            runtimeManager,
                            parents,
                            deploys,
                            systemDeploys,
                            justifications,
                            maxBlockNumber,
                            validatorIdentity.publicKey,
                            shardId,
                            version,
                            now,
                            invalidBlocks
                          )
                        } else {
                          CreateBlockStatus.noNewDeploys.pure[F]
                        }
        _ <- spanF.mark("block-created")
        signedBlock <- unsignedBlock.mapF(
                        signBlock(
                          _,
                          dag,
                          validatorIdentity.publicKey,
                          validatorIdentity.privateKey,
                          validatorIdentity.sigAlgorithm,
                          shardId
                        )
                      )
        _ <- spanF.mark("block-signed")
      } yield signedBlock
    }

  // TODO: Remove no longer valid deploys here instead of with lastFinalizedBlock call
  private def extractDeploys[F[_]: Sync: Log: BlockStore: DeployStorage](
      dag: BlockDagRepresentation[F],
      parents: Seq[BlockMetadata],
      maxBlockNumber: Long,
      expirationThreshold: Int
  ): F[Seq[Signed[DeployData]]] =
    for {
      deploys             <- DeployStorage[F].getUnfinalized
      currentBlockNumber  = maxBlockNumber + 1
      earliestBlockNumber = currentBlockNumber - expirationThreshold
      validDeploys = deploys.filter(
        d =>
          notFutureDeploy(currentBlockNumber, d.data) && notExpiredDeploy(
            earliestBlockNumber,
            d.data
          )
      )
      result <- DagOperations
                 .bfTraverseF[F, BlockMetadata](parents.toList)(
                   b =>
                     ProtoUtil
                       .getParentMetadatasAboveBlockNumber(b, earliestBlockNumber, dag)
                 )
                 .foldLeftF(validDeploys) { (deploys, blockMetadata) =>
                   for {
                     block        <- ProtoUtil.getBlock(blockMetadata.blockHash)
                     blockDeploys = ProtoUtil.deploys(block).map(_.deploy)
                   } yield deploys -- blockDeploys
                 }
    } yield result.toSeq

  private def notExpiredDeploy(earliestBlockNumber: Long, d: DeployData): Boolean =
    d.validAfterBlockNumber > earliestBlockNumber

  private def notFutureDeploy(currentBlockNumber: Long, d: DeployData): Boolean =
    d.validAfterBlockNumber < currentBlockNumber

  /*
   * We ensure that only the justifications given in the block are those
   * which are bonded validators in the chosen parent. This is safe because
   * any latest message not from a bonded validator will not change the
   * final fork-choice.
   */
  private def computeJustifications[F[_]: Monad](
      dag: BlockDagRepresentation[F],
      parents: Seq[BlockMessage]
  ): F[Seq[Justification]] = {
    val bondedValidators = bonds(parents.head).map(_.validator).toSet
    dag.latestMessages.map { latestMessages =>
      toJustification(latestMessages)
        .filter(j => bondedValidators.contains(j.validator))
    }
  }

  private def processDeploysAndCreateBlock[F[_]: Sync: Log: BlockStore: Metrics](
      dag: BlockDagRepresentation[F],
      runtimeManager: RuntimeManager[F],
      parents: Seq[BlockMessage],
      deploys: Seq[Signed[DeployData]],
      systemDeploys: Seq[SystemDeploy],
      justifications: Seq[Justification],
      maxBlockNumber: Long,
      sender: PublicKey,
      shardId: String,
      version: Long,
      now: Long,
      invalidBlocks: Map[BlockHash, Validator]
  )(implicit spanF: Span[F]): F[CreateBlockStatus] =
    spanF.trace(ProcessDeploysAndCreateBlockMetricsSource) {
      (for {
        blockData <- BlockData(now, maxBlockNumber + 1, sender, parents.head.blockHash.toByteArray).pure
        result <- InterpreterUtil.computeDeploysCheckpoint(
                   parents,
                   deploys,
                   systemDeploys,
                   dag,
                   runtimeManager,
                   blockData,
                   invalidBlocks
                 )
        (preStateHash, postStateHash, processedDeploys, processedSystemDeploys) = result
        newBonds                                                                <- runtimeManager.computeBonds(postStateHash)
        _                                                                       <- spanF.mark("before-packing-block")
        block = createBlock(
          blockData,
          parents,
          justifications,
          preStateHash,
          postStateHash,
          processedDeploys,
          processedSystemDeploys,
          newBonds,
          shardId,
          version
        )
      } yield block)
      //TODO both the log message and block status seems misleading - the error could have happened anywhere,
      // e.g. during `replayIntoMergeBlock`
        .onError {
          case ex =>
            Log[F].error(s"Critical error encountered while processing deploys", ex)
        }
        .handleError(CreateBlockStatus.internalDeployError)
    }

  private def createBlock(
      blockData: BlockData,
      p: Seq[BlockMessage],
      justifications: Seq[Justification],
      preStateHash: StateHash,
      postStateHash: StateHash,
      persistableDeploys: Seq[ProcessedDeploy],
      persistableSystemDeploys: Seq[ProcessedSystemDeploy],
      newBonds: Seq[Bond],
      shardId: String,
      version: Long
  ): CreateBlockStatus = {
    val postState = RChainState(preStateHash, postStateHash, newBonds.toList, blockData.blockNumber)

    val body   = Body(postState, persistableDeploys.toList, persistableSystemDeploys.toList)
    val header = blockHeader(body, p.map(_.blockHash), version, blockData.timeStamp)
    val block  = unsignedBlockProto(body, header, justifications, shardId)
    CreateBlockStatus.created(block)
  }
}
