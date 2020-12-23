package coop.rchain.casper

import cats.Monad
import cats.effect.Sync
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.BlockDagRepresentation
import coop.rchain.blockstorage.deploy.DeployStorage
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil._
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.casper.util.rholang.costacc.{CloseBlockDeploy, SlashDeploy}
import coop.rchain.casper.util.rholang.{SystemDeploy, _}
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.signatures.Signed
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockMetadata
import coop.rchain.models.Validator.Validator
import coop.rchain.rholang.interpreter.SystemProcesses.BlockData
import coop.rchain.shared.{DagOps, Log, Time}
import coop.rchain.casper.util.rholang.SystemDeployUtil

object BlockCreator {
  private[this] val CreateBlockMetricsSource =
    Metrics.Source(CasperMetricsSource, "create-block")
  private[this] val ProcessDeploysAndCreateBlockMetricsSource =
    Metrics.Source(CasperMetricsSource, "process-deploys-and-create-block")

  private def isActiveValidator[F[_]: Sync](
      block: BlockMessage,
      runtimeManager: RuntimeManager[F],
      validatorId: ValidatorIdentity
  ): F[Boolean] = {
    val lastProposedTuplespace = ProtoUtil.postStateHash(block)
    for {
      activeValidators <- runtimeManager.getActiveValidators(lastProposedTuplespace)
      validator        = ByteString.copyFrom(validatorId.publicKey.bytes)
      isActive         = activeValidators.contains(validator)
    } yield isActive
  }

  private def isBonded[F[_]: Sync](
      blockMeta: BlockMetadata,
      validator: Validator
  ): Boolean =
    blockMeta.weightMap.getOrElse(validator, 0L) > 0L // consider stake greater than 0 as bonded

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
  def createBlock[F[_]: Sync: Log: Time: BlockStore: Estimator: DeployStorage: Metrics](
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
        _                     <- Log[F].info(s"Creating block with maxBlockNumber ${maxBlockNumber}")
        invalidLatestMessages <- ProtoUtil.invalidLatestMessages(dag)
        deploys               <- extractDeploys(dag, parentMetadatas, maxBlockNumber, expirationThreshold)
        sender                = ByteString.copyFrom(validatorIdentity.publicKey.bytes)
        latestMessageOpt      <- dag.latestMessage(sender)
        seqNum                = latestMessageOpt.fold(0)(_.seqNum) + 1
        _                     <- Log[F].info(s"Creating block with seqNum ${seqNum}")
        // if the node is already not bonded by the parent, the node won't slash once more
        invalidLatestMessagesExcludeUnbonded = invalidLatestMessages.filter {
          case (validator, _) => isBonded(parentMetadatas.head, validator)
        }
        // TODO: Add `slashingDeploys` to DeployStorage
        slashingDeploys = invalidLatestMessagesExcludeUnbonded.values.map(
          invalidBlockHash =>
            SlashDeploy(
              invalidBlockHash,
              validatorIdentity.publicKey,
              SystemDeployUtil.generateSlashDeployRandomSeed(sender, seqNum)
            )
        )
        parents <- parentMetadatas.toList.traverse(p => BlockStore[F].getUnsafe(p.blockHash))
        // there are 3 situations on judging whether the validator is active
        // 1. it is possible that you are active in some parents but not active in other parents
        // 2. all of the parents are in active state
        // 3. all of the parents are not in active state
        // Let's just talk about 1 situation because 2 and 3 are easy to judge.
        // If one validator issue a bonding or unbonding request , it would be possible to end up with
        // parent in 1 situation. It would be safer for the validator to make sure all parents post state
        // are in active state.
        isActive = parents
          .traverse(b => isActiveValidator(b, runtimeManager, validatorIdentity))
          .map(_.forall(identity))
        justifications   <- computeJustifications(dag, parents)
        now              <- Time[F].currentMillis
        invalidBlocksSet <- dag.invalidBlocks
        invalidBlocks    = invalidBlocksSet.map(block => (block.blockHash, block.sender)).toMap
        // make sure closeBlock is the last system Deploy
        systemDeploys = slashingDeploys.toList :+ CloseBlockDeploy(
          SystemDeployUtil.generateCloseDeployRandomSeed(sender, seqNum)
        )
        unsignedBlock <- isActive.ifM(
                          if (deploys.nonEmpty || slashingDeploys.nonEmpty) {
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
                              seqNum,
                              invalidBlocks
                            )
                          } else {
                            CreateBlockStatus.noNewDeploys.pure[F]
                          },
                          CreateBlockStatus.readOnlyMode.pure[F]
                        )
        _           <- spanF.mark("block-created")
        signedBlock = unsignedBlock.map(validatorIdentity.signBlock)
        _           <- spanF.mark("block-signed")
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
      result <- DagOps
                 .bfTraverseF[F, BlockMetadata](parents.toList)(
                   b =>
                     ProtoUtil
                       .getParentMetadatasAboveBlockNumber(b, earliestBlockNumber, dag)
                 )
                 .foldLeftF(validDeploys) { (deploys, blockMetadata) =>
                   for {
                     block        <- BlockStore[F].getUnsafe(blockMetadata.blockHash)
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
  private def computeJustifications[F[_]: Sync](
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
      seqNum: Int,
      invalidBlocks: Map[BlockHash, Validator]
  )(implicit spanF: Span[F]): F[CreateBlockStatus] =
    spanF.trace(ProcessDeploysAndCreateBlockMetricsSource) {
      (for {
        blockData <- BlockData(now, maxBlockNumber + 1, sender, seqNum).pure
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
    val block  = unsignedBlockProto(body, header, justifications, shardId, blockData.seqNum)
    CreateBlockStatus.created(block)
  }
}
