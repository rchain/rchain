package coop.rchain.casper

import cats.Monad
import cats.effect.Sync
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.BlockDagRepresentation
import coop.rchain.casper.CasperState.CasperStateCell
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil._
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.casper.util.rholang._
import coop.rchain.casper.util.{ConstructDeploy, DagOperations, ProtoUtil}
import coop.rchain.crypto.codec.Base16
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockMetadata
import coop.rchain.models.Validator.Validator
import coop.rchain.rholang.interpreter.Runtime.BlockData
import coop.rchain.shared.{Cell, Log, Time}

object BlockCreator {
  private[this] val CreateBlockMetricsSource =
    Metrics.Source(CasperMetricsSource, "create-block")

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
  def createBlock[F[_]: Sync: Log: Time: BlockStore: SynchronyConstraintChecker](
      dag: BlockDagRepresentation[F],
      genesis: BlockMessage,
      validatorIdentity: ValidatorIdentity,
      shardId: String,
      version: Long,
      expirationThreshold: Int,
      runtimeManager: RuntimeManager[F]
  )(
      implicit state: CasperStateCell[F],
      metricsF: Metrics[F],
      spanF: Span[F]
  ): F[CreateBlockStatus] =
    spanF.trace(CreateBlockMetricsSource) {
      import cats.instances.list._

      val validator = ByteString.copyFrom(validatorIdentity.publicKey.bytes)
      for {
        tipHashes             <- Estimator.tips(dag, genesis)
        _                     <- spanF.mark("after-estimator")
        parentMetadatas       <- EstimatorHelper.chooseNonConflicting(tipHashes, dag)
        maxBlockNumber        = ProtoUtil.maxBlockNumberMetadata(parentMetadatas)
        invalidLatestMessages <- ProtoUtil.invalidLatestMessages(dag)
        slashingDeploys <- invalidLatestMessages.values.toList.traverse { invalidBlockHash =>
                            val encodedInvalidBlockHash =
                              Base16.encode(invalidBlockHash.toByteArray)
                            // TODO: Do something useful with the result of "slash".
                            ConstructDeploy.sourceDeployNowF(
                              s"""
                               #new rl(`rho:registry:lookup`), deployerId(`rho:rchain:deployerId`), posCh in {
                               #  rl!(`rho:rchain:pos`, *posCh) |
                               #  for(@(_, PoS) <- posCh) {
                               #    @PoS!("slash", *deployerId, "$encodedInvalidBlockHash".hexToBytes(), "IGNOREFORNOW")
                               #  }
                               #}
                               #
                            """.stripMargin('#'),
                              phloPrice = 0,
                              sec = validatorIdentity.privateKey
                            )
                          }
        _ <- Cell[F, CasperState].modify { s =>
              s.copy(deployHistory = s.deployHistory ++ slashingDeploys)
            }
        _                <- updateDeployHistory(state, maxBlockNumber)
        deploys          <- extractDeploys(dag, parentMetadatas, maxBlockNumber, expirationThreshold)
        parents          <- parentMetadatas.toList.traverse(p => ProtoUtil.getBlock(p.blockHash))
        justifications   <- computeJustifications(dag, parents)
        now              <- Time[F].currentMillis
        invalidBlocksSet <- dag.invalidBlocks
        invalidBlocks    = invalidBlocksSet.map(block => (block.blockHash, block.sender)).toMap
        unsignedBlock <- if (deploys.nonEmpty) {
                          SynchronyConstraintChecker[F]
                            .check(dag, runtimeManager, genesis, validator)
                            .ifM(
                              processDeploysAndCreateBlock(
                                dag,
                                runtimeManager,
                                parents,
                                deploys,
                                justifications,
                                maxBlockNumber,
                                shardId,
                                version,
                                now,
                                invalidBlocks
                              ),
                              CreateBlockStatus.notEnoughNewBlocks.pure[F]
                            )
                        } else {
                          CreateBlockStatus.noNewDeploys.pure[F]
                        }
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

  /*
   * This mechanism is a first effort to make life of a deploying party easier.
   * Instead of expecting the user to guess the current block number we assume that
   * if no value is given (default: -1) rchain should try to deploy
   * with the current known max block number.
   *
   * TODO: Make more developer friendly by introducing Option instead of a magic number
   */
  private def updateDeployHistory[F[_]: Sync: Log: Time: BlockStore](
      state: CasperStateCell[F],
      maxBlockNumber: Long
  ): F[Unit] = {
    def updateDeployValidAfterBlock(deployData: DeployData, max: Long): DeployData =
      if (deployData.validAfterBlockNumber == -1)
        deployData.copy(validAfterBlockNumber = max)
      else
        deployData

    def updateDeployHistory(state: CasperState, max: Long): CasperState =
      state.copy(deployHistory = state.deployHistory.map(deployData => {
        updateDeployValidAfterBlock(deployData, max)
      }))

    state.modify(state => updateDeployHistory(state, maxBlockNumber))
  }

  // TODO: Remove no longer valid deploys here instead of with lastFinalizedBlock call
  private def extractDeploys[F[_]: Sync: Log: Time: BlockStore](
      dag: BlockDagRepresentation[F],
      parents: Seq[BlockMetadata],
      maxBlockNumber: Long,
      expirationThreshold: Int
  )(implicit state: CasperStateCell[F]): F[Seq[DeployData]] =
    for {
      state               <- state.read
      currentBlockNumber  = maxBlockNumber + 1
      earliestBlockNumber = currentBlockNumber - expirationThreshold
      deploys             = state.deployHistory
      validDeploys = deploys.filter(
        d => notFutureDeploy(currentBlockNumber, d) && notExpiredDeploy(earliestBlockNumber, d)
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

  private def processDeploysAndCreateBlock[F[_]: Sync: Log: BlockStore: Span](
      dag: BlockDagRepresentation[F],
      runtimeManager: RuntimeManager[F],
      parents: Seq[BlockMessage],
      deploys: Seq[DeployData],
      justifications: Seq[Justification],
      maxBlockNumber: Long,
      shardId: String,
      version: Long,
      now: Long,
      invalidBlocks: Map[BlockHash, Validator]
  ): F[CreateBlockStatus] =
    (for {
      result <- InterpreterUtil.computeDeploysCheckpoint(
                 parents,
                 deploys,
                 dag,
                 runtimeManager,
                 BlockData(now, maxBlockNumber + 1),
                 invalidBlocks
               )
      (preStateHash, postStateHash, processedDeploys) = result
      (internalErrors, persistableDeploys)            = processedDeploys.partition(_.status.isInternalError)
      _                                               <- logInternalErrors(internalErrors)
      newBonds                                        <- runtimeManager.computeBonds(postStateHash)
      block = createBlock(
        now,
        parents,
        justifications,
        maxBlockNumber,
        preStateHash,
        postStateHash,
        persistableDeploys,
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

  private def logInternalErrors[F[_]: Sync: Log](
      internalErrors: Seq[InternalProcessedDeploy]
  ): F[List[Unit]] = {
    import cats.instances.list._

    internalErrors.toList
      .traverse {
        case InternalProcessedDeploy(deploy, _, _, InternalErrors(errors)) =>
          val errorsMessage = errors.map(_.getMessage).mkString("\n")
          Log[F].error(
            s"Internal error encountered while processing deploy '$deploy': $errorsMessage"
          )
        case _ => ().pure[F]
      }
  }

  private def createBlock(
      now: Long,
      p: Seq[BlockMessage],
      justifications: Seq[Justification],
      maxBlockNumber: Long,
      preStateHash: StateHash,
      postStateHash: StateHash,
      persistableDeploys: Seq[InternalProcessedDeploy],
      newBonds: Seq[Bond],
      shardId: String,
      version: Long
  ): CreateBlockStatus = {
    val postState = RChainState(preStateHash, postStateHash, newBonds.toList, maxBlockNumber + 1)

    val body   = Body(postState, persistableDeploys.map(_.toProcessedDeploy).toList)
    val header = blockHeader(body, p.map(_.blockHash), version, now)
    val block  = unsignedBlockProto(body, header, justifications, shardId)
    CreateBlockStatus.created(block)
  }
}
