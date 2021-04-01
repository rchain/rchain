package coop.rchain.casper

import cats.Applicative
import cats.data.EitherT
import cats.effect.{Concurrent, Sync}
import cats.effect.concurrent.{Ref, Semaphore}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage._
import coop.rchain.blockstorage.dag.{BlockDagRepresentation, BlockDagStorage}
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.casper.protocol._
import coop.rchain.casper.syntax._
import coop.rchain.casper.util._
import coop.rchain.casper.util.ProtoUtil.{deploys, _}
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.casper.util.rholang._
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.catscontrib.BooleanF._
import coop.rchain.catscontrib.Catscontrib.ToBooleanF
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.metrics.implicits._
import coop.rchain.models.BlockHash._
import coop.rchain.models.{BlockMetadata, EquivocationRecord, NormalizerEnv}
import coop.rchain.models.Validator.Validator
import coop.rchain.shared._
import coop.rchain.blockstorage.casperbuffer.CasperBufferStorage
import coop.rchain.blockstorage.dag.BlockDagKeyValueStorage.LastFinalizedStorage
import coop.rchain.blockstorage.deploy.DeployStorage
import coop.rchain.casper.blocks.merger.MergingVertex
import coop.rchain.casper.engine.BlockRetriever
import coop.rchain.casper.finality.Finalizer
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.signatures.Signed
import coop.rchain.dag.DagOps

class MultiParentCasperImpl[F[_]: Sync: Concurrent: Log: Time: SafetyOracle: LastFinalizedBlockCalculator: BlockStore: BlockDagStorage: CommUtil: EventPublisher: Estimator: DeployStorage: BlockRetriever](
    validatorId: Option[ValidatorIdentity],
    // todo this should be read from chain, for now read from startup options
    casperShardConf: CasperShardConf,
    approvedBlock: BlockMessage
)(
    implicit casperBuffer: CasperBufferStorage[F],
    metricsF: Metrics[F],
    spanF: Span[F],
    runtimeManager: RuntimeManager[F]
) extends MultiParentCasper[F] {
  import MultiParentCasperImpl._

  implicit private val logSource: LogSource = LogSource(this.getClass)
  private[this] val syncF                   = Sync[F]

  // TODO: Extract hardcoded version from shard config
  private val version = 1L

  def getValidator: F[Option[ValidatorIdentity]] = validatorId.pure[F]

  def getVersion: F[Long] = version.pure[F]

  def getApprovedBlock: F[BlockMessage] = approvedBlock.pure[F]

  private def updateLastFinalizedBlock(
      newBlock: BlockMessage,
      targetView: Option[Validator]
  ): F[Unit] = {

    val finalizationEffect = (h: BlockHash) =>
      for {
        // add finalized block to storage
        _ <- BlockDagStorage[F].setFinalizedHash(targetView.getOrElse(), h)
        // remove deploys that are finalized, if any found in storage
        finalisedDeploys <- BlockStore[F].getUnsafe(h).map(_.body.deploys.map(_.deploy))
        deploysRemoved   <- DeployStorage[F].remove(finalisedDeploys)
        _ <- Log[F].info(
              s"Removed $deploysRemoved deploys from deploy history as we finalized block ${PrettyPrinter
                .buildString(h)}."
            )
      } yield ()

    (for {
      dag <- blockDag.flatMap(_.view(newBlock.header.parentsHashList.toSet))
      currLFB <- BlockDagStorage[F]
                  .getFinalizedHash(targetView.getOrElse())
                  .getOrElse(approvedBlock.blockHash, targetView)
      _ <- Finalizer
            .run(
              dag,
              currLFB,
              finalizationEffect,
              SafetyOracle[F].normalizedFaultTolerance,
              casperShardConf.faultToleranceThreshold
            )
            .flatMap { newLFBOpt =>
              newLFBOpt.traverse { newLFB =>
                LastFinalizedStorage[F].put(
                  validatorId.map(vid => ByteString.copyFrom(vid.publicKey.bytes)),
                  newLFB
                ) >>
                  EventPublisher[F]
                    .publish(
                      RChainEvent.blockFinalised(newLFB.base16String)
                    )
              }

            }
    } yield ())
      .whenA(
        newBlock.body.state.blockNumber % casperShardConf.finalizationRate == 0
      )
  }

  /**
    * Check if there are blocks in CasperBuffer available with all dependencies met.
    * @return First from the set of available blocks
    */
  override def getDependencyFreeFromBuffer: F[List[BlockMessage]] = {
    import cats.instances.list._
    for {
      pendants       <- CasperBufferStorage[F].getPendants
      pendantsStored <- pendants.toList.filterA(BlockStore[F].contains)
      depFreePendants <- pendantsStored.filterA { pendant =>
                          for {
                            pendantBlock   <- BlockStore[F].get(pendant)
                            justifications = pendantBlock.get.justifications
                            // If even one of justifications is not in DAG - block is not dependency free
                            missingDep <- justifications
                                           .map(_.latestBlockHash)
                                           .existsM(dagContains(_).not)
                          } yield !missingDep
                        }
      r <- depFreePendants.traverse(BlockStore[F].getUnsafe)
    } yield r
  }

  def dagContains(hash: BlockHash): F[Boolean] = blockDag.flatMap(_.contains(hash))

  def bufferContains(hash: BlockHash): F[Boolean] = casperBuffer.contains(hash)

  def contains(hash: BlockHash): F[Boolean] = bufferContains(hash) ||^ dagContains(hash)

  def deploy(d: Signed[DeployData]): F[Either[DeployError, DeployId]] = {
    import cats.instances.either._
    import coop.rchain.models.rholang.implicits._

    InterpreterUtil
      .mkTerm(d.data.term, NormalizerEnv(d))
      .bitraverse(
        err => DeployError.parsingError(s"Error in parsing term: \n$err").pure[F],
        _ => addDeploy(d)
      )
  }

  def addDeploy(deploy: Signed[DeployData]): F[DeployId] =
    for {
      _ <- DeployStorage[F].add(List(deploy))
      _ <- Log[F].info(s"Received ${PrettyPrinter.buildString(deploy)}")
    } yield deploy.sig

  def estimator(dag: BlockDagRepresentation[F]): F[IndexedSeq[BlockHash]] =
    Estimator[F].tips(dag, approvedBlock).map(_.tips)

  def lastFinalizedBlock(validatorOpt: Option[Validator]): F[BlockMessage] =
    LastFinalizedStorage[F]
      .getOrElse(approvedBlock.blockHash, validatorOpt)
      .flatMap(h => BlockStore[F].getUnsafe(h))

  def blockDag: F[BlockDagRepresentation[F]] =
    BlockDagStorage[F].getRepresentation

  def normalizedInitialFault(weights: Map[Validator, Long]): F[Float] =
    BlockDagStorage[F].accessEquivocationsTracker { tracker =>
      tracker.equivocationRecords.map { equivocations =>
        equivocations
          .map(_.equivocator)
          .flatMap(weights.get)
          .sum
          .toFloat / weightMapTotal(weights)
      }
    }

  def getRuntimeManager: F[RuntimeManager[F]] = syncF.pure(runtimeManager)

  def fetchDependencies: F[Unit] = {
    import cats.instances.list._
    for {
      pendants       <- casperBuffer.getPendants
      pendantsUnseen <- pendants.toList.filterA(BlockStore[F].contains(_).not)
      _ <- Log[F].debug(s"Requesting CasperBuffer pendant hashes, ${pendantsUnseen.size} items.") >>
            pendantsUnseen.toList.traverse_(
              dependency =>
                Log[F]
                  .debug(
                    s"Sending dependency ${PrettyPrinter.buildString(dependency)} to BlockRetriever"
                  ) >>
                  BlockRetriever[F].admitHash(
                    dependency,
                    admitHashReason = BlockRetriever.MissingDependencyRequested
                  )
            )
    } yield ()
  }

  override def getSnapshot(targetBlockOpt: Option[BlockMessage]): F[CasperSnapshot[F]] = {
    import cats.instances.list._

    def getOnChainState(
        b: BlockMessage
    )(implicit runtimeManager: RuntimeManager[F]): F[OnChainCasperState] =
      for {
        av <- runtimeManager.getActiveValidators(b.body.state.postStateHash)
        // bonds are available in block message, but please remember this is just a cache, source of truth is RSpace.
        bm          = b.body.state.bonds
        shardConfig = casperShardConf
      } yield OnChainCasperState(shardConfig, bm.map(v => v.validator -> v.stake).toMap, av)

    /**
      * After introducing block merge, all parents that share the same bonds map should be parents.
      * Parents that have different bonds maps are only one that cannot be merged in any way.
      */
    def calculateParents(dag: BlockDagRepresentation[F]): F[List[BlockMessage]] =
      for {
        r         <- Estimator[F].tips(dag, approvedBlock)
        (_, tips) = (r.lca, r.tips)
        parents <- for {
                    // For now main parent bonds map taken as a reference, but might be we want to pick a subset with equal
                    // bond maps that has biggest cumulative stake.
                    blocks  <- tips.toList.traverse(BlockStore[F].getUnsafe)
                    parents = blocks.filter(b => b.body.state.bonds == blocks.head.body.state.bonds)
                  } yield parents

      } yield parents

    /**
      * Justifications are latest messages from bonded validators.
      * If validator is not bonded - no reason to use its message as justification, as Casper does not care about
      * that validator
      */
    def calculateJustifications(
        dag: BlockDagRepresentation[F],
        onChainState: OnChainCasperState
    ): F[List[Justification]] =
      for {
        justifications <- {
          for {
            lms <- dag.latestMessages
            r = lms.toList
              .map {
                case (validator, blockMetadata) => Justification(validator, blockMetadata.blockHash)
              }
              .filter(j => onChainState.bondsMap.keySet.contains(j.validator))
          } yield r
        }
      } yield justifications

    for {
      dag <- BlockDagStorage[F].getRepresentation

      // read or calculate parents
      parents <- if (targetBlockOpt.isDefined) ProtoUtil.getParents(targetBlockOpt.get)
                else calculateParents(dag)

      /**
        * read on chain state from main parent
        *
        * On chain state can potentially be modified by quorum of validators or any other procedure.
        * As on chain state is atomic thing defining rules of a shard, it does not make sense to merge changes
        * introduced by different validators. There always should be a single source of truth,
        * which is parent with the most stake.
        */
      onChainState <- getOnChainState(parents.head)

      // read or calculate justifications
      justifications <- if (targetBlockOpt.isDefined) targetBlockOpt.get.justifications.pure[F]
                       else calculateJustifications(dag, onChainState)

      // calculate misc data required
      parentMetas <- parents.traverse(b => dag.lookupUnsafe(b.blockHash))
      maxBlockNum = ProtoUtil.maxBlockNumberMetadata(parentMetas)
      maxSeqNums  <- dag.latestMessages.map(m => m.map { case (k, v) => k -> v.seqNum })

      // read deploys that are still in scope of block created/replayed
      deploysInScope <- {
        val currentBlockNumber  = maxBlockNum + 1
        val earliestBlockNumber = currentBlockNumber - onChainState.shardConf.deployLifespan
        for {
          result <- DagOps
                     .bfTraverseF[F, BlockMetadata](parentMetas)(
                       b =>
                         ProtoUtil
                           .getParentMetadatasAboveBlockNumber(
                             b,
                             earliestBlockNumber,
                             dag
                           )
                     )
                     .foldLeftF(Set.empty[Signed[DeployData]]) { (deploys, blockMetadata) =>
                       for {
                         block        <- BlockStore[F].getUnsafe(blockMetadata.blockHash)
                         blockDeploys = ProtoUtil.deploys(block).map(_.deploy)
                       } yield deploys ++ blockDeploys
                     }
        } yield result
      }

      // get all invalid blocks known
      invalidBlocks <- dag.invalidBlocksMap

      // if snapshot is created for some target block - update finalization state for block creator
      _ <- targetBlockOpt.traverse(b => updateLastFinalizedBlock(b, b.sender.some))
      // get last finalized block for block producer or for local view
      lfb <- LastFinalizedStorage[F]
              .getOrElse(
                approvedBlock.blockHash,
                targetBlockOpt
                  .map(_.sender.some)
                  .getOrElse(
                    validatorId.map(id => ByteString.copyFrom(id.publicKey.bytes))
                  )
              )
    } yield CasperSnapshot(
      dag,
      lfb,
      //lca,
      //tips,
      parents,
      justifications.toSet,
      invalidBlocks,
      deploysInScope,
      maxBlockNum,
      maxSeqNums,
      onChainState
    )
  }

  override def validate(
      b: BlockMessage,
      s: CasperSnapshot[F]
  ): F[Either[BlockError, ValidBlock]] = {
    val validationStatus: EitherT[F, BlockError, ValidBlock] =
      for {
        _ <- EitherT(
              Validate
                .blockSummary(b, approvedBlock, s, casperShardConf.shardName, deployLifespan)
            )
        _ <- EitherT.liftF(Span[F].mark("post-validation-block-summary"))
        _ <- EitherT(
              InterpreterUtil
                .validateBlockCheckpoint(b, s, runtimeManager)
                .map {
                  case Left(ex)       => Left(ex)
                  case Right(Some(_)) => Right(BlockStatus.valid)
                  case Right(None)    => Left(BlockStatus.invalidTransaction)
                }
            )
        _ <- EitherT.liftF(Span[F].mark("transactions-validated"))
        _ <- EitherT(Validate.bondsCache(b, runtimeManager))
        _ <- EitherT.liftF(Span[F].mark("bonds-cache-validated"))
        _ <- EitherT(Validate.neglectedInvalidBlock(b, s))
        _ <- EitherT.liftF(Span[F].mark("neglected-invalid-block-validated"))
        _ <- EitherT(
              EquivocationDetector.checkNeglectedEquivocationsWithUpdate(b, s.dag, approvedBlock)
            )
        _      <- EitherT.liftF(Span[F].mark("neglected-equivocation-validated"))
        depDag <- EitherT.liftF(casperBuffer.toDoublyLinkedDag)
        status <- EitherT(EquivocationDetector.checkEquivocations(depDag, b, s.dag))
        _      <- EitherT.liftF(Span[F].mark("equivocation-validated"))
      } yield status

    Log[F].info(s"Validating block ${PrettyPrinter.buildString(b, short = true)}.") <*
      runtimeManager.getBlockIndexCache
        .get(
          MergingVertex(
            b.blockHash,
            ProtoUtil.postStateHash(b),
            ProtoUtil.preStateHash(b),
            ProtoUtil.deploys(b).toSet
          )
        ) >> validationStatus.value
  }

  override def handleValidBlock(block: BlockMessage): F[BlockDagRepresentation[F]] =
    for {
      updatedDag <- BlockDagStorage[F].insert(block, invalid = false)
      _          <- casperBuffer.remove(block.blockHash)
      // update local view on finalization after each new valida lock is added
      _ <- updateLastFinalizedBlock(
            block,
            validatorId.map(v => ByteString.copyFrom(v.publicKey.bytes))
          )
    } yield updatedDag

  override def handleInvalidBlock(
      block: BlockMessage,
      status: InvalidBlock,
      dag: BlockDagRepresentation[F]
  ): F[BlockDagRepresentation[F]] = {
    // TODO: Slash block for status except InvalidUnslashableBlock
    def handleInvalidBlockEffect(
        status: BlockError,
        block: BlockMessage
    ): F[BlockDagRepresentation[F]] =
      for {
        _ <- Log[F].warn(
              s"Recording invalid block ${PrettyPrinter.buildString(block.blockHash)} for ${status.toString}."
            )
        // TODO should be nice to have this transition of a block from casper buffer to dag storage atomic
        r <- BlockDagStorage[F].insert(block, invalid = true)
        _ <- CasperBufferStorage[F].remove(block.blockHash)
      } yield r

    status match {
      case InvalidBlock.AdmissibleEquivocation =>
        val baseEquivocationBlockSeqNum = block.seqNum - 1
        for {
          _ <- BlockDagStorage[F].accessEquivocationsTracker { tracker =>
                for {
                  equivocations <- tracker.equivocationRecords
                  _ <- syncF.unlessA(equivocations.exists {
                        case EquivocationRecord(validator, seqNum, _) =>
                          block.sender == validator && baseEquivocationBlockSeqNum == seqNum
                        // More than 2 equivocating children from base equivocation block and base block has already been recorded
                      }) {
                        val newEquivocationRecord =
                          EquivocationRecord(
                            block.sender,
                            baseEquivocationBlockSeqNum,
                            Set.empty[BlockHash]
                          )
                        tracker.insertEquivocationRecord(newEquivocationRecord)
                      }
                } yield ()
              }
          // We can only treat admissible equivocations as invalid blocks if
          // casper is single threaded.
          updatedDag <- handleInvalidBlockEffect(InvalidBlock.AdmissibleEquivocation, block)
        } yield updatedDag

      case InvalidBlock.IgnorableEquivocation =>
        /*
         * We don't have to include these blocks to the equivocation tracker because if any validator
         * will build off this side of the equivocation, we will get another attempt to add this block
         * through the admissible equivocations.
         */
        Log[F]
          .info(
            s"Did not add block ${PrettyPrinter.buildString(block.blockHash)} as that would add an equivocation to the BlockDAG"
          )
          .as(dag)

      case ib: InvalidBlock if InvalidBlock.isSlashable(ib) =>
        handleInvalidBlockEffect(ib, block)

      case ib: InvalidBlock =>
        CasperBufferStorage[F].remove(block.blockHash) >> Log[F]
          .warn(
            s"Recording invalid block ${PrettyPrinter.buildString(block.blockHash)} for $ib."
          )
          .as(dag)
    }
  }
}

object MultiParentCasperImpl {

  // TODO: Extract hardcoded deployLifespan from shard config
  // Size of deploy safety range.
  // Validators will try to put deploy in a block only for next `deployLifespan` blocks.
  // Required to enable protection from re-submitting duplicate deploys
  val deployLifespan = 50

  def addedEvent(block: BlockMessage): RChainEvent = {
    val (blockHash, parents, justifications, deployIds, creator, seqNum) = blockEvent(block)
    RChainEvent.blockAdded(
      blockHash,
      parents,
      justifications,
      deployIds,
      creator,
      seqNum
    )
  }

  def createdEvent(b: BlockMessage): RChainEvent = {
    val (blockHash, parents, justifications, deployIds, creator, seqNum) = blockEvent(b)
    RChainEvent.blockCreated(
      blockHash,
      parents,
      justifications,
      deployIds,
      creator,
      seqNum
    )
  }

  private def blockEvent(block: BlockMessage) = {

    val blockHash = block.blockHash.base16String
    val parentHashes =
      block.header.parentsHashList.map(_.base16String)
    val justificationHashes =
      block.justifications.toList
        .map(j => (j.validator.base16String, j.latestBlockHash.base16String))
    val deployIds: List[String] =
      block.body.deploys.map(pd => PrettyPrinter.buildStringNoLimit(pd.deploy.sig))
    val creator = block.sender.base16String
    val seqNum  = block.seqNum
    (blockHash, parentHashes, justificationHashes, deployIds, creator, seqNum)
  }
}
