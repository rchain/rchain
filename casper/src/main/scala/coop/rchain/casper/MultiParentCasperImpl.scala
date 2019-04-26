package coop.rchain.casper

import cats._
import coop.rchain.catscontrib.{BooleanF, Catscontrib}
import BooleanF._
import Catscontrib._
import cats.effect.concurrent.{Ref, Semaphore}
import cats.effect.{Concurrent, Sync}
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.{BlockDagRepresentation, BlockDagStorage, BlockStore}
import coop.rchain.casper.DeployError._
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil._
import coop.rchain.casper.util._
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.casper.util.rholang._
import coop.rchain.catscontrib.BooleanF._
import coop.rchain.catscontrib.ski._
import coop.rchain.comm.CommError.ErrorHandler
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.crypto.codec.Base16
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.EquivocationRecord
import coop.rchain.shared._

/**
  Encapsulates mutable state of the MultiParentCasperImpl

  @param blockBuffer - holds hashes of blocks that were received but were not added because DAG does not have their parents yet
  @param deployHistory - deploy data that will be eventually used to create block, removed when block holding it is finalizedchild
  @param dependencyDag - dependency dag for block buffer // TODO they should be one structure
  */
final case class CasperState(
    blockBuffer: Set[BlockHash] = Set.empty[BlockHash],
    deployHistory: Set[DeployData] = Set.empty[DeployData],
    dependencyDag: DoublyLinkedDag[BlockHash] = BlockDependencyDag.empty
)

class MultiParentCasperImpl[F[_]: Sync: Concurrent: Sync: ConnectionsCell: TransportLayer: Log: Time: ErrorHandler: SafetyOracle: BlockStore: RPConfAsk: BlockDagStorage](
    runtimeManager: RuntimeManager[F],
    validatorId: Option[ValidatorIdentity],
    genesis: BlockMessage,
    postGenesisStateHash: StateHash,
    shardId: String,
    blockProcessingLock: Semaphore[F]
)(implicit state: Cell[F, CasperState], metricsF: Metrics[F])
    extends MultiParentCasper[F] {

  implicit private val logSource: LogSource = LogSource(this.getClass)

  type Validator = ByteString

  //TODO: Extract hardcoded version and expirationThreshold
  private val version             = 1L
  private val expirationThreshold = 50

  private val emptyStateHash = runtimeManager.emptyStateHash

  private val lastFinalizedBlockHashContainer = Ref.unsafe[F, BlockHash](genesis.blockHash)

  private[this] val AddBlockMetricsSource =
    Metrics.Source(CasperMetricsSource, "add-block")

  def addBlock(
      b: BlockMessage,
      handleDoppelganger: (BlockMessage, Validator) => F[Unit]
  ): F[BlockStatus] = {

    def logAlreadyProcessed =
      Log[F]
        .info(
          s"Block ${PrettyPrinter.buildString(b.blockHash)} has already been processed by another thread."
        )
        .as(BlockStatus.processing)

    def doppelgangerAndAdd =
      for {
        span <- metricsF.span(AddBlockMetricsSource)
        dag  <- blockDag
        _ <- validatorId match {
              case Some(ValidatorIdentity(publicKey, _, _)) =>
                val sender = ByteString.copyFrom(publicKey.bytes)
                handleDoppelganger(b, sender)
              case None => ().pure[F]
            }
        _      <- BlockStore[F].put(b)
        _      <- span.mark("block-store-put")
        status <- internalAddBlock(b, dag, span)
        _      <- span.mark("block-added-status")
        _      <- span.close()
      } yield status

    Sync[F].bracket(blockProcessingLock.acquire)(kp {
      val exists = for {
        dag         <- blockDag
        cst         <- state.read
        dagContains <- dag.contains(b.blockHash)
      } yield dagContains || cst.blockBuffer.contains(b.blockHash)

      exists.ifM(logAlreadyProcessed, doppelgangerAndAdd)

    })(kp(blockProcessingLock.release))
  }

  private def internalAddBlock(
      b: BlockMessage,
      dag: BlockDagRepresentation[F],
      span: Span[F]
  ): F[BlockStatus] =
    for {
      _            <- span.mark("internal-add-block")
      validFormat  <- Validate.formatOfFields[F](b)
      validSig     <- Validate.blockSignature[F](b)
      validSender  <- Validate.blockSender[F](b, genesis, dag)
      validVersion <- Validate.version[F](b, version)
      attemptResult <- if (!validFormat) (InvalidUnslashableBlock, dag).pure[F]
                      else if (!validSig) (InvalidUnslashableBlock, dag).pure[F]
                      else if (!validSender) (InvalidUnslashableBlock, dag).pure[F]
                      else if (!validVersion) (InvalidUnslashableBlock, dag).pure[F]
                      else attemptAdd(b, dag, span)
      (attempt, updatedDag) = attemptResult
      _ <- attempt match {
            case MissingBlocks => ().pure[F]
            case _ =>
              Cell[F, CasperState].modify { s =>
                s.copy(
                  blockBuffer = s.blockBuffer - b.blockHash,
                  dependencyDag = DoublyLinkedDagOperations.remove(s.dependencyDag, b.blockHash)
                )
              }
          }
      _ <- span.mark("attempt-result")
      _ <- attempt match {
            case MissingBlocks           => ().pure[F]
            case IgnorableEquivocation   => ().pure[F]
            case InvalidUnslashableBlock => ().pure[F]
            case _ =>
              reAttemptBuffer(updatedDag, span) // reAttempt for any status that resulted in the adding of the block into the view
          }
      _         <- span.mark("reattempted-buffer")
      tipHashes <- estimator(updatedDag)
      _         <- span.mark("after-estimator")
      tipHash   = tipHashes.head
      _         <- Log[F].info(s"New fork-choice tip is block ${PrettyPrinter.buildString(tipHash)}.")
    } yield attempt

  def contains(
      b: BlockMessage
  ): F[Boolean] =
    for {
      dag            <- blockDag
      dagContains    <- dag.contains(b.blockHash)
      state          <- Cell[F, CasperState].read
      bufferContains = state.blockBuffer.contains(b.blockHash)
    } yield (dagContains || bufferContains)

  /**
    * @note The deployer vault balance is not verified here exclusively
    *       because any viable state-hash from which the vault balance can
    *       be computed is necessarily a post-state hash. A balance check only
    *       at deployment-time would introduce the following bug:
    *
    *       Suppose a deployer issues $n deployments, where each deployment
    *       costs $m phlo and each deployment is to be included in the next block.
    *       If the deployer's vault balance $b at the most recent post-state hash $s
    *       is greater than or equal to $m, and the deployer's account balance is
    *       computed at $s for all $n deployment, then deployment $i+1 would fail
    *       only after deployment $i was processed, resulting in $n * $m amount of
    *       total work performed. To allow users to issue > 1 deployment per block,
    *       vault balances must be calculated dynamically. Of course, we can always
    *       perform a preliminary balance check at deploy-time to catch the first
    *       deployment.
    * @param deployData
    * @return
    */
  private def validateDeploy(deployData: DeployData): Either[DeployError, Unit] = deployData match {
    case d if (d.sig == ByteString.EMPTY)      => missingSignature.asLeft[Unit]
    case d if (d.sigAlgorithm == "")           => missingSignatureAlgorithm.asLeft[Unit]
    case d if (d.deployer == ByteString.EMPTY) => missingUser.asLeft[Unit]
    case _ =>
      val maybeVerified = SignDeployment.verify(deployData)
      maybeVerified.fold(unknownSignatureAlgorithm(deployData.sigAlgorithm).asLeft[Unit]) {
        case false => signatureVerificationFailed.asLeft[Unit]
        case true  => ().asRight[DeployError]
      }
  }

  def deploy(d: DeployData): F[Either[DeployError, Unit]] =
    validateDeploy(d).fold(
      _.asLeft[Unit].pure[F],
      kp(
        InterpreterUtil
          .mkTerm(d.term)
          .bitraverse(
            err => DeployError.parsingError(s"Error in parsing term: \n$err").pure[F],
            _ => addDeploy(d)
          )
      )
    )

  def addDeploy(deploy: DeployData): F[Unit] =
    for {
      _ <- Cell[F, CasperState].modify { s =>
            s.copy(deployHistory = s.deployHistory + deploy)
          }
      _ <- Log[F].info(s"Received ${PrettyPrinter.buildString(deploy)}")
    } yield ()

  def estimator(dag: BlockDagRepresentation[F]): F[IndexedSeq[BlockHash]] =
    Estimator.tips[F](dag, genesis)

  def createBlock: F[CreateBlockStatus] = validatorId match {
    case Some(ValidatorIdentity(publicKey, privateKey, sigAlgorithm)) =>
      BlockDagStorage[F].getRepresentation.flatMap { dag =>
        BlockCreator.createBlock(
          dag,
          genesis,
          publicKey,
          privateKey,
          sigAlgorithm,
          shardId,
          version,
          expirationThreshold,
          runtimeManager
        )
      }
    case None => CreateBlockStatus.readOnlyMode.pure[F]
  }

  def lastFinalizedBlock: F[BlockMessage] =
    for {
      dag                    <- blockDag
      lastFinalizedBlockHash <- lastFinalizedBlockHashContainer.get
      updatedLastFinalizedBlockHash <- LastFinalizedBlockCalculator
                                        .run[F](dag, lastFinalizedBlockHash)
      _            <- lastFinalizedBlockHashContainer.set(updatedLastFinalizedBlockHash)
      blockMessage <- ProtoUtil.unsafeGetBlock[F](updatedLastFinalizedBlockHash)
    } yield blockMessage

  def blockDag: F[BlockDagRepresentation[F]] =
    BlockDagStorage[F].getRepresentation

  def storageContents(hash: StateHash): F[String] =
    runtimeManager
      .storageRepr(hash)
      .map(_.getOrElse(s"Tuplespace hash ${Base16.encode(hash.toByteArray)} not found!"))

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

  /*
   * TODO: Pass in blockDag. We should only call _blockDag.get at one location.
   * This would require returning the updated block DAG with the block status.
   *
   * We want to catch equivocations only after we confirm that the block completing
   * the equivocation is otherwise valid.
   */
  private def attemptAdd(
      b: BlockMessage,
      dag: BlockDagRepresentation[F],
      span: Span[F]
  ): F[(BlockStatus, BlockDagRepresentation[F])] =
    for {
      _ <- span.mark("attempt-add")
      _ <- Log[F].info(s"Attempting to add Block ${PrettyPrinter.buildString(b.blockHash)} to DAG.")
      postValidationStatus <- Validate
                               .blockSummary[F](b, genesis, dag, shardId, expirationThreshold, span)
      _ <- span.mark("post-validation-block-summary")
      postTransactionsCheckStatus <- postValidationStatus.traverse(
                                      _ =>
                                        Validate.transactions[F](
                                          b,
                                          dag,
                                          emptyStateHash,
                                          runtimeManager,
                                          span
                                        )
                                    )
      _ <- span.mark("transactions-validated")
      postBondsCacheStatus <- postTransactionsCheckStatus.joinRight.traverse(
                               _ => Validate.bondsCache[F](b, runtimeManager)
                             )
      _ <- span.mark("bonds-cache-validated")
      s <- Cell[F, CasperState].read
      postNeglectedInvalidBlockStatus <- postBondsCacheStatus.joinRight.traverse(
                                          _ =>
                                            Validate
                                              .neglectedInvalidBlock[F](
                                                b,
                                                dag
                                              )
                                        )
      _ <- span.mark("neglected-invalid-block-validated")
      postNeglectedEquivocationCheckStatus <- postNeglectedInvalidBlockStatus.joinRight
                                               .traverse(
                                                 _ =>
                                                   EquivocationDetector
                                                     .checkNeglectedEquivocationsWithUpdate[F](
                                                       b,
                                                       dag,
                                                       genesis
                                                     )
                                               )
      _ <- span.mark("neglected-equivocation-validated")
      postEquivocationCheckStatus <- postNeglectedEquivocationCheckStatus.joinRight.traverse(
                                      _ =>
                                        EquivocationDetector
                                          .checkEquivocations[F](s.dependencyDag, b, dag)
                                    )
      status     = postEquivocationCheckStatus.joinRight.merge
      _          <- span.mark("equivocation-validated")
      updatedDag <- addEffects(status, b, dag)
      _          <- span.mark("effects-added")
    } yield (status, updatedDag)

  @SuppressWarnings(Array("org.wartremover.warts.Throw")) // TODO remove throw
  // TODO: Handle slashing
  private def addEffects(
      status: BlockStatus,
      block: BlockMessage,
      dag: BlockDagRepresentation[F]
  ): F[BlockDagRepresentation[F]] =
    status match {
      case Valid =>
        // Add successful! Send block to peers, log success, try to add other blocks
        for {
          updatedDag <- BlockDagStorage[F].insert(block, genesis, invalid = false)
          _          <- CommUtil.sendBlock[F](block)
          _ <- Log[F].info(
                s"Added ${PrettyPrinter.buildString(block.blockHash)}"
              )
        } yield updatedDag
      case MissingBlocks =>
        Cell[F, CasperState].modify { s =>
          s.copy(blockBuffer = s.blockBuffer + block.blockHash)
        } *> fetchMissingDependencies(block) *> dag.pure[F]
      case AdmissibleEquivocation =>
        val baseEquivocationBlockSeqNum = block.seqNum - 1
        for {
          _ <- BlockDagStorage[F].accessEquivocationsTracker { tracker =>
                for {
                  equivocations <- tracker.equivocationRecords
                  _ <- if (equivocations.exists {
                             case EquivocationRecord(validator, seqNum, _) =>
                               block.sender == validator && baseEquivocationBlockSeqNum == seqNum
                           }) {
                        // More than 2 equivocating children from base equivocation block and base block has already been recorded
                        ().pure[F]
                      } else {
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
          updatedDag <- BlockDagStorage[F].insert(block, genesis, invalid = false)
          _          <- CommUtil.sendBlock[F](block)
          _ <- Log[F].info(
                s"Added admissible equivocation child block ${PrettyPrinter.buildString(block.blockHash)}"
              )
        } yield updatedDag
      case IgnorableEquivocation =>
        /*
         * We don't have to include these blocks to the equivocation tracker because if any validator
         * will build off this side of the equivocation, we will get another attempt to add this block
         * through the admissible equivocations.
         */
        Log[F].info(
          s"Did not add block ${PrettyPrinter.buildString(block.blockHash)} as that would add an equivocation to the BlockDAG"
        ) *> dag.pure[F]
      case InvalidUnslashableBlock =>
        handleInvalidBlockEffect(status, block)
      case InvalidFollows =>
        handleInvalidBlockEffect(status, block)
      case InvalidBlockNumber =>
        handleInvalidBlockEffect(status, block)
      case InvalidParents =>
        handleInvalidBlockEffect(status, block)
      case JustificationRegression =>
        handleInvalidBlockEffect(status, block)
      case InvalidSequenceNumber =>
        handleInvalidBlockEffect(status, block)
      case NeglectedInvalidBlock =>
        handleInvalidBlockEffect(status, block)
      case NeglectedEquivocation =>
        handleInvalidBlockEffect(status, block)
      case InvalidTransaction =>
        handleInvalidBlockEffect(status, block)
      case InvalidBondsCache =>
        handleInvalidBlockEffect(status, block)
      case InvalidRepeatDeploy =>
        handleInvalidBlockEffect(status, block)
      case InvalidShardId =>
        handleInvalidBlockEffect(status, block)
      case InvalidBlockHash =>
        handleInvalidBlockEffect(status, block)
      case InvalidDeployCount =>
        handleInvalidBlockEffect(status, block)
      case ContainsExpiredDeploy =>
        handleInvalidBlockEffect(status, block)
      case ContainsFutureDeploy =>
        handleInvalidBlockEffect(status, block)
      case Processing =>
        throw new RuntimeException(s"A block should not be processing at this stage.")
      case BlockException(ex) =>
        Log[F].error(s"Encountered exception in while processing block ${PrettyPrinter
          .buildString(block.blockHash)}: ${ex.getMessage}") *> dag.pure[F]
    }

  private def fetchMissingDependencies(
      b: BlockMessage
  ): F[Unit] =
    for {
      dag <- blockDag
      missingDependencies <- dependenciesHashesOf(b)
                              .filterA(
                                blockHash =>
                                  dag
                                    .lookup(blockHash)
                                    .map(_.isEmpty)
                              )
      missingUnseenDependencies <- missingDependencies.filterA(
                                    blockHash => ~^(BlockStore[F].contains(blockHash))
                                  )
      _ <- missingDependencies.traverse(hash => handleMissingDependency(hash, b))
      _ <- missingUnseenDependencies.traverse(hash => requestMissingDependency(hash))
    } yield ()

  private def handleMissingDependency(hash: BlockHash, childBlock: BlockMessage): F[Unit] =
    Cell[F, CasperState].modify(
      s =>
        s.copy(
          dependencyDag = DoublyLinkedDagOperations
            .add[BlockHash](s.dependencyDag, hash, childBlock.blockHash)
        )
    )

  private def requestMissingDependency(hash: BlockHash) =
    CommUtil.sendBlockRequest[F](BlockRequest(Base16.encode(hash.toByteArray), hash))

  // TODO: Slash block for status except InvalidUnslashableBlock
  private def handleInvalidBlockEffect(
      status: BlockStatus,
      block: BlockMessage
  ): F[BlockDagRepresentation[F]] =
    Log[F].warn(
      s"Recording invalid block ${PrettyPrinter.buildString(block.blockHash)} for ${status.toString}."
    ) >> BlockDagStorage[F].insert(block, genesis, invalid = true)

  private def reAttemptBuffer(
      dag: BlockDagRepresentation[F],
      span: Span[F]
  ): F[Unit] =
    for {
      _              <- span.mark("reattempt-buffer")
      state          <- Cell[F, CasperState].read
      dependencyFree = state.dependencyDag.dependencyFree
      dependencyFreeBlocks = state.blockBuffer
        .filter(blockHash => dependencyFree.contains(blockHash))
        .toList
      attemptsWithDag <- dependencyFreeBlocks.foldM(
                          (
                            List.empty[(BlockMessage, (BlockStatus, BlockDagRepresentation[F]))],
                            dag
                          )
                        ) {
                          case ((attempts, updatedDag), blockHash) =>
                            for {
                              maybeBlock <- BlockStore[F].get(blockHash)
                              b <- maybeBlock
                                    .map(_.pure[F])
                                    .getOrElse(
                                      Sync[F].raiseError(
                                        new RuntimeException(
                                          s"Could not find a block for hash $blockHash in the blockstore. Exiting..."
                                        )
                                      )
                                    )
                              status <- attemptAdd(b, updatedDag, span)
                            } yield ((b, status) :: attempts, status._2)
                        }
      (attempts, updatedDag) = attemptsWithDag
      _ <- if (attempts.isEmpty) {
            ().pure[F]
          } else {
            for {
              _ <- removeAdded(state.dependencyDag, attempts)
              _ <- span.mark("added-removed")
              _ <- reAttemptBuffer(updatedDag, span)
            } yield ()
          }
    } yield ()

  private def removeAdded(
      blockBufferDependencyDag: DoublyLinkedDag[BlockHash],
      attempts: List[(BlockMessage, (BlockStatus, BlockDagRepresentation[F]))]
  ): F[Unit] =
    for {
      successfulAdds <- attempts
                         .filter {
                           case (_, (status, _)) => status.inDag
                         }
                         .pure[F]
      _ <- unsafeRemoveFromBlockBuffer(successfulAdds)
      _ <- removeFromBlockBufferDependencyDag(blockBufferDependencyDag, successfulAdds)
    } yield ()

  private def unsafeRemoveFromBlockBuffer(
      successfulAdds: List[(BlockMessage, (BlockStatus, BlockDagRepresentation[F]))]
  ): F[Unit] = {
    val addedBlocks = successfulAdds.map(_._1.blockHash)
    Cell[F, CasperState].modify { s =>
      s.copy(blockBuffer = s.blockBuffer -- addedBlocks)
    }
  }

  private def removeFromBlockBufferDependencyDag(
      blockBufferDependencyDag: DoublyLinkedDag[BlockHash],
      successfulAdds: List[(BlockMessage, (BlockStatus, BlockDagRepresentation[F]))]
  ): F[Unit] =
    Cell[F, CasperState].modify { s =>
      s.copy(dependencyDag = successfulAdds.foldLeft(blockBufferDependencyDag) {
        case (acc, successfulAdd) =>
          DoublyLinkedDagOperations.remove(acc, successfulAdd._1.blockHash)
      })
    }

  def getRuntimeManager: F[Option[RuntimeManager[F]]] =
    Applicative[F].pure(Some(runtimeManager))

  def fetchDependencies: F[Unit] =
    for {
      s <- Cell[F, CasperState].read
      _ <- s.dependencyDag.dependencyFree.toList.traverse { hash =>
            CommUtil.sendBlockRequest[F](BlockRequest(Base16.encode(hash.toByteArray), hash))
          }
    } yield ()
}
