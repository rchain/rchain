package coop.rchain.casper

import cats._
import cats.data.EitherT
import cats.effect.{Concurrent, Sync}
import cats.effect.concurrent.{Ref, Semaphore}
import cats.implicits._

import coop.rchain.blockstorage._
import coop.rchain.blockstorage.dag.{BlockDagRepresentation, BlockDagStorage}
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.casper.CasperState.CasperStateCell
import coop.rchain.casper.DeployError._
import coop.rchain.casper.engine.Running
import coop.rchain.casper.protocol._
import coop.rchain.casper.util._
import coop.rchain.casper.util.ProtoUtil._
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.casper.util.rholang._
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.catscontrib.BooleanF._
import coop.rchain.catscontrib.ski._
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.metrics.implicits._
import coop.rchain.models.BlockHash._
import coop.rchain.models.EquivocationRecord
import coop.rchain.models.Validator.Validator
import coop.rchain.rholang.interpreter.NormalizerEnv
import coop.rchain.shared._

import com.google.protobuf.ByteString

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

object CasperState {
  type CasperStateCell[F[_]] = Cell[F, CasperState]
}

class MultiParentCasperImpl[F[_]: Sync: Concurrent: ConnectionsCell: TransportLayer: Log: Time: SafetyOracle: LastFinalizedBlockCalculator: BlockStore: RPConfAsk: BlockDagStorage: Running.RequestedBlocks: EventPublisher: SynchronyConstraintChecker](
    validatorId: Option[ValidatorIdentity],
    genesis: BlockMessage,
    postGenesisStateHash: StateHash,
    shardId: String,
    blockProcessingLock: Semaphore[F]
)(
    implicit state: CasperStateCell[F],
    metricsF: Metrics[F],
    spanF: Span[F],
    runtimeManager: RuntimeManager[F]
) extends MultiParentCasper[F] {
  import MultiParentCasper.MetricsSource

  implicit private val logSource: LogSource = LogSource(this.getClass)

  //TODO: Extract hardcoded version and expirationThreshold
  private val version             = 1L
  private val expirationThreshold = 50

  private val lastFinalizedBlockHashContainer = Ref.unsafe[F, BlockHash](genesis.blockHash)

  private[this] val AddBlockMetricsSource =
    Metrics.Source(CasperMetricsSource, "add-block")

  def addBlock(
      b: BlockMessage,
      handleDoppelganger: (BlockMessage, Validator) => F[Unit]
  ): F[ValidBlockProcessing] = {

    def logAlreadyProcessed: F[ValidBlockProcessing] =
      Log[F]
        .info(
          s"Block ${PrettyPrinter.buildString(b.blockHash)} has already been processed by another thread."
        )
        .as(BlockStatus.processing.asLeft)

    def doppelgangerAndAdd: F[ValidBlockProcessing] = spanF.trace(AddBlockMetricsSource) {
      for {
        dag <- blockDag
        _ <- validatorId match {
              case Some(ValidatorIdentity(publicKey, _, _)) =>
                val sender = ByteString.copyFrom(publicKey.bytes)
                handleDoppelganger(b, sender)
              case None => ().pure[F]
            }
        _      <- BlockStore[F].put(b)
        _      <- spanF.mark("block-store-put")
        status <- internalAddBlock(b, dag)
        _      <- spanF.mark("block-added-status")
      } yield status
    }

    for {
      status <- blockProcessingLock.withPermit {
                 val exists = for {
                   dag         <- blockDag
                   cst         <- state.read
                   dagContains <- dag.contains(b.blockHash)
                 } yield dagContains || cst.blockBuffer.contains(b.blockHash)

                 exists.ifM(logAlreadyProcessed, doppelgangerAndAdd)
               }
      _ <- status.fold(
            kp(().pure[F]),
            kp(
              metricsF.setGauge("block-height", blockNumber(b))(AddBlockMetricsSource) >>
                EventPublisher[F].publish(MultiParentCasperImpl.addedEvent(b))
            )
          )
    } yield status
  }.timer("add-block-time")

  private def internalAddBlock(
      b: BlockMessage,
      dag: BlockDagRepresentation[F]
  ): F[ValidBlockProcessing] =
    for {
      _            <- Span[F].mark("internal-add-block")
      validFormat  <- Validate.formatOfFields[F](b)
      validSig     <- Validate.blockSignature[F](b)
      validSender  <- Validate.blockSender[F](b, genesis, dag)
      validVersion <- Validate.version[F](b, version)
      attemptResult <- if (!validFormat)
                        (BlockStatus.invalidFormat.asLeft[ValidBlock], dag).pure[F]
                      else if (!validSig)
                        (BlockStatus.invalidSignature.asLeft[ValidBlock], dag).pure[F]
                      else if (!validSender)
                        (BlockStatus.invalidSender.asLeft[ValidBlock], dag).pure[F]
                      else if (!validVersion)
                        (BlockStatus.invalidVersion.asLeft[ValidBlock], dag).pure[F]
                      else attemptAdd(b, dag)
      (attempt, updatedDag) = attemptResult
      _ <- attempt match {
            case Left(InvalidBlock.MissingBlocks) => ().pure[F]
            case _ =>
              Cell[F, CasperState].modify { s =>
                s.copy(
                  blockBuffer = s.blockBuffer - b.blockHash,
                  dependencyDag = DoublyLinkedDagOperations.remove(s.dependencyDag, b.blockHash)
                )
              }
          }
      _ <- Span[F].mark("attempt-result")
      _ <- attempt match {
            case Left(ib: InvalidBlock) if !InvalidBlock.isSlashable(ib) => ().pure[F]
            case Left(bs) if !BlockStatus.isInDag(bs)                    => ().pure[F]
            case _ =>
              reAttemptBuffer(updatedDag) // reAttempt for any status that resulted in the adding of the block into the view
          }
      _         <- Span[F].mark("reattempted-buffer")
      tipHashes <- estimator(updatedDag)
      _         <- Span[F].mark("after-estimator")
      tipHash   = tipHashes.head
      _         <- Log[F].info(s"New fork-choice tip is block ${PrettyPrinter.buildString(tipHash)}.")
    } yield attempt

  def contains(
      hash: BlockHash
  ): F[Boolean] =
    for {
      dag            <- blockDag
      dagContains    <- dag.contains(hash)
      state          <- Cell[F, CasperState].read
      bufferContains = state.blockBuffer.contains(hash)
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

  def deploy(d: DeployData): F[Either[DeployError, DeployId]] =
    validateDeploy(d).fold(
      _.asLeft[DeployId].pure[F],
      kp(
        InterpreterUtil
          .mkTerm(d.term, NormalizerEnv(DeployData.toProto(d)))
          .bitraverse(
            err => DeployError.parsingError(s"Error in parsing term: \n$err").pure[F],
            _ => addDeploy(d)
          )
      )
    )

  def addDeploy(deploy: DeployData): F[DeployId] =
    for {
      _ <- Cell[F, CasperState].modify { s =>
            s.copy(deployHistory = s.deployHistory + deploy)
          }
      _ <- Log[F].info(s"Received ${PrettyPrinter.buildString(deploy)}")
    } yield deploy.sig

  def estimator(dag: BlockDagRepresentation[F]): F[IndexedSeq[BlockHash]] =
    Estimator.tips[F](dag, genesis)

  def createBlock: F[CreateBlockStatus] =
    (validatorId match {
      case Some(ValidatorIdentity(publicKey, privateKey, sigAlgorithm)) =>
        BlockDagStorage[F].getRepresentation
          .flatMap { dag =>
            BlockCreator
              .createBlock(
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
          .flatMap {
            case c: Created =>
              EventPublisher[F]
                .publish(MultiParentCasperImpl.createdEvent(c))
                .as[CreateBlockStatus](c)
            case o: CreateBlockStatus => o.pure[F]
          }
      case None => CreateBlockStatus.readOnlyMode.pure[F]
    }).timer("create-block-time")

  def lastFinalizedBlock: F[BlockMessage] =
    for {
      dag                    <- blockDag
      lastFinalizedBlockHash <- lastFinalizedBlockHashContainer.get
      updatedLastFinalizedBlockHash <- LastFinalizedBlockCalculator[F]
                                        .run(dag, lastFinalizedBlockHash)
      _ <- lastFinalizedBlockHashContainer.set(updatedLastFinalizedBlockHash)
      _ <- EventPublisher[F]
            .publish(
              RChainEvent.blockFinalised(updatedLastFinalizedBlockHash.base16String)
            )
            .whenA(lastFinalizedBlockHash != updatedLastFinalizedBlockHash)
      blockMessage <- ProtoUtil.getBlock[F](updatedLastFinalizedBlockHash)
    } yield blockMessage

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

  /*
   * TODO: Pass in blockDag. We should only call _blockDag.get at one location.
   * This would require returning the updated block DAG with the block status.
   *
   * We want to catch equivocations only after we confirm that the block completing
   * the equivocation is otherwise valid.
   */
  private def attemptAdd(
      b: BlockMessage,
      dag: BlockDagRepresentation[F]
  ): F[(ValidBlockProcessing, BlockDagRepresentation[F])] = {
    val validationStatus: EitherT[F, BlockError, ValidBlock] =
      for {
        _ <- EitherT(
              Validate
                .blockSummary[F](b, genesis, dag, shardId, expirationThreshold)
            )
        _      <- EitherT.liftF(Span[F].mark("post-validation-block-summary"))
        _      <- EitherT(Validate.transactions[F](b, dag, runtimeManager))
        _      <- EitherT.liftF(Span[F].mark("transactions-validated"))
        _      <- EitherT(Validate.bondsCache[F](b, runtimeManager))
        _      <- EitherT.liftF(Span[F].mark("bonds-cache-validated"))
        _      <- EitherT(Validate.neglectedInvalidBlock[F](b, dag))
        _      <- EitherT.liftF(Span[F].mark("neglected-invalid-block-validated"))
        _      <- EitherT(EquivocationDetector.checkNeglectedEquivocationsWithUpdate[F](b, dag, genesis))
        _      <- EitherT.liftF(Span[F].mark("neglected-equivocation-validated"))
        state  <- EitherT.liftF(Cell[F, CasperState].read)
        status <- EitherT(EquivocationDetector.checkEquivocations[F](state.dependencyDag, b, dag))
        _      <- EitherT.liftF(Span[F].mark("equivocation-validated"))
      } yield status

    for {
      _          <- Span[F].mark("attempt-add")
      _          <- Log[F].info(s"Attempting to add Block ${PrettyPrinter.buildString(b.blockHash)} to DAG.")
      status     <- validationStatus.value
      updatedDag <- addEffects(status, b, dag)
      _          <- Span[F].mark("effects-added")
    } yield (status, updatedDag)
  }

  // TODO: Handle slashing
  private def addEffects(
      blockProcessingResult: ValidBlockProcessing,
      block: BlockMessage,
      dag: BlockDagRepresentation[F]
  ): F[BlockDagRepresentation[F]] =
    blockProcessingResult
      .map { _ =>
        // Add successful! Send block to peers, log success, try to add other blocks
        for {
          updatedDag <- BlockDagStorage[F].insert(block, genesis, invalid = false)
          _          <- CommUtil.sendBlock[F](block)
          _ <- Log[F].info(
                s"Added ${PrettyPrinter.buildString(block.blockHash)}"
              )
        } yield updatedDag
      }
      .leftMap {
        case InvalidBlock.MissingBlocks =>
          Cell[F, CasperState].modify { s =>
            s.copy(blockBuffer = s.blockBuffer + block.blockHash)
          } >> fetchMissingDependencies(block) >> dag.pure[F]

        case InvalidBlock.AdmissibleEquivocation =>
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
            // We can only treat admissible equivocations as invalid blocks if
            // casper is single threaded.
            updatedDag <- handleInvalidBlockEffect(InvalidBlock.AdmissibleEquivocation, block)
            _          <- CommUtil.sendBlock[F](block)
          } yield updatedDag

        case InvalidBlock.IgnorableEquivocation =>
          /*
           * We don't have to include these blocks to the equivocation tracker because if any validator
           * will build off this side of the equivocation, we will get another attempt to add this block
           * through the admissible equivocations.
           */
          Log[F].info(
            s"Did not add block ${PrettyPrinter.buildString(block.blockHash)} as that would add an equivocation to the BlockDAG"
          ) >> dag.pure[F]

        case ib: InvalidBlock if InvalidBlock.isSlashable(ib) =>
          handleInvalidBlockEffect(ib, block)

        case ib: InvalidBlock =>
          Log[F].warn(
            s"Recording invalid block ${PrettyPrinter.buildString(block.blockHash)} for $ib."
          ) >> dag.pure[F]

        case BlockError.Processing =>
          Sync[F].raiseError[BlockDagRepresentation[F]](
            new RuntimeException(s"A block should not be processing at this stage.")
          )

        case BlockError.BlockException(ex) =>
          Log[F].error(s"Encountered exception in while processing block ${PrettyPrinter
            .buildString(block.blockHash)}: ${ex.getMessage}") >> dag.pure[F]

      }
      .merge

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
    CommUtil.sendBlockRequest[F](hash)

  // TODO: Slash block for status except InvalidUnslashableBlock
  private def handleInvalidBlockEffect(
      status: BlockError,
      block: BlockMessage
  ): F[BlockDagRepresentation[F]] =
    Log[F].warn(
      s"Recording invalid block ${PrettyPrinter.buildString(block.blockHash)} for ${status.toString}."
    ) >> BlockDagStorage[F].insert(block, genesis, invalid = true)

  private def reAttemptBuffer(
      dag: BlockDagRepresentation[F]
  ): F[Unit] =
    for {
      _                    <- Span[F].mark("reattempt-buffer")
      state                <- Cell[F, CasperState].read
      dependencyFree       = state.dependencyDag.dependencyFree
      dependencyFreeBlocks = state.blockBuffer.intersect(dependencyFree).toList
      attemptsWithDag <- dependencyFreeBlocks.foldM(
                          (
                            List.empty[
                              (
                                  BlockMessage,
                                  (ValidBlockProcessing, BlockDagRepresentation[F])
                              )
                            ],
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
                              status <- attemptAdd(b, updatedDag)
                            } yield ((b, status) :: attempts, status._2)
                        }
      (attempts, updatedDag) = attemptsWithDag
      _ <- if (attempts.isEmpty) {
            ().pure[F]
          } else {
            val addedBlocks = attempts.collect {
              case (bm, (status, _)) if BlockStatus.isInDag(status.merge) => bm.blockHash
            }
            for {
              _ <- removeAdded(state.dependencyDag, addedBlocks)
              _ <- Span[F].mark("added-removed")
              _ <- reAttemptBuffer(updatedDag)
            } yield ()
          }
    } yield ()

  private def removeAdded(
      blockBufferDependencyDag: DoublyLinkedDag[BlockHash],
      addedBlocks: List[ByteString]
  ): F[Unit] =
    for {
      _ <- unsafeRemoveFromBlockBuffer(addedBlocks)
      _ <- removeFromBlockBufferDependencyDag(blockBufferDependencyDag, addedBlocks)
    } yield ()

  private def unsafeRemoveFromBlockBuffer(
      addedBlocks: List[ByteString]
  ): F[Unit] =
    Cell[F, CasperState].modify { s =>
      s.copy(blockBuffer = s.blockBuffer -- addedBlocks)
    }

  private def removeFromBlockBufferDependencyDag(
      blockBufferDependencyDag: DoublyLinkedDag[BlockHash],
      addedBlocks: List[ByteString]
  ): F[Unit] =
    Cell[F, CasperState].modify { s =>
      s.copy(dependencyDag = addedBlocks.foldLeft(blockBufferDependencyDag) {
        case (acc, addedBlock) =>
          DoublyLinkedDagOperations.remove(acc, addedBlock)
      })
    }

  def getRuntimeManager: F[RuntimeManager[F]] =
    Applicative[F].pure(runtimeManager)

  def fetchDependencies: F[Unit] =
    for {
      s <- Cell[F, CasperState].read
      _ <- s.dependencyDag.dependencyFree.toList.traverse(CommUtil.sendBlockRequest[F])
    } yield ()
}

object MultiParentCasperImpl {
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

  def createdEvent(cbs: Created): RChainEvent = {
    val (blockHash, parents, justifications, deployIds, creator, seqNum) = blockEvent(cbs.block)
    RChainEvent.blockAdded(
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
