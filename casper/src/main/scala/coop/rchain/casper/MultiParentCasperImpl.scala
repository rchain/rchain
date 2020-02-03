package coop.rchain.casper

import cats.data.EitherT
import cats.effect.{Concurrent, Sync}
import cats.effect.concurrent.{Ref, Semaphore}
import cats.syntax.all._
import coop.rchain.blockstorage._
import coop.rchain.blockstorage.dag.{BlockDagRepresentation, BlockDagStorage}
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.casper.CasperState.CasperStateCell
import coop.rchain.casper.protocol._
import coop.rchain.casper.util._
import coop.rchain.casper.util.ProtoUtil._
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.casper.util.rholang._
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.catscontrib.BooleanF._
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.metrics.implicits._
import coop.rchain.models.BlockHash._
import coop.rchain.models.{EquivocationRecord, NormalizerEnv}
import coop.rchain.models.Validator.Validator
import coop.rchain.shared._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.deploy.DeployStorage
import coop.rchain.blockstorage.finality.LastFinalizedStorage
import coop.rchain.crypto.signatures.Signed

/**
  Encapsulates mutable state of the MultiParentCasperImpl

  @param blockBuffer - holds hashes of blocks that were received but were not added because DAG does not have their parents yet
  @param dependencyDag - dependency dag for block buffer // TODO they should be one structure
  */
final case class CasperState(
    blockBuffer: Set[BlockHash] = Set.empty[BlockHash],
    dependencyDag: DoublyLinkedDag[BlockHash] = BlockDependencyDag.empty
)

object CasperState {
  type CasperStateCell[F[_]] = Cell[F, CasperState]
}

class MultiParentCasperImpl[F[_]: Sync: Concurrent: Log: Time: SafetyOracle: LastFinalizedBlockCalculator: BlockStore: BlockDagStorage: LastFinalizedStorage: CommUtil: EventPublisher: SynchronyConstraintChecker: LastFinalizedHeightConstraintChecker: Estimator: DeployStorage](
    validatorId: Option[ValidatorIdentity],
    genesis: BlockMessage,
    postGenesisStateHash: StateHash,
    shardId: String,
    finalizationRate: Int,
    blockProcessingLock: Semaphore[F]
)(
    implicit state: CasperStateCell[F],
    metricsF: Metrics[F],
    spanF: Span[F],
    runtimeManager: RuntimeManager[F]
) extends MultiParentCasper[F] {
  import MultiParentCasper.MetricsSource

  implicit private val logSource: LogSource = LogSource(this.getClass)
  private[this] val syncF                   = Sync[F]
  private[this] val noop                    = syncF.unit

  //TODO: Extract hardcoded version and expirationThreshold
  private val version             = 1L
  private val expirationThreshold = 50

  private[this] val AddBlockMetricsSource =
    Metrics.Source(CasperMetricsSource, "add-block")
  private[this] val CreateBlockMetricsSource =
    Metrics.Source(CasperMetricsSource, "create-block")

  def getGenesis: F[BlockMessage] = genesis.pure[F]

  def getValidator: F[ByteString] =
    validatorId match {
      case Some(validatorIdentity) =>
        ByteString.copyFrom(validatorIdentity.publicKey.bytes).pure[F]
      case None => ByteString.EMPTY.pure[F]
    }

  def addBlock(b: BlockMessage): F[ValidBlockProcessing] = {

    def logAlreadyProcessed: F[ValidBlockProcessing] =
      Log[F]
        .info(
          s"Block ${PrettyPrinter.buildString(b.blockHash)} has already been processed by another thread."
        )
        .as(BlockStatus.processing.asLeft)

    def add: F[ValidBlockProcessing] = spanF.trace(AddBlockMetricsSource) {
      for {
        _      <- BlockStore[F].put(b)
        _      <- spanF.mark("block-store-put")
        dag    <- blockDag
        status <- internalAddBlock(b, dag)
        _      <- spanF.mark("block-added-status")
      } yield status
    }

    import cats.instances.either._
    for {
      status <- blockProcessingLock.withPermit {
                 val exists = for {
                   dag         <- blockDag
                   cst         <- state.read
                   dagContains <- dag.contains(b.blockHash)
                 } yield dagContains || cst.blockBuffer.contains(b.blockHash)

                 exists.ifM(logAlreadyProcessed, add)
               }
      _ <- status.traverse_ { _ =>
            metricsF.setGauge("block-height", blockNumber(b))(AddBlockMetricsSource) >>
              EventPublisher[F].publish(MultiParentCasperImpl.addedEvent(b))
          }
    } yield status
  }.timer("add-block-time")

  private def updateLastFinalizedBlock(newBlock: BlockMessage): F[Unit] =
    lastFinalizedBlock.whenA(newBlock.body.state.blockNumber % finalizationRate == 0)

  private def internalAddBlock(
      b: BlockMessage,
      dag: BlockDagRepresentation[F]
  ): F[ValidBlockProcessing] =
    for {
      _            <- Span[F].mark("internal-add-block")
      validFormat  <- Validate.formatOfFields(b)
      validSig     <- Validate.blockSignature(b)
      validSender  <- Validate.blockSenderHasWeight(b, genesis, dag)
      validVersion <- Validate.version(b, version)
      attemptResult <- if (!validFormat) (BlockStatus.invalidFormat.asLeft[ValidBlock], dag).pure
                      else if (!validSig)
                        (BlockStatus.invalidSignature.asLeft[ValidBlock], dag).pure
                      else if (!validSender)
                        (BlockStatus.invalidSender.asLeft[ValidBlock], dag).pure
                      else if (!validVersion)
                        (BlockStatus.invalidVersion.asLeft[ValidBlock], dag).pure
                      else attemptAdd(b, dag)
      (attempt, updatedDag) = attemptResult
      _ <- attempt match {
            case Left(InvalidBlock.MissingBlocks) => noop
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
            case Left(ib: InvalidBlock) if !InvalidBlock.isSlashable(ib) => noop
            case Left(bs) if !BlockStatus.isInDag(bs)                    => noop
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
    Estimator[F].tips(dag, genesis)

  def createBlock: F[CreateBlockStatus] = spanF.trace(CreateBlockMetricsSource) {
    (validatorId match {
      case Some(validatorIdentity) =>
        BlockDagStorage[F].getRepresentation
          .flatMap { dag =>
            BlockCreator
              .createBlock(
                dag,
                genesis,
                validatorIdentity,
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
            case o: CreateBlockStatus => o.pure
          }
      case None => CreateBlockStatus.readOnlyMode.pure
    }).timer("create-block-time")
  }

  def lastFinalizedBlock: F[BlockMessage] =
    for {
      dag                    <- blockDag
      lastFinalizedBlockHash <- LastFinalizedStorage[F].get(genesis)
      updatedLastFinalizedBlockHash <- LastFinalizedBlockCalculator[F]
                                        .run(dag, lastFinalizedBlockHash)
      _ <- LastFinalizedStorage[F].put(updatedLastFinalizedBlockHash)
      _ <- EventPublisher[F]
            .publish(
              RChainEvent.blockFinalised(updatedLastFinalizedBlockHash.base16String)
            )
            .whenA(lastFinalizedBlockHash != updatedLastFinalizedBlockHash)
      blockMessage <- ProtoUtil.getBlock(updatedLastFinalizedBlockHash)
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
        _ <- EitherT(Validate.blockSummary(b, genesis, dag, shardId, expirationThreshold))
        _ <- EitherT.liftF(Span[F].mark("post-validation-block-summary"))
        _ <- EitherT(
              InterpreterUtil
                .validateBlockCheckpoint(b, dag, runtimeManager)
                .map {
                  case Left(ex)       => Left(ex)
                  case Right(Some(_)) => Right(BlockStatus.valid)
                  case Right(None)    => Left(BlockStatus.invalidTransaction)
                }
            )
        _      <- EitherT.liftF(Span[F].mark("transactions-validated"))
        _      <- EitherT(Validate.bondsCache(b, runtimeManager))
        _      <- EitherT.liftF(Span[F].mark("bonds-cache-validated"))
        _      <- EitherT(Validate.neglectedInvalidBlock(b, dag))
        _      <- EitherT.liftF(Span[F].mark("neglected-invalid-block-validated"))
        _      <- EitherT(EquivocationDetector.checkNeglectedEquivocationsWithUpdate(b, dag, genesis))
        _      <- EitherT.liftF(Span[F].mark("neglected-equivocation-validated"))
        state  <- EitherT.liftF(Cell[F, CasperState].read)
        status <- EitherT(EquivocationDetector.checkEquivocations(state.dependencyDag, b, dag))
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
        // Add successful! Send block hash to peers, log success, try to add other blocks
        for {
          updatedDag <- BlockDagStorage[F].insert(block, genesis, invalid = false)
          _          <- CommUtil[F].sendBlockHash(block.blockHash, block.sender)
          _ <- Log[F].info(
                s"Added ${PrettyPrinter.buildString(block.blockHash)}"
              )
          _ <- updateLastFinalizedBlock(block)
        } yield updatedDag
      }
      .leftMap {
        case InvalidBlock.MissingBlocks =>
          Cell[F, CasperState].modify { s =>
            s.copy(blockBuffer = s.blockBuffer + block.blockHash)
          } >> fetchMissingDependencies(block) >> dag.pure

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
            _          <- CommUtil[F].sendBlockHash(block.blockHash, block.sender)
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
          Log[F]
            .warn(
              s"Recording invalid block ${PrettyPrinter.buildString(block.blockHash)} for $ib."
            )
            .as(dag)

        case BlockError.Processing =>
          syncF.raiseError[BlockDagRepresentation[F]](
            new RuntimeException(s"A block should not be processing at this stage.")
          )

        case BlockError.BlockException(ex) =>
          Log[F]
            .error(s"Encountered exception in while processing block ${PrettyPrinter
              .buildString(block.blockHash)}: ${ex.getMessage}")
            .as(dag)

      }
      .merge

  private def fetchMissingDependencies(
      b: BlockMessage
  ): F[Unit] = {
    import cats.instances.list._
    for {
      dag                 <- blockDag
      missingDependencies <- dependenciesHashesOf(b).filterA(dag.lookup(_).map(_.isEmpty))
      missingUnseenDependencies <- missingDependencies.filterA(
                                    blockHash => ~^(BlockStore[F].contains(blockHash))
                                  )
      _ <- missingDependencies.traverse(addDependencyToDag(_, b))
      _ <- missingUnseenDependencies.traverse(CommUtil[F].sendBlockRequest)
    } yield ()
  }

  private def addDependencyToDag(hash: BlockHash, childBlock: BlockMessage): F[Unit] =
    Cell[F, CasperState].modify(
      s =>
        s.copy(
          dependencyDag = DoublyLinkedDagOperations.add(s.dependencyDag, hash, childBlock.blockHash)
        )
    )

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
  ): F[Unit] = {
    import cats.instances.list._
    type Attempt = (BlockMessage, (ValidBlockProcessing, BlockDagRepresentation[F]))
    for {
      _                    <- Span[F].mark("reattempt-buffer")
      state                <- Cell[F, CasperState].read
      dependencyFree       = state.dependencyDag.dependencyFree
      dependencyFreeBlocks = state.blockBuffer.intersect(dependencyFree).toList
      attemptsWithDag <- dependencyFreeBlocks.foldM((List.empty[Attempt], dag)) {
                          case ((attempts, updatedDag), blockHash) =>
                            for {
                              maybeBlock <- BlockStore[F].get(blockHash)
                              b <- syncF.fromOption(
                                    maybeBlock,
                                    new RuntimeException(
                                      s"Could not find a block for hash $blockHash in the blockstore. Exiting..."
                                    )
                                  )
                              status <- attemptAdd(b, updatedDag)
                            } yield ((b, status) :: attempts, status._2)
                        }
      (attempts, updatedDag) = attemptsWithDag
      _ <- syncF.whenA(attempts.nonEmpty) {
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
  }

  private def removeAdded(
      blockBufferDependencyDag: DoublyLinkedDag[BlockHash],
      addedBlocks: List[ByteString]
  ): F[Unit] = Cell[F, CasperState].modify { s =>
    s.copy(
      blockBuffer = s.blockBuffer -- addedBlocks,
      dependencyDag = addedBlocks.foldLeft(blockBufferDependencyDag) {
        case (acc, addedBlock) =>
          DoublyLinkedDagOperations.remove(acc, addedBlock)
      }
    )
  }

  def getRuntimeManager: F[RuntimeManager[F]] = syncF.pure(runtimeManager)

  def fetchDependencies: F[Unit] = {
    import cats.instances.list._
    for {
      s <- Cell[F, CasperState].read
      _ <- s.dependencyDag.dependencyFree.toList.traverse(CommUtil[F].sendBlockRequest)
    } yield ()
  }
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
    import cats.instances.list._
    import cats.instances.option._

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
