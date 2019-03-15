package coop.rchain.casper

import cats._
import cats.implicits._
import coop.rchain.catscontrib.ski._
import coop.rchain.catscontrib.{BooleanF, Catscontrib}, BooleanF._, Catscontrib._
import cats.effect.concurrent.{Ref, Semaphore}
import cats.effect.{Concurrent, Sync}
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.{BlockDagRepresentation, BlockDagStorage, BlockStore}
import coop.rchain.catscontrib._
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil._
import coop.rchain.casper.util._
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.casper.util.rholang._
import coop.rchain.catscontrib.ListContrib
import coop.rchain.comm.CommError.ErrorHandler
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.crypto.codec.Base16
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
)(implicit state: Cell[F, CasperState])
    extends MultiParentCasper[F] {

  private implicit val logSource: LogSource = LogSource(this.getClass)

  type Validator = ByteString

  //TODO: Extract hardcoded version and expirationThreshold
  private val version             = 1L
  private val expirationThreshold = 50

  private val emptyStateHash = runtimeManager.emptyStateHash

  private val lastFinalizedBlockHashContainer = Ref.unsafe[F, BlockHash](genesis.blockHash)

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
        dag <- blockDag
        _ <- validatorId match {
              case Some(ValidatorIdentity(publicKey, _, _)) =>
                val sender = ByteString.copyFrom(publicKey)
                handleDoppelganger(b, sender)
              case None => ().pure[F]
            }
        _      <- BlockStore[F].put(b)
        status <- internalAddBlock(b, dag)
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
      dag: BlockDagRepresentation[F]
  ): F[BlockStatus] =
    for {
      validFormat  <- Validate.formatOfFields[F](b)
      validSig     <- Validate.blockSignature[F](b)
      validSender  <- Validate.blockSender[F](b, genesis, dag)
      validVersion <- Validate.version[F](b, version)
      attemptResult <- if (!validFormat) (InvalidUnslashableBlock, dag).pure[F]
                      else if (!validSig) (InvalidUnslashableBlock, dag).pure[F]
                      else if (!validSender) (InvalidUnslashableBlock, dag).pure[F]
                      else if (!validVersion) (InvalidUnslashableBlock, dag).pure[F]
                      else attemptAdd(b, dag)
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
      _ <- attempt match {
            case MissingBlocks           => ().pure[F]
            case IgnorableEquivocation   => ().pure[F]
            case InvalidUnslashableBlock => ().pure[F]
            case _ =>
              reAttemptBuffer(updatedDag) // reAttempt for any status that resulted in the adding of the block into the view
          }
      tipHashes <- estimator(updatedDag)
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

  def deploy(d: DeployData): F[Either[Throwable, Unit]] =
    InterpreterUtil.mkTerm(d.term) match {
      case Right(_) =>
        addDeploy(d).as(Right(()))

      case Left(err) =>
        Applicative[F].pure(Left(new Exception(s"Error in parsing term: \n$err")))
    }

  def addDeploy(deploy: DeployData): F[Unit] =
    for {
      _ <- Cell[F, CasperState].modify { s =>
            s.copy(deployHistory = s.deployHistory + deploy)
          }
      _ <- Log[F].info(s"Received ${PrettyPrinter.buildString(deploy)}")
    } yield ()

  def estimator(dag: BlockDagRepresentation[F]): F[IndexedSeq[BlockHash]] =
    Estimator.tips[F](dag, genesis)

  /*
   * Logic:
   *  -Score each of the blockDAG heads extracted from the block messages via GHOST
   *  -Let P = subset of heads such that P contains no conflicts and the total score is maximized
   *  -Let R = subset of deploy messages which are not included in DAG obtained by following blocks in P
   *  -If R is non-empty then create a new block with parents equal to P and (non-conflicting) txns obtained from R
   *  -Else if R is empty and |P| > 1 then create a block with parents equal to P and no transactions
   *  -Else None
   *
   *  TODO: Make this return Either so that we get more information about why not block was
   *  produced (no deploys, already processing, no validator id)
   */
  def createBlock: F[CreateBlockStatus] = validatorId match {
    case Some(ValidatorIdentity(publicKey, privateKey, sigAlgorithm)) =>
      def updateDeployValidAfterBlock(deployData: DeployData, max: Long) =
        if (deployData.validAfterBlockNumber == -1)
          deployData.withValidAfterBlockNumber(max)
        else
          deployData

      def updateDeployHistory(state: CasperState, max: Long) =
        state.copy(deployHistory = state.deployHistory.map(deployData => {
          updateDeployValidAfterBlock(deployData, max)
        }))

      for {
        dag       <- blockDag
        tipHashes <- estimator(dag)
        p         <- chooseNonConflicting[F](tipHashes, dag)
        _ <- Log[F].info(
              s"${p.size} parents out of ${tipHashes.size} latest blocks will be used."
            )
        maxBlockNumber = ProtoUtil.maxBlockNumber(p)
        /*
         * This mechanism is a first effort to make life of a deploying party easier.
         * Instead of expecting the user to guess the current block number we assume that
         * if no value is given (default: -1) rchain should try to deploy
         * with the current known max block number.
         *
         * TODO make more developer friendly by introducing Option instead of a magic number
         */
        _                <- Cell[F, CasperState].modify(state => updateDeployHistory(state, maxBlockNumber))
        r                <- remDeploys(dag, p, maxBlockNumber)
        bondedValidators = bonds(p.head).map(_.validator).toSet
        //We ensure that only the justifications given in the block are those
        //which are bonded validators in the chosen parent. This is safe because
        //any latest message not from a bonded validator will not change the
        //final fork-choice.
        latestMessages <- dag.latestMessages
        justifications = toJustification(latestMessages)
          .filter(j => bondedValidators.contains(j.validator))
        proposal <- if (r.nonEmpty || p.length > 1) {
                     createProposal(dag, p, r, justifications, maxBlockNumber)
                   } else {
                     CreateBlockStatus.noNewDeploys.pure[F]
                   }
        signedBlock <- proposal.mapF(
                        signBlock(_, dag, publicKey, privateKey, sigAlgorithm, shardId)
                      )
      } yield signedBlock
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

  // TODO: Remove no longer valid deploys here instead of with lastFinalizedBlock call
  private def remDeploys(
      dag: BlockDagRepresentation[F],
      parents: Seq[BlockMessage],
      maxBlockNumber: Long
  ): F[Seq[DeployData]] =
    for {
      state               <- Cell[F, CasperState].read
      currentBlockNumber  = maxBlockNumber + 1
      earliestBlockNumber = currentBlockNumber - expirationThreshold
      deploys             = state.deployHistory
      validDeploys = deploys.filter(
        d => notFutureDeploy(currentBlockNumber, d) && notExpiredDeploy(earliestBlockNumber, d)
      )
      deploysInCurrentChain <- DagOperations
                                .bfTraverseF[F, BlockMessage](parents.toList)(
                                  b =>
                                    ProtoUtil
                                      .unsafeGetParentsAboveBlockNumber[F](b, earliestBlockNumber)
                                )
                                .map { b =>
                                  ProtoUtil.deploys(b).flatMap(_.deploy)
                                }
                                .toList
    } yield (validDeploys -- deploysInCurrentChain.flatten).toSeq

  private def notExpiredDeploy(earliestBlockNumber: Long, d: DeployData): Boolean =
    d.validAfterBlockNumber > earliestBlockNumber

  private def notFutureDeploy(currentBlockNumber: Long, d: DeployData): Boolean =
    d.validAfterBlockNumber < currentBlockNumber

  private def createProposal(
      dag: BlockDagRepresentation[F],
      p: Seq[BlockMessage],
      r: Seq[DeployData],
      justifications: Seq[Justification],
      maxBlockNumber: Long
  ): F[CreateBlockStatus] =
    for {
      now <- Time[F].currentMillis
      possibleProcessedDeploys <- InterpreterUtil.computeDeploysCheckpoint[F](
                                   p,
                                   r,
                                   dag,
                                   runtimeManager,
                                   Some(now)
                                 )
      result <- possibleProcessedDeploys match {
                 case Left(ex) =>
                   Log[F]
                     .error(
                       s"Critical error encountered while processing deploys: ${ex.getMessage}"
                     )
                     .map(_ => CreateBlockStatus.internalDeployError(ex))

                 case Right((preStateHash, postStateHash, processedDeploys)) =>
                   val (internalErrors, persistableDeploys) =
                     processedDeploys.partition(_.status.isInternalError)
                   internalErrors.toList
                     .traverse {
                       case InternalProcessedDeploy(deploy, _, _, InternalErrors(errors)) =>
                         val errorsMessage = errors.map(_.getMessage).mkString("\n")
                         Log[F].error(
                           s"Internal error encountered while processing deploy ${PrettyPrinter
                             .buildString(deploy)}: $errorsMessage"
                         )
                       case _ => ().pure[F]
                     }
                     .flatMap(_ => {
                       runtimeManager
                         .computeBonds(postStateHash)
                         .map { newBonds =>
                           createBlock(
                             now,
                             p,
                             justifications,
                             maxBlockNumber,
                             preStateHash,
                             postStateHash,
                             persistableDeploys,
                             newBonds
                           )
                         }
                     })
               }
    } yield result

  private def createBlock(
      now: Long,
      p: Seq[BlockMessage],
      justifications: Seq[Justification],
      maxBlockNumber: Long,
      preStateHash: StateHash,
      postStateHash: StateHash,
      persistableDeploys: Seq[InternalProcessedDeploy],
      newBonds: Seq[Bond]
  ): CreateBlockStatus = {
    val postState = RChainState()
      .withPreStateHash(preStateHash)
      .withPostStateHash(postStateHash)
      .withBonds(newBonds)
      .withBlockNumber(maxBlockNumber + 1)

    val body = Body()
      .withState(postState)
      .withDeploys(
        persistableDeploys.map(ProcessedDeployUtil.fromInternal)
      )
    val header = blockHeader(body, p.map(_.blockHash), version, now)
    val block  = unsignedBlockProto(body, header, justifications, shardId)
    CreateBlockStatus.created(block)
  }

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
      dag: BlockDagRepresentation[F]
  ): F[(BlockStatus, BlockDagRepresentation[F])] =
    for {
      _ <- Log[F].info(s"Attempting to add Block ${PrettyPrinter.buildString(b.blockHash)} to DAG.")
      postValidationStatus <- Validate
                               .blockSummary[F](b, genesis, dag, shardId, expirationThreshold)
      postTransactionsCheckStatus <- postValidationStatus.traverse(
                                      _ =>
                                        Validate.transactions[F](
                                          b,
                                          dag,
                                          emptyStateHash,
                                          runtimeManager
                                        )
                                    )
      postBondsCacheStatus <- postTransactionsCheckStatus.joinRight.traverse(
                               _ => Validate.bondsCache[F](b, runtimeManager)
                             )

      s <- Cell[F, CasperState].read
      postNeglectedInvalidBlockStatus <- postBondsCacheStatus.joinRight.traverse(
                                          _ =>
                                            Validate
                                              .neglectedInvalidBlock[F](
                                                b,
                                                dag
                                              )
                                        )
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
      postEquivocationCheckStatus <- postNeglectedEquivocationCheckStatus.joinRight.traverse(
                                      _ =>
                                        EquivocationDetector
                                          .checkEquivocations[F](s.dependencyDag, b, dag)
                                    )
      status     = postEquivocationCheckStatus.joinRight.merge
      updatedDag <- addEffects(status, b, dag)
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
          updatedDag <- addToState(block)
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
          updatedDag <- addToState(block)
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

  private def handleInvalidBlockEffect(
      status: BlockStatus,
      block: BlockMessage
  ): F[BlockDagRepresentation[F]] =
    for {
      _ <- Log[F].warn(
            s"Recording invalid block ${PrettyPrinter.buildString(block.blockHash)} for ${status.toString}."
          )
      // TODO: Slash block for status except InvalidUnslashableBlock
      _          <- BlockStore[F].put(block.blockHash, block)
      updatedDag <- BlockDagStorage[F].insert(block, true)
    } yield updatedDag

  // TODO: should only call insert on BLockDagStorage
  private def addToState(block: BlockMessage): F[BlockDagRepresentation[F]] =
    for {
      _          <- BlockStore[F].put(block.blockHash, block)
      updatedDag <- BlockDagStorage[F].insert(block, false)
    } yield updatedDag

  private def reAttemptBuffer(
      dag: BlockDagRepresentation[F]
  ): F[Unit] =
    for {
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
                              status <- attemptAdd(b, updatedDag)
                            } yield ((b, status) :: attempts, status._2)
                        }
      (attempts, updatedDag) = attemptsWithDag
      _ <- if (attempts.isEmpty) {
            ().pure[F]
          } else {
            for {
              _ <- removeAdded(state.dependencyDag, attempts)
              _ <- reAttemptBuffer(updatedDag)
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
