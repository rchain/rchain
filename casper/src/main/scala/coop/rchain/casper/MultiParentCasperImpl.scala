package coop.rchain.casper

import cats._
import cats.implicits._
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
import coop.rchain.catscontrib.{Capture, ListContrib}
import coop.rchain.comm.CommError.ErrorHandler
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.crypto.codec.Base16
import coop.rchain.shared._

/**
  Encapsulates mutable state of the MultiParentCasperImpl

  @param seenBlockHashes - tracks hashes of all blocks seen so far
  @param blockBuffer
  @param deployHistory
  @param invalidBlockTracker
  @param equivocationsTracker: Used to keep track of when other validators detect the equivocation consisting of the base block at the sequence number identified by the (validator, base equivocation sequence number) pair of each EquivocationRecord.
  */
final case class CasperState(
    seenBlockHashes: Set[BlockHash] = Set.empty[BlockHash],
    blockBuffer: Set[BlockMessage] = Set.empty[BlockMessage],
    deployHistory: Set[DeployData] = Set.empty[DeployData],
    invalidBlockTracker: Set[BlockHash] = Set.empty[BlockHash],
    dependencyDag: DoublyLinkedDag[BlockHash] = BlockDependencyDag.empty,
    equivocationsTracker: Set[EquivocationRecord] = Set.empty[EquivocationRecord]
)

class MultiParentCasperImpl[F[_]: Sync: Concurrent: Capture: ConnectionsCell: TransportLayer: Log: Time: ErrorHandler: SafetyOracle: BlockStore: RPConfAsk: BlockDagStorage](
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

  //TODO: Extract hardcoded version
  private val version = 1L

  private val emptyStateHash = runtimeManager.emptyStateHash

  // TODO: Extract hardcoded fault tolerance threshold
  private val faultToleranceThreshold         = 0f
  private val lastFinalizedBlockHashContainer = Ref.unsafe[F, BlockHash](genesis.blockHash)

  def addBlock(
      b: BlockMessage,
      handleDoppelganger: (BlockMessage, Validator) => F[Unit]
  ): F[BlockStatus] =
    Sync[F].bracket(blockProcessingLock.acquire)(
      _ =>
        for {
          dag            <- blockDag
          blockHash      = b.blockHash
          containsResult <- dag.contains(blockHash)
          s              <- Cell[F, CasperState].read
          result <- if (containsResult || s.seenBlockHashes.contains(blockHash)) {
                     Log[F]
                       .info(
                         s"Block ${PrettyPrinter.buildString(b.blockHash)} has already been processed by another thread."
                       )
                       .map(_ => BlockStatus.processing)
                   } else {
                     (validatorId match {
                       case Some(ValidatorIdentity(publicKey, _, _)) =>
                         val sender = ByteString.copyFrom(publicKey)
                         handleDoppelganger(b, sender)
                       case None => ().pure[F]
                     }) *> Cell[F, CasperState].modify { s =>
                       s.copy(seenBlockHashes = s.seenBlockHashes + b.blockHash)
                     } *> internalAddBlock(b, dag)
                   }
        } yield result
    )(_ => blockProcessingLock.release)

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
                  blockBuffer = s.blockBuffer - b,
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
      estimates                     <- estimator(updatedDag)
      tip                           = estimates.head
      _                             <- Log[F].info(s"New fork-choice tip is block ${PrettyPrinter.buildString(tip.blockHash)}.")
      lastFinalizedBlockHash        <- lastFinalizedBlockHashContainer.get
      updatedLastFinalizedBlockHash <- updateLastFinalizedBlock(updatedDag, lastFinalizedBlockHash)
      _                             <- lastFinalizedBlockHashContainer.set(updatedLastFinalizedBlockHash)
    } yield attempt

  private def updateLastFinalizedBlock(
      dag: BlockDagRepresentation[F],
      lastFinalizedBlockHash: BlockHash
  ): F[BlockHash] =
    for {
      childrenHashes <- dag
                         .children(lastFinalizedBlockHash)
                         .map(_.getOrElse(Set.empty[BlockHash]).toList)
      maybeFinalizedChild <- ListContrib.findM(
                              childrenHashes,
                              (blockHash: BlockHash) =>
                                isGreaterThanFaultToleranceThreshold(dag, blockHash)
                            )
      newFinalizedBlock <- maybeFinalizedChild match {
                            case Some(finalizedChild) =>
                              removeDeploysInFinalizedBlock(finalizedChild) *> updateLastFinalizedBlock(
                                dag,
                                finalizedChild
                              )
                            case None => lastFinalizedBlockHash.pure[F]
                          }
    } yield newFinalizedBlock

  private def removeDeploysInFinalizedBlock(finalizedChild: BlockHash): F[Unit] =
    for {
      b                  <- ProtoUtil.unsafeGetBlock[F](finalizedChild)
      deploys            = b.body.get.deploys.map(_.deploy.get).toList
      stateBefore        <- Cell[F, CasperState].read
      initialHistorySize = stateBefore.deployHistory.size
      _ <- Cell[F, CasperState].modify { s =>
            s.copy(deployHistory = s.deployHistory -- deploys)
          }

      stateAfter     <- Cell[F, CasperState].read
      deploysRemoved = initialHistorySize - stateAfter.deployHistory.size
      _ <- Log[F].info(
            s"Removed $deploysRemoved deploys from deploy history as we finalized block ${PrettyPrinter
              .buildString(finalizedChild)}."
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

  def contains(
      b: BlockMessage
  ): F[Boolean] =
    for {
      blockStoreContains <- BlockStore[F].contains(b.blockHash)
      state              <- Cell[F, CasperState].read
      bufferContains     = state.blockBuffer.exists(_.blockHash == b.blockHash)
    } yield (blockStoreContains || bufferContains)

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

  def estimator(dag: BlockDagRepresentation[F]): F[IndexedSeq[BlockMessage]] =
    for {
      rankedEstimates <- Estimator.tips[F](dag, genesis)
    } yield rankedEstimates.take(1)

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
      for {
        dag          <- blockDag
        orderedHeads <- estimator(dag)
        p            <- chooseNonConflicting[F](orderedHeads, dag)
        _ <- Log[F].info(
              s"${p.size} parents out of ${orderedHeads.size} latest blocks will be used."
            )
        r                <- remDeploys(dag, p)
        bondedValidators = bonds(p.head).map(_.validator).toSet
        //We ensure that only the justifications given in the block are those
        //which are bonded validators in the chosen parent. This is safe because
        //any latest message not from a bonded validator will not change the
        //final fork-choice.
        latestMessages <- dag.latestMessages
        justifications = toJustification(latestMessages)
          .filter(j => bondedValidators.contains(j.validator))
        proposal <- if (r.nonEmpty || p.length > 1) {
                     createProposal(dag, p, r, justifications)
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
      lastFinalizedBlockHash <- lastFinalizedBlockHashContainer.get
      blockMessage           <- ProtoUtil.unsafeGetBlock[F](lastFinalizedBlockHash)
    } yield blockMessage

  // TODO: Optimize for large number of deploys accumulated over history
  private def remDeploys(dag: BlockDagRepresentation[F], p: Seq[BlockMessage]): F[Seq[DeployData]] =
    for {
      state <- Cell[F, CasperState].read
      hist  = state.deployHistory
      d <- DagOperations
            .bfTraverseF[F, BlockMessage](p.toList)(ProtoUtil.unsafeGetParents[F])
            .map { b =>
              b.body
                .map(_.deploys.flatMap(_.deploy))
                .toSeq
                .flatten
            }
            .toList
      deploy = d.flatten
      result = hist -- deploy
    } yield (result.toSeq)

  private def createProposal(
      dag: BlockDagRepresentation[F],
      p: Seq[BlockMessage],
      r: Seq[DeployData],
      justifications: Seq[Justification]
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
                       val maxBlockNumber: Long =
                         p.foldLeft(-1L) {
                           case (acc, b) => math.max(acc, blockNumber(b))
                         }

                       runtimeManager
                         .computeBonds(postStateHash)
                         .map {
                           newBonds =>
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
                     })
               }
    } yield result

  def blockDag: F[BlockDagRepresentation[F]] =
    BlockDagStorage[F].getRepresentation

  def storageContents(hash: StateHash): F[String] =
    runtimeManager
      .storageRepr(hash)
      .map(_.getOrElse(s"Tuplespace hash ${Base16.encode(hash.toByteArray)} not found!"))

  def normalizedInitialFault(weights: Map[Validator, Long]): F[Float] =
    for {
      state   <- Cell[F, CasperState].read
      tracker = state.equivocationsTracker
    } yield
      (tracker
        .map(_.equivocator)
        .toSet
        .flatMap(weights.get)
        .sum
        .toFloat / weightMapTotal(weights))

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
                               .blockSummary[F](b, genesis, dag, shardId)
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
                                                s.invalidBlockTracker
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
          s.copy(blockBuffer = s.blockBuffer + block)
        } *> fetchMissingDependencies(block) *> dag.pure[F]
      case AdmissibleEquivocation =>
        val baseEquivocationBlockSeqNum = block.seqNum - 1
        for {
          _ <- Cell[F, CasperState].modify { s =>
                if (s.equivocationsTracker.exists {
                      case EquivocationRecord(validator, seqNum, _) =>
                        block.sender == validator && baseEquivocationBlockSeqNum == seqNum
                    }) {
                  // More than 2 equivocating children from base equivocation block and base block has already been recorded
                  s
                } else {
                  val newEquivocationRecord =
                    EquivocationRecord(
                      block.sender,
                      baseEquivocationBlockSeqNum,
                      Set.empty[BlockHash]
                    )
                  s.copy(equivocationsTracker = s.equivocationsTracker + newEquivocationRecord)
                }
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
      state <- Cell[F, CasperState].read
      missingUnseenDependencies = missingDependencies.filter(
        blockHash => !state.seenBlockHashes.contains(blockHash)
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
      _ <- Cell[F, CasperState].modify { s =>
            s.copy(invalidBlockTracker = s.invalidBlockTracker + block.blockHash)
          }
      updatedDag <- addToState(block)
    } yield updatedDag

  private def addToState(block: BlockMessage): F[BlockDagRepresentation[F]] =
    for {
      _          <- BlockStore[F].put(block.blockHash, block)
      updatedDag <- BlockDagStorage[F].insert(block)
    } yield updatedDag

  private def reAttemptBuffer(
      dag: BlockDagRepresentation[F]
  ): F[Unit] =
    for {
      state          <- Cell[F, CasperState].read
      dependencyFree = state.dependencyDag.dependencyFree
      dependencyFreeBlocks = state.blockBuffer
        .filter(block => dependencyFree.contains(block.blockHash))
        .toList
      attemptsWithDag <- dependencyFreeBlocks.foldM(
                          (
                            List.empty[(BlockMessage, (BlockStatus, BlockDagRepresentation[F]))],
                            dag
                          )
                        ) {
                          case ((attempts, updatedDag), b) =>
                            for {
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
    val addedBlocks = successfulAdds.map(_._1)
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
