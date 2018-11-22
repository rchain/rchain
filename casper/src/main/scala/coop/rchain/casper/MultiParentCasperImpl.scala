package coop.rchain.casper

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.{Applicative, Monad}
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.{BlockMetadata, BlockStore}
import coop.rchain.blockstorage.util.TopologicalSortUtil
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
import monix.execution.Scheduler
import monix.execution.atomic.AtomicAny
import coop.rchain.shared.AttemptOps._
import coop.rchain.catscontrib.TaskContrib._

import scala.collection.immutable.HashSet
import scala.collection.mutable
import scala.concurrent.SyncVar

class MultiParentCasperImpl[F[_]: Sync: Capture: ConnectionsCell: TransportLayer: Log: Time: ErrorHandler: SafetyOracle: BlockStore: RPConfAsk](
    runtimeManager: RuntimeManager,
    validatorId: Option[ValidatorIdentity],
    genesis: BlockMessage,
    initialDag: BlockDag,
    postGenesisStateHash: StateHash,
    shardId: String
)(implicit scheduler: Scheduler)
    extends MultiParentCasper[F] {

  private implicit val logSource: LogSource = LogSource(this.getClass)

  type BlockHash = ByteString
  type Validator = ByteString

  //TODO: Extract hardcoded version
  private val version = 1L

  private val _blockDag: AtomicSyncVar[BlockDag] = new AtomicSyncVar(initialDag)

  private val emptyStateHash = runtimeManager.emptyStateHash

  private val blockBuffer: mutable.HashSet[BlockMessage] =
    new mutable.HashSet[BlockMessage]()
  private val blockBufferDependencyDagState =
    new AtomicMonadState[F, DoublyLinkedDag[BlockHash]](AtomicAny(BlockDependencyDag.empty))

  private val deployHist: mutable.HashSet[Deploy] = new mutable.HashSet[Deploy]()

  // Used to keep track of when other validators detect the equivocation consisting of the base block
  // at the sequence number identified by the (validator, base equivocation sequence number) pair of
  // each EquivocationRecord.
  private val equivocationsTracker: mutable.Set[EquivocationRecord] =
    new mutable.HashSet[EquivocationRecord]()
  private val invalidBlockTracker: mutable.HashSet[BlockHash] =
    new mutable.HashSet[BlockHash]()

  // TODO: Extract hardcoded fault tolerance threshold
  private val faultToleranceThreshold         = 0f
  private val lastFinalizedBlockHashContainer = Ref.unsafe[F, BlockHash](genesis.blockHash)

  private val processingBlock          = new SyncVar[Unit]()
  private val PROCESSING_BLOCK_TIMEOUT = 5 * 60 * 1000L
  processingBlock.put(())

  def addBlock(b: BlockMessage): F[BlockStatus] =
    for {
      _         <- Sync[F].delay(processingBlock.take(PROCESSING_BLOCK_TIMEOUT))
      dag       = _blockDag.get
      blockHash = b.blockHash
      result <- if (dag.dataLookup.contains(blockHash) || blockBuffer.exists(
                      _.blockHash == blockHash
                    )) {
                 Log[F]
                   .info(
                     s"Block ${PrettyPrinter.buildString(b.blockHash)} has already been processed by another thread."
                   )
                   .map(_ => BlockStatus.processing)
               } else {
                 internalAddBlock(b)
               }
      _ <- Sync[F].delay(processingBlock.put(()))
    } yield result

  def internalAddBlock(b: BlockMessage): F[BlockStatus] =
    for {
      validFormat  <- Validate.formatOfFields[F](b)
      validSig     <- Validate.blockSignature[F](b)
      dag          <- blockDag
      validSender  <- Validate.blockSender[F](b, genesis, dag)
      validVersion <- Validate.version[F](b, version)
      attempt <- if (!validFormat) InvalidUnslashableBlock.pure[F]
                else if (!validSig) InvalidUnslashableBlock.pure[F]
                else if (!validSender) InvalidUnslashableBlock.pure[F]
                else if (!validVersion) InvalidUnslashableBlock.pure[F]
                else attemptAdd(b)
      _ <- attempt match {
            case MissingBlocks => ().pure[F]
            case _ =>
              Capture[F].capture { blockBuffer -= b } *> blockBufferDependencyDagState.modify(
                blockBufferDependencyDag =>
                  DoublyLinkedDagOperations.remove(blockBufferDependencyDag, b.blockHash)
              )
          }
      _ <- attempt match {
            case MissingBlocks           => ().pure[F]
            case IgnorableEquivocation   => ().pure[F]
            case InvalidUnslashableBlock => ().pure[F]
            case _ =>
              reAttemptBuffer // reAttempt for any status that resulted in the adding of the block into the view
          }
      estimates                     <- estimator(dag)
      tip                           = estimates.head
      _                             <- Log[F].info(s"New fork-choice tip is block ${PrettyPrinter.buildString(tip.blockHash)}.")
      lastFinalizedBlockHash        <- lastFinalizedBlockHashContainer.get
      updatedLastFinalizedBlockHash <- updateLastFinalizedBlock(dag, lastFinalizedBlockHash)
      _                             <- lastFinalizedBlockHashContainer.set(updatedLastFinalizedBlockHash)
    } yield attempt

  private def updateLastFinalizedBlock(
      dag: BlockDag,
      lastFinalizedBlockHash: BlockHash
  ): F[BlockHash] =
    for {
      childrenHashes <- dag.childMap
                         .getOrElse(lastFinalizedBlockHash, Set.empty[BlockHash])
                         .toList
                         .pure[F]
      maybeFinalizedChild <- ListContrib.findM(
                              childrenHashes,
                              (blockHash: BlockHash) =>
                                isGreaterThanFaultToleranceThreshold(dag, blockHash)
                            )
      newFinalizedBlock <- maybeFinalizedChild match {
                            case Some(finalizedChild) =>
                              updateLastFinalizedBlock(dag, finalizedChild)
                            case None => lastFinalizedBlockHash.pure[F]
                          }
    } yield newFinalizedBlock

  private def isGreaterThanFaultToleranceThreshold(
      dag: BlockDag,
      blockHash: BlockHash
  ): F[Boolean] =
    (SafetyOracle[F].normalizedFaultTolerance(dag, blockHash) > faultToleranceThreshold).pure[F]

  def contains(b: BlockMessage): F[Boolean] =
    BlockStore[F].contains(b.blockHash).map(_ || blockBuffer.contains(b))

  def deploy(deploy: Deploy): F[Unit] =
    for {
      _ <- Sync[F].delay {
            deployHist += deploy
          }
      _ <- Log[F].info(s"Received ${PrettyPrinter.buildString(deploy)}")
    } yield ()

  def deploy(d: DeployData): F[Either[Throwable, Unit]] =
    InterpreterUtil.mkTerm(d.term) match {
      case Right(term) =>
        deploy(
          Deploy(
            term = Some(term),
            raw = Some(d)
          )
        ).as(Right(()))

      case Left(err) =>
        Applicative[F].pure(Left(new Exception(s"Error in parsing term: \n$err")))
    }

  def estimator(dag: BlockDag): F[IndexedSeq[BlockMessage]] =
    for {
      lastFinalizedBlockHash <- lastFinalizedBlockHashContainer.get
      rankedEstimates        <- Estimator.tips[F](dag, lastFinalizedBlockHash)
    } yield rankedEstimates

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
    case Some(vId @ ValidatorIdentity(publicKey, privateKey, sigAlgorithm)) =>
      for {
        dag              <- blockDag
        orderedHeads     <- estimator(dag)
        p                <- chooseNonConflicting[F](orderedHeads, genesis, dag)
        r                <- remDeploys(dag, p)
        bondedValidators = bonds(p.head).map(_.validator).toSet
        //We ensure that only the justifications given in the block are those
        //which are bonded validators in the chosen parent. This is safe because
        //any latest message not from a bonded validator will not change the
        //final fork-choice.
        justifications = toJustification(dag.latestMessages)
          .filter(j => bondedValidators.contains(j.validator))
        proposal <- if (r.nonEmpty || p.length > 1) {
                     createProposal(dag, p, r, justifications)
                   } else {
                     CreateBlockStatus.noNewDeploys.pure[F]
                   }
        signedBlock = proposal.map(
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
  private def remDeploys(dag: BlockDag, p: Seq[BlockMessage]): F[Seq[Deploy]] =
    for {
      result <- Capture[F].capture { deployHist.clone() }
      _ <- DagOperations
            .bfTraverseF[F, BlockMessage](p.toList)(ProtoUtil.unsafeGetParents[F])
            .foreach(
              b =>
                Capture[F].capture {
                  b.body.foreach(_.deploys.flatMap(_.deploy).foreach(result -= _))
                }
            )
    } yield result.toSeq

  private def createProposal(
      dag: BlockDag,
      p: Seq[BlockMessage],
      r: Seq[Deploy],
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
                     .map(_ => {
                       val maxBlockNumber: Long =
                         p.foldLeft(-1L) {
                           case (acc, b) => math.max(acc, blockNumber(b))
                         }

                       val newBonds = runtimeManager.computeBonds(postStateHash)
                       val postState = RChainState()
                         .withPreStateHash(preStateHash)
                         .withPostStateHash(postStateHash)
                         .withBonds(newBonds)
                         .withBlockNumber(maxBlockNumber + 1)

                       val body = Body()
                         .withState(postState)
                         .withDeploys(persistableDeploys.map(ProcessedDeployUtil.fromInternal))
                       val header = blockHeader(body, p.map(_.blockHash), version, now)
                       val block  = unsignedBlockProto(body, header, justifications, shardId)
                       CreateBlockStatus.created(block)
                     })
               }
    } yield result

  def blockDag: F[BlockDag] = Capture[F].capture {
    _blockDag.get
  }

  def storageContents(hash: StateHash): F[String] =
    runtimeManager
      .storageRepr(hash)
      .getOrElse(s"Tuplespace hash ${Base16.encode(hash.toByteArray)} not found!")
      .pure[F]

  def normalizedInitialFault(weights: Map[Validator, Long]): F[Float] =
    (equivocationsTracker
      .map(_.equivocator)
      .toSet
      .flatMap(weights.get)
      .sum
      .toFloat / weightMapTotal(weights))
      .pure[F]

  /*
   * TODO: Pass in blockDag. We should only call _blockDag.get at one location.
   * This would require returning the updated block DAG with the block status.
   *
   * We want to catch equivocations only after we confirm that the block completing
   * the equivocation is otherwise valid.
   */
  private def attemptAdd(b: BlockMessage): F[BlockStatus] =
    for {
      _                    <- Log[F].info(s"Attempting to add Block ${PrettyPrinter.buildString(b.blockHash)} to DAG.")
      dag                  <- Capture[F].capture { _blockDag.get }
      postValidationStatus <- Validate.blockSummary[F](b, genesis, dag, shardId)
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
      postNeglectedInvalidBlockStatus <- postBondsCacheStatus.joinRight.traverse(
                                          _ =>
                                            Validate
                                              .neglectedInvalidBlock[F](
                                                b,
                                                invalidBlockTracker.toSet
                                              )
                                        )
      postNeglectedEquivocationCheckStatus <- postNeglectedInvalidBlockStatus.joinRight
                                               .traverse(
                                                 _ =>
                                                   EquivocationDetector
                                                     .checkNeglectedEquivocationsWithUpdate[F](
                                                       equivocationsTracker,
                                                       b,
                                                       dag,
                                                       genesis
                                                     )
                                               )
      blockBufferDependencyDag <- blockBufferDependencyDagState.get
      postEquivocationCheckStatus <- postNeglectedEquivocationCheckStatus.joinRight.traverse(
                                      _ =>
                                        EquivocationDetector
                                          .checkEquivocations[F](blockBufferDependencyDag, b, dag)
                                    )
      status = postEquivocationCheckStatus.joinRight.merge
      _      <- addEffects(status, b)
    } yield status

  // TODO: Handle slashing
  private def addEffects(status: BlockStatus, block: BlockMessage): F[Unit] =
    status match {
      //Add successful! Send block to peers, log success, try to add other blocks
      case Valid =>
        addToState(block) *> CommUtil.sendBlock[F](block) *> Log[F].info(
          s"Added ${PrettyPrinter.buildString(block.blockHash)}"
        )
      case MissingBlocks =>
        Capture[F].capture { blockBuffer += block } *> fetchMissingDependencies(block)
      case AdmissibleEquivocation =>
        Capture[F].capture {
          val baseEquivocationBlockSeqNum = block.seqNum - 1
          if (equivocationsTracker.exists {
                case EquivocationRecord(validator, seqNum, _) =>
                  block.sender == validator && baseEquivocationBlockSeqNum == seqNum
              }) {
            // More than 2 equivocating children from base equivocation block and base block has already been recorded
          } else {
            val newEquivocationRecord =
              EquivocationRecord(block.sender, baseEquivocationBlockSeqNum, Set.empty[BlockHash])
            equivocationsTracker.add(newEquivocationRecord)
          }
        } *>
          addToState(block) *> CommUtil.sendBlock[F](block) *> Log[F].info(
          s"Added admissible equivocation child block ${PrettyPrinter.buildString(block.blockHash)}"
        )
      case IgnorableEquivocation =>
        /*
         * We don't have to include these blocks to the equivocation tracker because if any validator
         * will build off this side of the equivocation, we will get another attempt to add this block
         * through the admissible equivocations.
         */
        Log[F].info(
          s"Did not add block ${PrettyPrinter.buildString(block.blockHash)} as that would add an equivocation to the BlockDAG"
        )
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
          .buildString(block.blockHash)}: ${ex.getMessage}")
    }

  private def fetchMissingDependencies(b: BlockMessage): F[Unit] =
    for {
      dag            <- blockDag
      missingParents = parentHashes(b).toSet
      missingJustifications = b.justifications
        .map(_.latestBlockHash)
        .toSet
      allDependencies = (missingParents union missingJustifications).toList
      missingDependencies = allDependencies.filterNot(
        blockHash =>
          dag.dataLookup.contains(blockHash) || blockBuffer.exists(_.blockHash == blockHash)
      )
      _ <- missingDependencies.traverse(hash => handleMissingDependency(hash, b))
    } yield ()

  private def handleMissingDependency(hash: BlockHash, parentBlock: BlockMessage): F[Unit] =
    for {
      _ <- blockBufferDependencyDagState.modify(
            blockBufferDependencyDag =>
              DoublyLinkedDagOperations
                .add[BlockHash](blockBufferDependencyDag, hash, parentBlock.blockHash)
          )
      _ <- CommUtil.sendBlockRequest[F](BlockRequest(Base16.encode(hash.toByteArray), hash))
    } yield ()

  private def handleInvalidBlockEffect(status: BlockStatus, block: BlockMessage): F[Unit] =
    for {
      _ <- Log[F].warn(
            s"Recording invalid block ${PrettyPrinter.buildString(block.blockHash)} for ${status.toString}."
          )
      // TODO: Slash block for status except InvalidUnslashableBlock
      _ <- Capture[F].capture(invalidBlockTracker += block.blockHash) *> addToState(block)
    } yield ()

  private def addToState(block: BlockMessage): F[Unit] =
    BlockStore[F].put {
      _blockDag.update(bd => {
        val hash = block.blockHash

        val updatedSort   = TopologicalSortUtil.update(bd.topoSort, bd.sortOffset, block)
        val updatedLookup = bd.dataLookup.updated(block.blockHash, BlockMetadata.fromBlock(block))

        //add current block as new child to each of its parents
        val newChildMap = parentHashes(block).foldLeft(bd.childMap) {
          case (acc, p) =>
            val currChildren = acc.getOrElse(p, HashSet.empty[BlockHash])
            acc.updated(p, currChildren + hash)
        }

        //Block which contains newly bonded validators will not
        //have those validators in its justification
        val newValidators =
          bonds(block).map(_.validator).toSet.diff(block.justifications.map(_.validator).toSet)
        val newLatestMessages = newValidators.foldLeft(
          //update creator of the block
          bd.latestMessages.updated(block.sender, block)
        ) {
          //Update new validators with block in which
          //they were bonded (i.e. this block)
          case (acc, v) => acc.updated(v, block)
        }

        val lmJustifications = toLatestMessageHashes(block.justifications)
        //ditto for latestMessagesOfLatestMessages
        val newLatestLatestMessages = newValidators.foldLeft(
          bd.latestMessagesOfLatestMessages.updated(block.sender, lmJustifications)
        ) {
          case (acc, v) => acc.updated(v, lmJustifications)
        }

        bd.copy(
          //Assume that a non-equivocating validator must include
          //its own latest message in the justification. Therefore,
          //for a given validator the blocks are guaranteed to arrive in causal order.
          // Even for a equivocating validator, we just update its latest message
          // to whatever block we have fetched latest among the blocks that
          // constitute the equivocation.
          latestMessages = newLatestMessages,
          latestMessagesOfLatestMessages = newLatestLatestMessages,
          childMap = newChildMap,
          dataLookup = updatedLookup,
          topoSort = updatedSort
        )
      })
      (block.blockHash, block)
    }

  private def reAttemptBuffer: F[Unit] =
    for {
      blockBufferDependencyDag <- blockBufferDependencyDagState.get
      dependencyFree           = blockBufferDependencyDag.dependencyFree
      dependencyFreeBlocks = blockBuffer
        .filter(block => dependencyFree.contains(block.blockHash))
        .toList
      attempts <- dependencyFreeBlocks.traverse { b =>
                   for {
                     status <- attemptAdd(b)
                   } yield (b, status)
                 }
      _ <- if (attempts.isEmpty) {
            ().pure[F]
          } else {
            for {
              _ <- removeAdded(blockBufferDependencyDag, attempts)
              _ <- reAttemptBuffer
            } yield ()
          }
    } yield ()

  private def removeAdded(
      blockBufferDependencyDag: DoublyLinkedDag[BlockHash],
      attempts: List[(BlockMessage, BlockStatus)]
  ): F[Unit] =
    for {
      successfulAdds <- attempts
                         .filter {
                           case (_, status) => status.inDag
                         }
                         .pure[F]
      _ <- unsafeRemoveFromBlockBuffer(successfulAdds)
      _ <- removeFromBlockBufferDependencyDag(blockBufferDependencyDag, successfulAdds)
    } yield ()

  private def unsafeRemoveFromBlockBuffer(
      successfulAdds: List[(BlockMessage, BlockStatus)]
  ): F[Unit] =
    Sync[F]
      .delay {
        successfulAdds.map {
          blockBuffer -= _._1
        }
      }
      .as(Unit)

  private def removeFromBlockBufferDependencyDag(
      blockBufferDependencyDag: DoublyLinkedDag[BlockHash],
      successfulAdds: List[(BlockMessage, BlockStatus)]
  ): F[Unit] =
    blockBufferDependencyDagState.set(
      successfulAdds.foldLeft(blockBufferDependencyDag) {
        case (acc, successfulAdd) =>
          DoublyLinkedDagOperations.remove(acc, successfulAdd._1.blockHash)
      }
    )

  def getRuntimeManager: F[Option[RuntimeManager]] = Applicative[F].pure(Some(runtimeManager))

  def fetchDependencies: F[Unit] =
    for {
      blockBufferDependencyDag <- blockBufferDependencyDagState.get
      _ <- blockBufferDependencyDag.dependencyFree.toList.traverse { hash =>
            CommUtil.sendBlockRequest[F](BlockRequest(Base16.encode(hash.toByteArray), hash))
          }
    } yield ()
}
