package coop.rchain.casper

import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import cats.{Applicative, Monad}
import cats.implicits._
import cats.mtl.implicits._
import cats.effect.Sync
import com.google.protobuf.ByteString
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.casper.protocol._
import coop.rchain.casper.util._
import coop.rchain.casper.util.ProtoUtil._
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.casper.util.rholang.{InterpreterUtil, RuntimeManager}
import coop.rchain.catscontrib._
import coop.rchain.crypto.codec.Base16
import coop.rchain.comm.CommError.ErrorHandler
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.shared._
import coop.rchain.shared.AttemptOps._

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}
import scala.collection.immutable.{HashMap, HashSet}

import cats.effect.concurrent.Ref
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.EquivocationRecord.SequenceNumber
import coop.rchain.casper.Estimator.Validator
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.rspace.Checkpoint
import monix.execution.Scheduler
import monix.execution.atomic.AtomicAny

trait Casper[F[_], A] {
  def addBlock(b: BlockMessage): F[BlockStatus]
  def contains(b: BlockMessage): F[Boolean]
  def deploy(d: DeployData): F[Either[Throwable, Unit]]
  def estimator: F[A]
  def createBlock: F[Option[BlockMessage]]
}

trait MultiParentCasper[F[_]] extends Casper[F, IndexedSeq[BlockMessage]] {
  def blockDag: F[BlockDag]
  // This is the weight of faults that have been accumulated so far.
  // We want the clique oracle to give us a fault tolerance that is greater than
  // this initial fault weight combined with our fault tolerance threshold t.
  def normalizedInitialFault(weights: Map[Validator, Int]): F[Float]
  def lastFinalizedBlock: F[BlockMessage]
  def storageContents(hash: ByteString): F[String]
  // TODO: Refactor hashSetCasper to take a RuntimeManager[F] just like BlockStore[F]
  def getRuntimeManager: F[Option[RuntimeManager]]
}

object MultiParentCasper extends MultiParentCasperInstances {
  def apply[F[_]](implicit instance: MultiParentCasper[F]): MultiParentCasper[F] = instance
}

sealed abstract class MultiParentCasperInstances {

  private implicit val logSource: LogSource = LogSource(this.getClass)

  def hashSetCasper[
      F[_]: Sync: Monad: Capture: ConnectionsCell: TransportLayer: Log: Time: ErrorHandler: SafetyOracle: BlockStore: RPConfAsk](
      runtimeManager: RuntimeManager,
      validatorId: Option[ValidatorIdentity],
      genesis: BlockMessage,
      shardId: String)(implicit scheduler: Scheduler): F[MultiParentCasper[F]] = {
    val dag = BlockDag()
    for {
      validateBlockCheckpointResult <- InterpreterUtil
                                        .validateBlockCheckpoint[F](
                                          genesis,
                                          genesis,
                                          dag,
                                          runtimeManager.emptyStateHash,
                                          Set[StateHash](runtimeManager.emptyStateHash),
                                          runtimeManager)
      (maybePostGenesisStateHash, _) = validateBlockCheckpointResult
    } yield
      createMultiParentCasper[F](runtimeManager,
                                 validatorId,
                                 genesis,
                                 dag,
                                 maybePostGenesisStateHash,
                                 shardId)

  }

  private[this] def createMultiParentCasper[
      F[_]: Sync: Monad: Capture: ConnectionsCell: TransportLayer: Log: Time: ErrorHandler: SafetyOracle: BlockStore: RPConfAsk](
      runtimeManager: RuntimeManager,
      validatorId: Option[ValidatorIdentity],
      genesis: BlockMessage,
      initialDag: BlockDag,
      maybePostGenesisStateHash: Option[StateHash],
      shardId: String)(implicit scheduler: Scheduler) =
    new MultiParentCasper[F] {
      type BlockHash = ByteString
      type Validator = ByteString

      //TODO: Extract hardcoded version
      private val version = 0L

      private val _blockDag: AtomicSyncVar[BlockDag] = new AtomicSyncVar(initialDag)

      private val emptyStateHash = runtimeManager.emptyStateHash

      private val knownStateHashesContainer: AtomicSyncVarF[F, Set[StateHash]] =
        maybePostGenesisStateHash match {
          case Some(postGenesisStateHash) =>
            AtomicSyncVarF.of[F, Set[StateHash]](
              Set[StateHash](emptyStateHash, postGenesisStateHash)
            )
          case None => throw new Error("Genesis block validation failed.")
        }

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
      private val faultToleranceThreshold     = 0f
      private val lastFinalizedBlockContainer = Ref.unsafe[F, BlockMessage](genesis)

      private val processingBlocks = new AtomicSyncVar(Set.empty[BlockHash])

      def addBlock(b: BlockMessage): F[BlockStatus] =
        for {
          acquire <- Capture[F].capture {
                      processingBlocks.mapAndUpdate[(Set[BlockHash], Boolean)](
                        blocks => {
                          if (blocks.contains(b.blockHash)) blocks -> false
                          else blocks                              -> true
                        }, {
                          case (blocks, false) => blocks
                          case (blocks, true)  => blocks + b.blockHash
                        }
                      )
                    }
          result <- acquire match {
                     case Right((_, false)) =>
                       Log[F]
                         .info(
                           s"CASPER: Block ${PrettyPrinter.buildString(b.blockHash)} is already being processed by another thread.")
                         .map(_ => BlockStatus.processing)
                     case Right((_, true)) =>
                       internalAddBlock(b).flatMap(status =>
                         Capture[F].capture { processingBlocks.update(_ - b.blockHash); status })
                     case Left(ex) =>
                       Log[F]
                         .warn(
                           s"CASPER: Block ${PrettyPrinter.buildString(b.blockHash)} encountered an exception during processing: ${ex.getMessage}")
                         .map(_ => BlockStatus.exception(ex))
                   }
        } yield result

      def internalAddBlock(b: BlockMessage): F[BlockStatus] =
        for {
          validFormat <- Validate.formatOfFields[F](b)
          validSig    <- Validate.blockSignature[F](b)
          dag         <- blockDag
          validSender <- Validate.blockSender[F](b, genesis, dag)
          attempt <- if (!validFormat) InvalidUnslashableBlock.pure[F]
                    else if (!validSig) InvalidUnslashableBlock.pure[F]
                    else if (!validSender) InvalidUnslashableBlock.pure[F]
                    else attemptAdd(b)
          _ <- attempt match {
                case MissingBlocks => ().pure[F]
                case _ =>
                  Capture[F].capture { blockBuffer -= b } *> blockBufferDependencyDagState.modify(
                    blockBufferDependencyDag =>
                      DoublyLinkedDagOperations.remove(blockBufferDependencyDag, b.blockHash))
              }
          _ <- attempt match {
                case MissingBlocks           => ().pure[F]
                case IgnorableEquivocation   => ().pure[F]
                case InvalidUnslashableBlock => ().pure[F]
                case _ =>
                  reAttemptBuffer // reAttempt for any status that resulted in the adding of the block into the view
              }
          estimates <- estimator
          tip       = estimates.head
          _ <- Log[F].info(
                s"CASPER: New fork-choice tip is block ${PrettyPrinter.buildString(tip.blockHash)}.")
          lastFinalizedBlock        <- lastFinalizedBlockContainer.get
          updatedLastFinalizedBlock <- updateLastFinalizedBlock(dag, lastFinalizedBlock)
          _                         <- lastFinalizedBlockContainer.set(updatedLastFinalizedBlock)
        } yield attempt

      private def updateLastFinalizedBlock(dag: BlockDag,
                                           lastFinalizedBlock: BlockMessage): F[BlockMessage] =
        for {
          childrenHashes <- dag.childMap
                             .getOrElse(lastFinalizedBlock.blockHash, Set.empty[BlockHash])
                             .pure[F]
          children <- childrenHashes.toList.traverse(ProtoUtil.unsafeGetBlock[F])
          maybeFinalizedChild <- ListContrib.findM(
                                  children,
                                  (block: BlockMessage) =>
                                    isGreaterThanFaultToleranceThreshold(dag, block))
          newFinalizedBlock <- maybeFinalizedChild match {
                                case Some(finalizedChild) =>
                                  updateLastFinalizedBlock(dag, finalizedChild)
                                case None => lastFinalizedBlock.pure[F]
                              }
        } yield newFinalizedBlock

      private def isGreaterThanFaultToleranceThreshold(dag: BlockDag,
                                                       block: BlockMessage): F[Boolean] =
        for {
          ft <- SafetyOracle[F].normalizedFaultTolerance(dag, block)
        } yield ft > faultToleranceThreshold

      def contains(b: BlockMessage): F[Boolean] =
        BlockStore[F].contains(b.blockHash).map(_ || blockBuffer.contains(b))

      def deploy(d: DeployData): F[Either[Throwable, Unit]] =
        InterpreterUtil.mkTerm(d.term) match {
          case Right(term) =>
            val deploy = Deploy(
              term = Some(term),
              raw = Some(d)
            )
            for {
              _ <- Capture[F].capture {
                    deployHist += deploy
                  }
              _ <- Log[F].info(s"CASPER: Received ${PrettyPrinter.buildString(deploy)}")
            } yield Right(())

          case Left(err) =>
            Applicative[F].pure(Left(new Exception(s"Error in parsing term: \n$err")))
        }

      def estimator: F[IndexedSeq[BlockMessage]] =
        for {
          lastFinalizedBlock <- lastFinalizedBlockContainer.get
          rankedEstimates    <- Estimator.tips[F](_blockDag.get, lastFinalizedBlock)
        } yield rankedEstimates

      /*
       * Logic:
       *  -Score each of the blockDAG heads extracted from the block messages via GHOST
       *  -Let P = subset of heads such that P contains no conflicts and the total score is maximized
       *  -Let R = subset of deploy messages which are not included in DAG obtained by following blocks in P
       *  -If R is non-empty then create a new block with parents equal to P and (non-conflicting) txns obtained from R
       *  -Else if R is empty and |P| > 1 then create a block with parents equal to P and no transactions
       *  -Else None
       */
      def createBlock: F[Option[BlockMessage]] = validatorId match {
        case Some(vId @ ValidatorIdentity(publicKey, privateKey, sigAlgorithm)) =>
          for {
            orderedHeads   <- estimator
            dag            <- blockDag
            p              <- chooseNonConflicting[F](orderedHeads, genesis, dag)
            r              <- remDeploys(dag, p)
            justifications = toJustification(dag.latestMessages)
            proposal <- if (r.nonEmpty || p.length > 1) {
                         createProposal(p, r, justifications)
                       } else {
                         none[BlockMessage].pure[F]
                       }
          } yield
            proposal.map(
              signBlock(_, dag, publicKey, privateKey, sigAlgorithm, vId.signFunction, shardId)
            )

        case None => none[BlockMessage].pure[F]
      }

      def lastFinalizedBlock: F[BlockMessage] = lastFinalizedBlockContainer.get

      // TODO: Optimize for large number of deploys accumulated over history
      private def remDeploys(dag: BlockDag, p: Seq[BlockMessage]): F[Seq[Deploy]] =
        for {
          result <- Capture[F].capture { deployHist.clone() }
          _ <- DagOperations
                .bfTraverseF[F, BlockMessage](p.toList)(ProtoUtil.unsafeGetParents[F])
                .foreach(b =>
                  Capture[F].capture {
                    b.body.foreach(_.newCode.flatMap(_.deploy).foreach(result -= _))
                })
        } yield result.toSeq

      private def createProposal(p: Seq[BlockMessage],
                                 r: Seq[Deploy],
                                 justifications: Seq[Justification]): F[Option[BlockMessage]] =
        for {
          now                                                                     <- Time[F].currentMillis
          deploysCheckpoint                                                       <- updateKnownStateHashes(knownStateHashesContainer, p, r)
          (computedCheckpoint, mergeLog, updatedKnownStateHashes, deployWithCost) = deploysCheckpoint
          computedStateHash                                                       = ByteString.copyFrom(computedCheckpoint.root.bytes.toArray)
          serializedLog                                                           = mergeLog ++ computedCheckpoint.log.map(EventConverter.toCasperEvent)
          postState = RChainState()
            .withTuplespace(computedStateHash)
            .withBonds(bonds(p.head))
            .withBlockNumber(p.headOption.fold(0L)(blockNumber) + 1)
          body = Body()
            .withPostState(postState)
            .withNewCode(deployWithCost)
            .withCommReductions(serializedLog)
          header = blockHeader(body, p.map(_.blockHash), version, now)
          block  = unsignedBlockProto(body, header, justifications, shardId)
        } yield Some(block)

      private def updateKnownStateHashes(
          knownStateHashesContainer: AtomicSyncVarF[F, Set[StateHash]],
          p: Seq[BlockMessage],
          r: Seq[Deploy]): F[(Checkpoint, Seq[Event], Set[StateHash], Vector[DeployCost])] =
        knownStateHashesContainer
          .modify[(Checkpoint, Seq[Event], Set[StateHash], Vector[DeployCost])] {
            knownStateHashes =>
              for {
                checkpoint <- InterpreterUtil.computeDeploysCheckpoint[F](
                               p,
                               r,
                               genesis,
                               _blockDag.get,
                               emptyStateHash,
                               knownStateHashes,
                               runtimeManager.computeState)
              } yield (checkpoint._3, checkpoint)
          }

      def blockDag: F[BlockDag] = Capture[F].capture {
        _blockDag.get
      }

      def storageContents(hash: StateHash): F[String] =
        for {
          knownStateHashes <- knownStateHashesContainer.get
        } yield
          if (knownStateHashes.contains(hash)) {
            runtimeManager.storageRepr(hash)
          } else {
            s"Tuplespace hash ${Base16.encode(hash.toByteArray)} not found!"
          }

      def normalizedInitialFault(weights: Map[Validator, Int]): F[Float] =
        (equivocationsTracker
          .map(_.equivocator)
          .toSet
          .flatMap(weights.get)
          .sum
          .toFloat / weightMapTotal(weights))
          .pure[F]

      /*
       * TODO: Put tuplespace validation back in after we have deterministic unforgeable names.
       *
       * We want to catch equivocations only after we confirm that the block completing
       * the equivocation is otherwise valid.
       */
      private def attemptAdd(b: BlockMessage): F[BlockStatus] =
        for {
          dag                  <- Capture[F].capture { _blockDag.get }
          postValidationStatus <- Validate.blockSummary[F](b, genesis, dag, shardId)
          postTransactionsCheckStatus <- postValidationStatus.traverse(
                                          _ =>
                                            Validate.transactions[F](b,
                                                                     genesis,
                                                                     dag,
                                                                     emptyStateHash,
                                                                     runtimeManager,
                                                                     knownStateHashesContainer))
          postBondsCacheStatus <- postTransactionsCheckStatus.joinRight.traverse(_ =>
                                   Validate.bondsCache[F](b, runtimeManager))
          postNeglectedInvalidBlockStatus <- postBondsCacheStatus.joinRight.traverse(
                                              _ =>
                                                Validate.neglectedInvalidBlock[F](
                                                  b,
                                                  invalidBlockTracker.toSet))
          postNeglectedEquivocationCheckStatus <- postNeglectedInvalidBlockStatus.joinRight
                                                   .traverse(
                                                     _ =>
                                                       EquivocationDetector
                                                         .checkNeglectedEquivocationsWithUpdate[F](
                                                           equivocationsTracker,
                                                           b,
                                                           dag))
          blockBufferDependencyDag <- blockBufferDependencyDagState.get
          postEquivocationCheckStatus <- postNeglectedEquivocationCheckStatus.joinRight.traverse(
                                          _ =>
                                            EquivocationDetector
                                              .checkEquivocations(blockBufferDependencyDag, b, dag)
                                              .pure[F])
          status = postEquivocationCheckStatus.joinRight.merge
          _      <- addEffects(status, b)
        } yield status

      // TODO: Handle slashing
      private def addEffects(status: BlockStatus, block: BlockMessage): F[Unit] =
        status match {
          //Add successful! Send block to peers, log success, try to add other blocks
          case Valid =>
            addToState(block) *> CommUtil.sendBlock[F](block) *> Log[F].info(
              s"CASPER: Added ${PrettyPrinter.buildString(block.blockHash)}")
          case MissingBlocks =>
            for {
              _              <- Capture[F].capture { blockBuffer += block }
              dag            <- blockDag
              missingParents = parentHashes(block).toSet
              missingJustifictions = block.justifications
                .map(_.latestBlockHash)
                .toSet
              missingDependencies <- (missingParents union missingJustifictions).toList.filterA(
                                      blockHash =>
                                        BlockStore[F]
                                          .contains(blockHash)
                                          .map(contains => !contains))
              _ <- missingDependencies.traverse(hash => handleMissingDependency(hash, block))
            } yield ()
          case AdmissibleEquivocation =>
            Capture[F].capture {
              val baseEquivocationBlockSeqNum = block.seqNum - 1
              if (equivocationsTracker.exists {
                    case EquivocationRecord(validator, seqNum, _) =>
                      block.sender == validator && baseEquivocationBlockSeqNum == seqNum
                  }) {
                // More than 2 equivocating children from base equivocation block and base block has already been recorded
              } else {
                val newEquivocationRecord = EquivocationRecord(block.sender,
                                                               baseEquivocationBlockSeqNum,
                                                               Set.empty[BlockHash])
                equivocationsTracker.add(newEquivocationRecord)
              }
            } *>
              addToState(block) *> CommUtil.sendBlock[F](block) *> Log[F].info(
              s"CASPER: Added admissible equivocation child block ${PrettyPrinter.buildString(block.blockHash)}")
          case IgnorableEquivocation =>
            /*
             * We don't have to include these blocks to the equivocation tracker because if any validator
             * will build off this side of the equivocation, we will get another attempt to add this block
             * through the admissible equivocations.
             */
            Log[F].info(
              s"CASPER: Did not add block ${PrettyPrinter.buildString(block.blockHash)} as that would add an equivocation to the BlockDAG")
          case InvalidUnslashableBlock =>
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
          case _ => throw new Error("Should never reach")
        }

      private def handleMissingDependency(hash: BlockHash, parentBlock: BlockMessage): F[Unit] =
        for {
          _ <- blockBufferDependencyDagState.modify(
                blockBufferDependencyDag =>
                  DoublyLinkedDagOperations
                    .add[BlockHash](blockBufferDependencyDag, hash, parentBlock.blockHash))
          _ <- CommUtil.sendBlockRequest[F](BlockRequest(Base16.encode(hash.toByteArray), hash))
        } yield ()

      private def handleInvalidBlockEffect(status: BlockStatus, block: BlockMessage): F[Unit] =
        for {
          _ <- Log[F].warn(s"CASPER: Recording invalid block ${PrettyPrinter.buildString(
                block.blockHash)} for ${status.toString}.")
          // TODO: Slash block for status except InvalidUnslashableBlock
          _ <- Capture[F].capture(invalidBlockTracker += block.blockHash) *> addToState(block)
        } yield ()

      private def addToState(block: BlockMessage): F[Unit] =
        BlockStore[F].put {
          _blockDag.update(bd => {
            val hash = block.blockHash

            //add current block as new child to each of its parents
            val newChildMap = parentHashes(block).foldLeft(bd.childMap) {
              case (acc, p) =>
                val currChildren = acc.getOrElse(p, HashSet.empty[BlockHash])
                acc.updated(p, currChildren + hash)
            }

            val newSeqNum = bd.currentSeqNum.updated(block.sender, block.seqNum)
            bd.copy(
              //Assume that a non-equivocating validator must include
              //its own latest message in the justification. Therefore,
              //for a given validator the blocks are guaranteed to arrive in causal order.
              // Even for a equivocating validator, we just update its latest message
              // to whatever block we have fetched latest among the blocks that
              // constitute the equivocation.
              latestMessages = bd.latestMessages.updated(block.sender, hash),
              latestMessagesOfLatestMessages =
                bd.latestMessagesOfLatestMessages.updated(block.sender,
                                                          toLatestMessages(block.justifications)),
              childMap = newChildMap,
              currentSeqNum = newSeqNum
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
          attempts <- dependencyFreeBlocks.traverse(b => attemptAdd(b))
          _ <- if (attempts.isEmpty) {
                ().pure[F]
              } else {
                Capture[F].capture {
                  dependencyFreeBlocks.map {
                    blockBuffer -= _
                  }
                } *>
                  blockBufferDependencyDagState.set(
                    dependencyFree.foldLeft(blockBufferDependencyDag) {
                      case (acc, hash) =>
                        DoublyLinkedDagOperations.remove(acc, hash)
                    }) *> reAttemptBuffer
              }
        } yield ()

      def getRuntimeManager: F[Option[RuntimeManager]] = Applicative[F].pure(Some(runtimeManager))
    }
}
