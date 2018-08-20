package coop.rchain.casper

import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import cats.{Applicative, Id, Monad}
import cats.implicits._
import cats.effect.{Bracket, Sync}
import com.google.protobuf.ByteString
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.protocol._
import coop.rchain.casper.util._
import coop.rchain.casper.util.ProtoUtil._
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.casper.util.rholang.{InterpreterUtil, RuntimeManager}
import coop.rchain.catscontrib._
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.comm.CommError.ErrorHandler
import coop.rchain.p2p.effects._
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.comm.discovery._
import coop.rchain.shared._
import coop.rchain.shared.AttemptOps._

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}
import scala.collection.immutable.{HashMap, HashSet}
import scala.concurrent.SyncVar
import scala.io.Source
import scala.util.Try
import java.nio.file.Path

import cats.effect.concurrent.Ref
import cats.mtl.MonadState
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.casper.EquivocationRecord.SequenceNumber
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.models.Par
import coop.rchain.rspace.{trace, Checkpoint}
import coop.rchain.rspace.trace.{COMM, Event}
import coop.rchain.rspace.trace.Event.codecLog
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.atomic.AtomicAny
import scodec.Codec
import scodec.bits.BitVector

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
      internalMap: Map[BlockHash, BlockMessage],
      shardId: String)(implicit scheduler: Scheduler): MultiParentCasper[F] = {
    val dag = BlockDag()
    val (maybePostGenesisStateHash, _) = InterpreterUtil
      .validateBlockCheckpoint(
        genesis,
        genesis,
        dag,
        internalMap,
        runtimeManager.emptyStateHash,
        Set[StateHash](runtimeManager.emptyStateHash),
        runtimeManager
      )
    createMultiParentCasper[F](
      runtimeManager,
      validatorId,
      genesis,
      dag,
      maybePostGenesisStateHash,
      shardId
    )
  }

  private[this] def createMultiParentCasper[
      F[_]: Sync: Monad: Capture: ConnectionsCell: TransportLayer: Log: Time: ErrorHandler: SafetyOracle: BlockStore: RPConfAsk](
      runtimeManager: RuntimeManager,
      validatorId: Option[ValidatorIdentity],
      genesis: BlockMessage,
      dag: BlockDag,
      maybePostGenesisStateHash: Option[StateHash],
      shardId: String)(implicit scheduler: Scheduler) =
    new MultiParentCasper[F] {
      type BlockHash = ByteString
      type Validator = ByteString

      //TODO: Extract hardcoded version
      private val version = 0L

      private val _blockDag: AtomicSyncVar[BlockDag] = new AtomicSyncVar(dag)

      private val emptyStateHash = runtimeManager.emptyStateHash

      private val knownStateHashesContainer: AtomicSyncVar[Set[StateHash]] =
        maybePostGenesisStateHash match {
          case Some(postGenesisStateHash) =>
            new AtomicSyncVar(
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
          validDeploy <- Validate.repeatDeploy[F](b, genesis, dag)
          attempt <- if (!validFormat) InvalidUnslashableBlock.pure[F]
                    else if (!validSig) InvalidUnslashableBlock.pure[F]
                    else if (!validSender) InvalidUnslashableBlock.pure[F]
                    else if (!validDeploy) InvalidRepeatDeploy.pure[F]
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
                case InvalidRepeatDeploy     => ().pure[F]
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

      def updateLastFinalizedBlock(dag: BlockDag,
                                   lastFinalizedBlock: BlockMessage): F[BlockMessage] = {
        val maybeFinalizedBlockChildren = dag.childMap.get(lastFinalizedBlock.blockHash)
        maybeFinalizedBlockChildren match {
          case Some(finalizedBlockChildren) =>
            updateLastFinalizedBlockAux(dag, lastFinalizedBlock, finalizedBlockChildren.toSeq)
          case None => lastFinalizedBlock.pure[F]
        }
      }

      def updateLastFinalizedBlockAux(
          dag: BlockDag,
          lastFinalizedBlock: BlockMessage,
          finalizedBlockCandidatesHashes: Seq[BlockHash]): F[BlockMessage] =
        finalizedBlockCandidatesHashes match {
          case Nil => lastFinalizedBlock.pure[F]
          case blockHash +: rem =>
            for {
              maybeBlock <- BlockStore[F].get(blockHash)
              updatedLastFinalizedBlock <- maybeBlock match {
                                            case Some(block) =>
                                              for {
                                                normalizedFaultTolerance <- SafetyOracle[F]
                                                                             .normalizedFaultTolerance(
                                                                               dag,
                                                                               block)
                                                updatedLastFinalizedBlock <- if (normalizedFaultTolerance > faultToleranceThreshold) {
                                                                              updateLastFinalizedBlock(
                                                                                dag,
                                                                                block)
                                                                            } else {
                                                                              updateLastFinalizedBlockAux(
                                                                                dag,
                                                                                lastFinalizedBlock,
                                                                                rem)
                                                                            }
                                              } yield updatedLastFinalizedBlock
                                            case None =>
                                              lastFinalizedBlock.pure[F]
                                          }
            } yield updatedLastFinalizedBlock
        }

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
        BlockStore[F].asMap() flatMap { internalMap: Map[BlockHash, BlockMessage] =>
          for {
            lastFinalizedBlock <- lastFinalizedBlockContainer.get
            rankedEstimates <- Capture[F].capture {
                                Estimator.tips(_blockDag.get, internalMap, lastFinalizedBlock)
                              }
          } yield rankedEstimates
        }

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
            internalMap    <- BlockStore[F].asMap()
            p              = chooseNonConflicting(orderedHeads, genesis, dag, internalMap)
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

      private def remDeploys(dag: BlockDag, p: Seq[BlockMessage]): F[Seq[Deploy]] =
        BlockStore[F].asMap() flatMap { internalMap: Map[BlockHash, BlockMessage] =>
          Capture[F].capture {
            val result = deployHist.clone()
            DagOperations
              .bfTraverse(p)(parents(_).iterator.map(internalMap))
              .foreach(b => {
                b.body.foreach(_.newCode.flatMap(_.deploy).foreach(result -= _))
              })
            result.toSeq
          }
        }

      private def createProposal(p: Seq[BlockMessage],
                                 r: Seq[Deploy],
                                 justifications: Seq[Justification]): F[Option[BlockMessage]] =
        for {
          now         <- Time[F].currentMillis
          internalMap <- BlockStore[F].asMap()
          Right((computedCheckpoint, mergeLog, _, deployWithCost)) = knownStateHashesContainer
            .mapAndUpdate[(Checkpoint, Seq[protocol.Event], Set[StateHash], Vector[DeployCost])](
              InterpreterUtil.computeDeploysCheckpoint(p,
                                                       r,
                                                       genesis,
                                                       _blockDag.get,
                                                       internalMap,
                                                       emptyStateHash,
                                                       _,
                                                       runtimeManager.computeState),
              _._3)
          computedStateHash = ByteString.copyFrom(computedCheckpoint.root.bytes.toArray)
          serializedLog     = mergeLog ++ computedCheckpoint.log.map(EventConverter.toCasperEvent)
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

      def blockDag: F[BlockDag] = Capture[F].capture {
        _blockDag.get
      }

      def storageContents(hash: StateHash): F[String] = Capture[F].capture {
        if (knownStateHashesContainer.get.contains(hash)) {
          runtimeManager.storageRepr(hash)
        } else {
          s"Tuplespace hash ${Base16.encode(hash.toByteArray)} not found!"
        }
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
                                                Validate.neglectedInvalidBlockCheck[F](
                                                  b,
                                                  invalidBlockTracker.toSet))
          postNeglectedEquivocationCheckStatus <- postNeglectedInvalidBlockStatus.joinRight
                                                   .traverse(
                                                     _ =>
                                                       neglectedEquivocationsCheckWithRecordUpdate(
                                                         b,
                                                         dag))
          postEquivocationCheckStatus <- postNeglectedEquivocationCheckStatus.joinRight.traverse(
                                          _ => equivocationsCheck(b, dag))
          status = postEquivocationCheckStatus.joinRight.merge
          _      <- addEffects(status, b)
        } yield status

      private def equivocationsCheck(block: BlockMessage,
                                     dag: BlockDag): F[Either[InvalidBlock, ValidBlock]] =
        for {
          blockBufferDependencyDag <- blockBufferDependencyDagState.get
          justificationOfCreator = block.justifications
            .find {
              case Justification(validator: Validator, _) => validator == block.sender
            }
            .getOrElse(Justification.defaultInstance)
            .latestBlockHash
          latestMessageOfCreator = dag.latestMessages.getOrElse(block.sender, ByteString.EMPTY)
          isNotEquivocation      = justificationOfCreator == latestMessageOfCreator
          result <- if (isNotEquivocation) {
                     Applicative[F].pure(Right(Valid))
                   } else if (blockBufferDependencyDag.parentToChildAdjacencyList.contains(
                                block.blockHash)) {
                     Applicative[F].pure(Left(AdmissibleEquivocation))
                   } else {
                     Applicative[F].pure(Left(IgnorableEquivocation))
                   }
        } yield result

      // See EquivocationRecord.scala for summary of algorithm.
      private def neglectedEquivocationsCheckWithRecordUpdate(
          block: BlockMessage,
          dag: BlockDag): F[Either[InvalidBlock, ValidBlock]] =
        BlockStore[F].asMap() flatMap { internalMap: Map[BlockHash, BlockMessage] =>
          Capture[F].capture {
            val neglectedEquivocationDetected =
              equivocationsTracker.foldLeft(false) {
                case (acc, equivocationRecord) =>
                  getEquivocationDiscoveryStatus(block,
                                                 dag,
                                                 internalMap,
                                                 equivocationRecord,
                                                 Set.empty[BlockMessage]) match {
                    case EquivocationNeglected =>
                      true
                    case EquivocationDetected =>
                      val updatedEquivocationDetectedBlockHashes = equivocationRecord.equivocationDetectedBlockHashes + block.blockHash
                      equivocationsTracker.remove(equivocationRecord)
                      equivocationsTracker.add(
                        equivocationRecord.copy(
                          equivocationDetectedBlockHashes = updatedEquivocationDetectedBlockHashes))
                      acc
                    case EquivocationOblivious =>
                      acc
                  }

              }
            if (neglectedEquivocationDetected) {
              Left(NeglectedEquivocation)
            } else {
              Right(Valid)
            }
          }
        }

      private def getEquivocationDiscoveryStatus(
          block: BlockMessage,
          dag: BlockDag,
          internalMap: Map[BlockHash, BlockMessage],
          equivocationRecord: EquivocationRecord,
          equivocationChild: Set[BlockMessage]): EquivocationDiscoveryStatus = {
        val equivocatingValidator = equivocationRecord.equivocator
        val latestMessages        = toLatestMessages(block.justifications)
        if (equivocationDetectable(latestMessages.toSeq,
                                   internalMap,
                                   equivocationRecord,
                                   equivocationChild)) {
          val maybeEquivocatingValidatorBond =
            bonds(block).find(_.validator == equivocatingValidator)
          maybeEquivocatingValidatorBond match {
            case Some(Bond(_, stake)) =>
              if (stake > 0) {
                EquivocationNeglected
              } else {
                // TODO: Eliminate by having a validity check that says no stake can be 0
                EquivocationDetected
              }
            case None =>
              EquivocationDetected
          }
        } else {
          // Since block has dropped equivocatingValidator from justifications, it has acknowledged the equivocation.
          // TODO: We check for unjustified droppings of validators in validateBlockSummary.
          EquivocationOblivious
        }
      }

      @tailrec
      private def equivocationDetectable(latestMessages: Seq[(Validator, BlockHash)],
                                         internalMap: Map[BlockHash, BlockMessage],
                                         equivocationRecord: EquivocationRecord,
                                         equivocationChildren: Set[BlockMessage]): Boolean = {
        def maybeAddEquivocationChildren(
            justificationBlock: BlockMessage,
            internalMap: Map[BlockHash, BlockMessage],
            equivocatingValidator: Validator,
            equivocationBaseBlockSeqNum: SequenceNumber,
            equivocationChildren: Set[BlockMessage]): Set[BlockMessage] =
          if (justificationBlock.sender == equivocatingValidator) {
            if (justificationBlock.seqNum > equivocationBaseBlockSeqNum) {
              findJustificationParentWithSeqNum(justificationBlock,
                                                internalMap,
                                                equivocationBaseBlockSeqNum + 1) match {
                case Some(equivocationChild) => equivocationChildren + equivocationChild
                case None =>
                  throw new Error(
                    "justification parent with higher sequence number hasn't been added to the blockDAG yet.")
              }
            } else {
              equivocationChildren
            }
          } else {
            // Latest according to the justification block
            val maybeLatestEquivocatingValidatorBlockHash: Option[BlockHash] =
              toLatestMessages(justificationBlock.justifications).get(equivocatingValidator)
            maybeLatestEquivocatingValidatorBlockHash match {
              case Some(blockHash) =>
                val latestEquivocatingValidatorBlock = internalMap(blockHash)
                if (latestEquivocatingValidatorBlock.seqNum > equivocationBaseBlockSeqNum)
                  findJustificationParentWithSeqNum(latestEquivocatingValidatorBlock,
                                                    internalMap,
                                                    equivocationBaseBlockSeqNum + 1) match {
                    case Some(equivocationChild) => equivocationChildren + equivocationChild
                    case None =>
                      throw new Error(
                        "justification parent with higher sequence number hasn't been added to the blockDAG yet.")
                  } else
                  equivocationChildren
              case None =>
                throw new Error(
                  "justificationBlock is missing justification pointers to equivocatingValidator even though justificationBlock isn't a part of equivocationDetectedBlockHashes for this equivocation record.")
            }
          }

        latestMessages match {
          case Nil => false
          case (_, justificationBlockHash) +: remainder =>
            val justificationBlock = internalMap.get(justificationBlockHash).get
            if (equivocationRecord.equivocationDetectedBlockHashes.contains(justificationBlockHash)) {
              true
            } else {
              val equivocatingValidator       = equivocationRecord.equivocator
              val equivocationBaseBlockSeqNum = equivocationRecord.equivocationBaseBlockSeqNum
              val updatedEquivocationChildren = maybeAddEquivocationChildren(
                justificationBlock,
                internalMap,
                equivocatingValidator,
                equivocationBaseBlockSeqNum,
                equivocationChildren)
              if (updatedEquivocationChildren.size > 1) {
                true
              } else {
                equivocationDetectable(remainder,
                                       internalMap,
                                       equivocationRecord,
                                       updatedEquivocationChildren)
              }
            }
        }
      }

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
              internalMap    <- BlockStore[F].asMap()
              missingParents = parents(block).toSet
              missingJustifictions = block.justifications
                .map(_.latestBlockHash)
                .toSet
              missingDependencies = (missingParents union missingJustifictions).filterNot(
                internalMap.contains)
              _ <- missingDependencies.toList.traverse(hash => handleMissingDependency(hash, block))
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
          case InvalidRepeatDeploy => handleInvalidBlockEffect(status, block)
          case _                   => throw new Error("Should never reach")
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
            val newChildMap = parents(block).foldLeft(bd.childMap) {
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
        // TODO: What if you get that same exact block come in
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
