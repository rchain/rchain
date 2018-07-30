package coop.rchain.casper

import cats.{Applicative, Id, Monad}
import cats.implicits._
import cats.effect.Bracket
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.InMemBlockStore
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

import cats.mtl.MonadState
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.casper.EquivocationRecord.SequenceNumber
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
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
  def deploy(d: Deploy): F[Unit]
  def estimator: F[A]
  def createBlock: F[Option[BlockMessage]]
}

trait MultiParentCasper[F[_]] extends Casper[F, IndexedSeq[BlockMessage]] {
  def blockDag: F[BlockDag]
  // This is the weight of faults that have been accumulated so far.
  // We want the clique oracle to give us a fault tolerance that is greater than
  // this initial fault weight combined with our fault tolerance threshold t.
  def normalizedInitialFault(weights: Map[Validator, Int]): F[Float]
  def storageContents(hash: ByteString): F[String]
}

object MultiParentCasper extends MultiParentCasperInstances {
  def apply[F[_]](implicit instance: MultiParentCasper[F]): MultiParentCasper[F] = instance
}

sealed abstract class MultiParentCasperInstances {

  private implicit val logSource: LogSource = LogSource(this.getClass)

  def noOpsCasper[F[_]: Applicative]: MultiParentCasper[F] =
    new MultiParentCasper[F] {
      def addBlock(b: BlockMessage): F[BlockStatus] = BlockStatus.valid.pure[F]
      def contains(b: BlockMessage): F[Boolean]     = false.pure[F]
      def deploy(r: Deploy): F[Unit]                = ().pure[F]
      def estimator: F[IndexedSeq[BlockMessage]] =
        Applicative[F].pure[IndexedSeq[BlockMessage]](Vector(BlockMessage()))
      def createBlock: F[Option[BlockMessage]]                           = Applicative[F].pure[Option[BlockMessage]](None)
      def blockDag: F[BlockDag]                                          = BlockDag().pure[F]
      def normalizedInitialFault(weights: Map[Validator, Int]): F[Float] = 0f.pure[F]
      def storageContents(hash: ByteString): F[String]                   = "".pure[F]
    }

  // TODO: Add Sync as a constraint to ensure stack safe-ness
  def hashSetCasper[
      F[_]: Monad: Capture: NodeDiscovery: TransportLayer: Log: Time: ErrorHandler: SafetyOracle: BlockStore](
      runtimeManager: RuntimeManager,
      validatorId: Option[ValidatorIdentity],
      genesis: BlockMessage,
      internalMap: Map[BlockHash, BlockMessage])(
      implicit scheduler: Scheduler): MultiParentCasper[F] = {
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
    createMultiParentCasper[F](runtimeManager, validatorId, genesis, dag, maybePostGenesisStateHash)
  }

  private[this] def createMultiParentCasper[
      F[_]: Monad: Capture: NodeDiscovery: TransportLayer: Log: Time: ErrorHandler: SafetyOracle: BlockStore](
      runtimeManager: RuntimeManager,
      validatorId: Option[ValidatorIdentity],
      genesis: BlockMessage,
      dag: BlockDag,
      maybePostGenesisStateHash: Option[StateHash])(implicit scheduler: Scheduler) =
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

      def addBlock(b: BlockMessage): F[BlockStatus] =
        for {
          validSig    <- Validate.blockSignature[F](b)
          dag         <- blockDag
          validSender <- Validate.blockSender[F](b, genesis, dag)
          validDeploy <- Validate.repeatDeploy[F](b, genesis, dag)
          attempt <- if (!validSig) InvalidUnslashableBlock.pure[F]
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
        } yield attempt

      def contains(b: BlockMessage): F[Boolean] =
        BlockStore[F].contains(b.blockHash).map(_ || blockBuffer.contains(b))

      def deploy(d: Deploy): F[Unit] =
        for {
          _ <- Capture[F].capture {
                deployHist += d
              }
          _ <- Log[F].info(s"CASPER: Received ${PrettyPrinter.buildString(d)}")
        } yield ()

      def estimator: F[IndexedSeq[BlockMessage]] =
        BlockStore[F].asMap() flatMap { internalMap: Map[BlockHash, BlockMessage] =>
          Capture[F].capture {
            Estimator.tips(_blockDag.get, internalMap, genesis)
          }
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
            proposal.map(signBlock(_, dag, publicKey, privateKey, sigAlgorithm, vId.signFunction))

        case None => none[BlockMessage].pure[F]
      }

      private def remDeploys(dag: BlockDag, p: Seq[BlockMessage]): F[Seq[Deploy]] =
        BlockStore[F].asMap() flatMap { internalMap: Map[BlockHash, BlockMessage] =>
          Capture[F].capture {
            val result = deployHist.clone()
            DagOperations
              .bfTraverse(p)(parents(_).iterator.map(internalMap))
              .foreach(b => {
                b.body.foreach(_.newCode.foreach(result -= _))
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
          Right((computedCheckpoint, _)) = knownStateHashesContainer
            .mapAndUpdate[(Checkpoint, Set[StateHash])](
              InterpreterUtil.computeDeploysCheckpoint(p,
                                                       r,
                                                       genesis,
                                                       _blockDag.get,
                                                       internalMap,
                                                       emptyStateHash,
                                                       _,
                                                       runtimeManager.computeState),
              _._2)
          computedStateHash = ByteString.copyFrom(computedCheckpoint.root.bytes.toArray)
          serializedLog     = computedCheckpoint.log.map(EventConverter.toCasperEvent)
          postState = RChainState()
            .withTuplespace(computedStateHash)
            .withBonds(bonds(p.head))
            .withBlockNumber(p.headOption.fold(0L)(blockNumber) + 1)
          body = Body()
            .withPostState(postState)
            .withNewCode(r)
            .withCommReductions(serializedLog)
          header = blockHeader(body, p.map(_.blockHash), version, now)
          block  = unsignedBlockProto(body, header, justifications)
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
          postValidationStatus <- Validate.blockSummary[F](b, genesis, dag)
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
    }
}
