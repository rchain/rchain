package coop.rchain.casper

import cats.{Applicative, Monad}
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.DagOperations
import coop.rchain.casper.util.ProtoUtil._
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.casper.util.rholang.{InterpreterUtil, RuntimeManager}
import coop.rchain.catscontrib._
import coop.rchain.crypto.codec.Base16
import coop.rchain.p2p.Network.ErrorHandler
import coop.rchain.p2p.effects._
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.comm.transport._
import coop.rchain.comm.discovery._
import coop.rchain.shared.AtomicSyncVar

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}
import scala.collection.immutable.{HashMap, HashSet}
import scala.concurrent.SyncVar
import java.nio.file.Path

import coop.rchain.casper.EquivocationRecord.SequenceNumber
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import monix.execution.Scheduler

trait Casper[F[_], A] {
  def addBlock(b: BlockMessage): F[Unit]
  def contains(b: BlockMessage): F[Boolean]
  def deploy(d: Deploy): F[Unit]
  def estimator: F[A]
  def createBlock: F[Option[BlockMessage]]
}

trait MultiParentCasper[F[_]] extends Casper[F, IndexedSeq[BlockMessage]] {
  def blockDag: F[BlockDag]
  def storageContents(hash: ByteString): F[String]
  def close(): F[Unit]
}

object MultiParentCasper extends MultiParentCasperInstances {
  def apply[F[_]](implicit instance: MultiParentCasper[F]): MultiParentCasper[F] = instance
}

sealed abstract class MultiParentCasperInstances {
  def noOpsCasper[F[_]: Applicative]: MultiParentCasper[F] =
    new MultiParentCasper[F] {
      def addBlock(b: BlockMessage): F[Unit]    = ().pure[F]
      def contains(b: BlockMessage): F[Boolean] = false.pure[F]
      def deploy(r: Deploy): F[Unit]            = ().pure[F]
      def estimator: F[IndexedSeq[BlockMessage]] =
        Applicative[F].pure[IndexedSeq[BlockMessage]](Vector(BlockMessage()))
      def createBlock: F[Option[BlockMessage]]         = Applicative[F].pure[Option[BlockMessage]](None)
      def blockDag: F[BlockDag]                        = BlockDag().pure[F]
      def storageContents(hash: ByteString): F[String] = "".pure[F]
      def close(): F[Unit]                             = ().pure[F]
    }

  def hashSetCasper[
      F[_]: Monad: Capture: NodeDiscovery: TransportLayer: Log: Time: ErrorHandler: SafetyOracle](
      storageLocation: Path,
      storageSize: Long,
      genesis: BlockMessage)(implicit scheduler: Scheduler): MultiParentCasper[F] =
    new MultiParentCasper[F] {
      type BlockHash = ByteString
      type Validator = ByteString

      //TODO: Extract hardcoded version
      private val version = 0L

      private val _blockDag: AtomicSyncVar[BlockDag] = new AtomicSyncVar(
        BlockDag().copy(
          blockLookup = HashMap[BlockHash, BlockMessage](genesis.blockHash -> genesis))
      )

      private val runtime = new SyncVar[Runtime]()
      runtime.put(Runtime.create(storageLocation, storageSize))
      private val (initStateHash, runtimeManager) = RuntimeManager.fromRuntime(runtime)

      private val knownStateHashesContainer: AtomicSyncVar[Set[StateHash]] = new AtomicSyncVar(
        Set[StateHash](initStateHash)
      )

      private val blockBuffer: mutable.HashSet[BlockMessage] =
        new mutable.HashSet[BlockMessage]()
      private val deployHist: mutable.HashSet[Deploy] = new mutable.HashSet[Deploy]()
      private val awaitingJustificationToChild =
        new mutable.HashMap[BlockHash, mutable.Set[BlockHash]]
        with mutable.MultiMap[BlockHash, BlockHash]

      // Used to keep track of when other validators detect the equivocation consisting of the base block
      // at the sequence number identified by the first element of the tuple.
      private val equivocationsTracker: mutable.MultiMap[Validator, EquivocationRecord] =
        new mutable.HashMap[Validator, mutable.Set[EquivocationRecord]]()
        with mutable.MultiMap[Validator, EquivocationRecord]

      def addBlock(b: BlockMessage): F[Unit] =
        for {
          validSig    <- Validate.blockSignature[F](b)
          dag         <- blockDag
          validSender <- Validate.blockSender[F](b, genesis, dag)
          attempt <- if (!validSig) InvalidUnslashableBlock.pure[F]
                    else if (!validSender) InvalidUnslashableBlock.pure[F]
                    else attemptAdd(b)
          _ <- attempt match {
                case Valid                  => reAttemptBuffer
                case AdmissibleEquivocation => reAttemptBuffer
                case _                      => ().pure[F]
              }
          estimates <- estimator
          tip       = estimates.head
          _ <- Log[F].info(
                s"CASPER: New fork-choice tip is block ${PrettyPrinter.buildString(tip.blockHash)}.")
          // TODO: Replace with getMainChainUntilLastFinalizedBlock
          forkchoice = getMainChain(dag, tip, IndexedSeq.empty[BlockMessage])
          forkchoiceSafety <- forkchoice.toList.traverse(block =>
                               SafetyOracle[F]
                                 .normalizedFaultTolerance(dag, block)
                                 .flatMap(faultTolerance =>
                                   Log[F].info(s"CASPER: Block ${PrettyPrinter
                                     .buildString(block.blockHash)} has a fault tolerance of ${faultTolerance}.")))
        } yield ()

      def contains(b: BlockMessage): F[Boolean] =
        Capture[F].capture {
          _blockDag.get.blockLookup.contains(b.blockHash) ||
          blockBuffer.contains(b)
        }

      def deploy(d: Deploy): F[Unit] =
        for {
          _ <- Capture[F].capture {
                deployHist += d
              }
          _ <- Log[F].info(s"CASPER: Received ${PrettyPrinter.buildString(d)}")
        } yield ()

      def estimator: F[IndexedSeq[BlockMessage]] =
        Capture[F].capture {
          Estimator.tips(_blockDag.get, genesis)
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
      def createBlock: F[Option[BlockMessage]] =
        for {
          orderedHeads   <- estimator
          dag            <- blockDag
          p              = chooseNonConflicting(orderedHeads, genesis, dag)
          r              <- remDeploys(dag, p)
          justifications = toJustification(dag.latestMessages)
          proposal <- if (r.nonEmpty || p.length > 1) {
                       createProposal(p, r, justifications)
                     } else {
                       none[BlockMessage].pure[F]
                     }
        } yield proposal

      private def remDeploys(dag: BlockDag, p: Seq[BlockMessage]): F[Seq[Deploy]] =
        Capture[F].capture {
          val result = deployHist.clone()
          DagOperations
            .bfTraverse(p)(parents(_).iterator.map(dag.blockLookup))
            .foreach(b => {
              b.body.foreach(_.newCode.foreach(result -= _))
            })
          result.toSeq
        }

      private def createProposal(p: Seq[BlockMessage],
                                 r: Seq[Deploy],
                                 justifications: Seq[Justification]): F[Option[BlockMessage]] =
        for {
          now <- Time[F].currentMillis
          Right((computedStateHash, _)) = knownStateHashesContainer
            .mapAndUpdate[(StateHash, Set[StateHash])](
              InterpreterUtil.computeDeploysCheckpoint(p,
                                                       r,
                                                       genesis,
                                                       _blockDag.get,
                                                       initStateHash,
                                                       _,
                                                       runtimeManager),
              _._2)
          postState = RChainState()
            .withTuplespace(computedStateHash)
            .withBonds(bonds(p.head))
            .withBlockNumber(p.headOption.fold(0L)(blockNumber) + 1)
          body = Body()
            .withPostState(postState)
            .withNewCode(r)
          header = blockHeader(body, p.map(_.blockHash), version, now)
          block  = unsignedBlockProto(body, header, justifications)
        } yield Some(block)

      def blockDag: F[BlockDag] = Capture[F].capture { _blockDag.get }

      def storageContents(hash: StateHash): F[String] = Capture[F].capture {
        if (knownStateHashesContainer.get.contains(hash)) {
          runtimeManager.storageRepr(hash)
        } else {
          s"Tuplespace hash ${Base16.encode(hash.toByteArray)} not found!"
        }
      }

      def close(): F[Unit] = Capture[F].capture {
        val _runtime = runtime.take()
        _runtime.close()
      }

      //invalid blocks return None and don't update the checkpoints
      private def validateTransactions(block: BlockMessage): F[Option[StateHash]] =
        Capture[F].capture {
          val Right((maybeCheckPoint, _)) =
            knownStateHashesContainer.mapAndUpdate[(Option[StateHash], Set[StateHash])](
              InterpreterUtil.validateBlockCheckpoint(
                block,
                genesis,
                _blockDag.get,
                initStateHash,
                _,
                runtimeManager
              ),
              _._2
            )
          maybeCheckPoint
        }

      /*
       * TODO: Put tuplespace validation back in after we have deterministic unforgeable names.
       *
       * We want to catch equivocations only after we confirm that the block completing
       * the equivocation is otherwise valid.
       */
      private def attemptAdd(b: BlockMessage): F[BlockStatus] =
        for {
          dag                  <- Capture[F].capture { _blockDag.get }
          postValidationStatus <- Validate.validateBlockSummary[F](b, genesis, dag)
          // TODO: postTransactionsCheckStatus <- validateTransactions(...)
          postedNeglectedEquivocationCheckStatus <- postValidationStatus.traverse(
                                                     _ =>
                                                       neglectedEquivocationsCheckWithRecordUpdate(
                                                         b,
                                                         dag))
          postEquivocationCheckStatus <- postedNeglectedEquivocationCheckStatus.traverse(_ =>
                                          equivocationsCheck(b, dag))
          status = postEquivocationCheckStatus.joinRight.merge
          _      <- addEffects(status, b)
        } yield status

      private def equivocationsCheck(
          block: BlockMessage,
          dag: BlockDag): F[Either[RejectableBlock, IncludeableBlock]] = {
        val justificationOfCreator = block.justifications
          .find {
            case Justification(validator: Validator, _) => validator == block.sender
          }
          .getOrElse(Justification.defaultInstance)
          .latestBlockHash
        val latestMessageOfCreator = dag.latestMessages.getOrElse(block.sender, ByteString.EMPTY)
        val isNotEquivocation      = justificationOfCreator == latestMessageOfCreator
        if (isNotEquivocation) {
          Applicative[F].pure(Right(Valid))
        } else if (awaitingJustificationToChild.contains(block.blockHash)) {
          Applicative[F].pure(Right(AdmissibleEquivocation))
        } else {
          Applicative[F].pure(Left(IgnorableEquivocation))
        }
      }

      private def neglectedEquivocationsCheckWithRecordUpdate(
          block: BlockMessage,
          dag: BlockDag): F[Either[RejectableBlock, IncludeableBlock]] =
        if (equivocationsTracker.isEmpty) {
          Applicative[F].pure(Right(Valid))
        } else {
          val neglectedEquivocationDetected =
            equivocationsTracker.filter(_._2.isEmpty).foldLeft(false) {
              case (acc, (equivocatingValidator, equivocationRecords)) =>
                equivocationRecords.foldLeft(acc) {
                  case (acc2, equivocationRecord: EquivocationRecord) =>
                    if (equivocationNeglected(block,
                                              dag,
                                              equivocatingValidator,
                                              equivocationRecord,
                                              Set.empty[BlockMessage])) {
                      val updatedEquivocationRecord = equivocationRecord.equivocationDetectedSeqNum
                        .updated(block.sender, block.seqNum)
                      equivocationsTracker.addBinding(
                        equivocatingValidator,
                        equivocationRecord.copy(
                          equivocationDetectedSeqNum = updatedEquivocationRecord))
                      true
                    } else {
                      acc2
                    }
                }
            }
          if (neglectedEquivocationDetected) {
            Applicative[F].pure(Left(NeglectedEquivocation))
          } else {
            Applicative[F].pure(Right(Valid))
          }
        }

      private def equivocationNeglected(block: BlockMessage,
                                        dag: BlockDag,
                                        equivocatingValidator: Validator,
                                        equivocationRecord: EquivocationRecord,
                                        equivocationChild: Set[BlockMessage]): Boolean = {
        val latestMessages = toLatestMessages(block.justifications)
        if (latestMessages.contains(equivocatingValidator)) {
          equivocationNeglectedAux(latestMessages.toSeq,
                                   dag,
                                   equivocatingValidator,
                                   equivocationRecord,
                                   equivocationChild)
        } else {
          // Since block has dropped equivocatingValidator from justifications, it has acknowledged the equivocation.
          // TODO: We check for unjustified droppings of validators in validateBlockSummary.
          false
        }
      }

      private def equivocationNeglectedAux(latestMessages: Seq[(Validator, BlockHash)],
                                           dag: BlockDag,
                                           equivocatingValidator: Validator,
                                           equivocationRecord: EquivocationRecord,
                                           equivocationChildren: Set[BlockMessage]): Boolean = {
        def updateEquivocationChildrenAndLoop(dag: BlockDag,
                                              equivocatingValidator: Validator,
                                              equivocationRecord: EquivocationRecord,
                                              equivocationChildren: Set[BlockMessage],
                                              remainder: Seq[(Validator, BlockHash)],
                                              justificationBlock: BlockMessage): Boolean = {
          val equivocationBaseBlockSeqNum = equivocationRecord.equivocationBaseBlockSeqNum
          val updatedEquivocationChildren = maybeAddEquivocationChildren(
            justificationBlock,
            dag,
            equivocatingValidator,
            equivocationBaseBlockSeqNum,
            equivocationChildren)
          if (updatedEquivocationChildren.size > 1) {
            true
          } else {
            equivocationNeglectedAux(remainder,
                                     dag,
                                     equivocatingValidator,
                                     equivocationRecord,
                                     updatedEquivocationChildren)
          }
        }

        def maybeAddEquivocationChildren(
            justificationBlock: BlockMessage,
            dag: BlockDag,
            equivocatingValidator: Validator,
            equivocationBaseBlockSeqNum: SequenceNumber,
            equivocationChildren: Set[BlockMessage]): Set[BlockMessage] = {
          // Latest according to the justification block
          val maybeLatestEquivocatingValidatorBlockHash: Option[BlockHash] =
            toLatestMessages(justificationBlock.justifications).get(equivocatingValidator)
          maybeLatestEquivocatingValidatorBlockHash match {
            case Some(blockHash) =>
              val latestEquivocatingValidatorBlock = dag.blockLookup(blockHash)
              if (latestEquivocatingValidatorBlock.seqNum >= equivocationBaseBlockSeqNum)
                equivocationChildren + latestEquivocatingValidatorBlock
              else
                equivocationChildren
            case None =>
              throw new Error(
                "justificationBlock is missing justification pointers to equivocatingValidator. This should have been caught earlier.")
          }
        }

        latestMessages match {
          case Nil => false
          case (validator, justificationBlockHash) :: remainder =>
            val justificationBlock = dag.blockLookup(justificationBlockHash)
            equivocationRecord.equivocationDetectedSeqNum.get(validator) match {
              case Some(seqNum) =>
                if (justificationBlock.seqNum >= seqNum) {
                  true
                } else {
                  updateEquivocationChildrenAndLoop(dag,
                                                    equivocatingValidator,
                                                    equivocationRecord,
                                                    equivocationChildren,
                                                    remainder,
                                                    justificationBlock)
                }
              case _ =>
                updateEquivocationChildrenAndLoop(dag,
                                                  equivocatingValidator,
                                                  equivocationRecord,
                                                  equivocationChildren,
                                                  remainder,
                                                  justificationBlock)
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
              missingParents = parents(block).filterNot(dag.blockLookup.contains).toSet
              missingJustifictions = block.justifications
                .map(_.latestBlockHash)
                .filterNot(dag.blockLookup.contains)
                .toSet
              _ <- (missingParents union missingJustifictions).toList.traverse(
                    hash =>
                      Capture[F].capture {
                        awaitingJustificationToChild.addBinding(hash, block.blockHash)
                      } *>
                        CommUtil.sendBlockRequest[F](
                          BlockRequest(Base16.encode(hash.toByteArray), hash)))
            } yield ()
          case AdmissibleEquivocation =>
            Capture[F].capture {
              val equivocationRecords =
                equivocationsTracker.getOrElse(block.sender,
                                               new mutable.HashSet[EquivocationRecord])
              val baseEquivocationBlockSeqNum = block.seqNum - 1
              if (equivocationRecords.exists {
                    case EquivocationRecord(seqNum, _) => baseEquivocationBlockSeqNum == seqNum
                  }) {
                // More than 2 equivocating children from base equivocation block and base block has already been recorded
              } else {
                val newEquivocationRecord =
                  EquivocationRecord(baseEquivocationBlockSeqNum,
                                     HashMap.empty[Validator, SequenceNumber])
                equivocationsTracker.addBinding(block.sender, newEquivocationRecord)
              }
            } *>
              addToState(block) *> CommUtil.sendBlock[F](block) *> Log[F].info(
              s"CASPER: Added ${PrettyPrinter.buildString(block.blockHash)}")
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
          case NeglectedEquivocation =>
            handleInvalidBlockEffect(status, block)
          case _ => throw new Error("Should never reach")
        }

      private def handleInvalidBlockEffect(status: BlockStatus, block: BlockMessage): F[Unit] =
        for {
          _ <- Log[F].info(s"CASPER: Did not add invalid block ${PrettyPrinter.buildString(
                block.blockHash)} for ${status.toString}.")
          // TODO: Slash block.blockHash for status except InvalidUnslashableBlock
          blocksToSlash = allChildren[BlockHash](awaitingJustificationToChild, block.blockHash) - block.blockHash
          _ <- Log[F].warn(s"""CASPER: About to slash the following blocks ${blocksToSlash
                .map(PrettyPrinter.buildString)
                .mkString("", ",", "")} for neglecting an invalid block""")
          // TODO: Slash blocksToSlash for neglecting an invalid block
          _ <- Capture[F].capture { blocksToSlash.foreach(awaitingJustificationToChild.remove) }
        } yield ()

      private def allChildren[A](map: mutable.MultiMap[A, A], element: A): Set[A] =
        DagOperations
          .bfTraverse[A](Some(element)) { (el: A) =>
            map.getOrElse(el, Set.empty[A]).iterator
          }
          .toSet

      private def addToState(block: BlockMessage): F[Unit] =
        Capture[F].capture {
          awaitingJustificationToChild -= block.blockHash
          _blockDag.update(bd => {
            val hash = block.blockHash

            //add current block as new child to each of its parents
            val newChildMap = parents(block).foldLeft(bd.childMap) {
              case (acc, p) =>
                val currChildren = acc.getOrElse(p, HashSet.empty[BlockHash])
                acc.updated(p, currChildren + hash)
            }

            bd.copy(
              blockLookup = bd.blockLookup.updated(hash, block),
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
              childMap = newChildMap
            )
          })
        }

      private def reAttemptBuffer: F[Unit] = {
        def findAddedBlockMessages(
            attempts: List[(BlockMessage, BlockStatus)]): List[BlockMessage] =
          attempts.filter(_._2 == Valid).map(_._1)

        def removeInvalidBlocksFromBuffer(attempts: List[(BlockMessage, BlockStatus)]) =
          attempts.flatMap {
            case (b, InvalidUnslashableBlock) =>
              blockBuffer -= b
              None
            case pair =>
              Some(pair)
          }

        for {
          attempts      <- blockBuffer.toList.traverse(b => attemptAdd(b).map(status => b -> status))
          validAttempts = removeInvalidBlocksFromBuffer(attempts)
          _ <- findAddedBlockMessages(validAttempts) match {
                case Nil => ().pure[F]
                case addedBlocks =>
                  Capture[F].capture { addedBlocks.map { blockBuffer -= _ } } *> reAttemptBuffer
              }
        } yield ()
      }
    }
}
