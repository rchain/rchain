package coop.rchain.casper

import cats.{Applicative, Monad}
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.{DagOperations, EventConverter}
import coop.rchain.casper.util.ProtoUtil._
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.casper.util.rholang.{InterpreterUtil, RuntimeManager}
import coop.rchain.catscontrib._
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.comm.CommError.ErrorHandler
import coop.rchain.p2p.effects._
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.comm.transport._
import coop.rchain.comm.discovery._
import coop.rchain.shared.{AtomicSyncVar, Log, LogSource, Time}
import coop.rchain.shared.AttemptOps._

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}
import scala.collection.immutable.{HashMap, HashSet}
import scala.concurrent.SyncVar
import scala.io.Source
import scala.util.Try
import java.nio.file.Path

import coop.rchain.casper.EquivocationRecord.SequenceNumber
import coop.rchain.casper.Estimator.Validator
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.rspace.{trace, Checkpoint}
import coop.rchain.rspace.trace.{COMM, Event}
import coop.rchain.rspace.trace.Event.codecLog
import monix.eval.Task
import monix.execution.Scheduler
import scodec.Codec
import scodec.bits.BitVector

trait Casper[F[_], A] {
  def addBlock(b: BlockMessage): F[Unit]
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
      def addBlock(b: BlockMessage): F[Unit]    = ().pure[F]
      def contains(b: BlockMessage): F[Boolean] = false.pure[F]
      def deploy(r: Deploy): F[Unit]            = ().pure[F]
      def estimator: F[IndexedSeq[BlockMessage]] =
        Applicative[F].pure[IndexedSeq[BlockMessage]](Vector(BlockMessage()))
      def createBlock: F[Option[BlockMessage]]                           = Applicative[F].pure[Option[BlockMessage]](None)
      def blockDag: F[BlockDag]                                          = BlockDag().pure[F]
      def normalizedInitialFault(weights: Map[Validator, Int]): F[Float] = 0f.pure[F]
      def storageContents(hash: ByteString): F[String]                   = "".pure[F]
    }

  def hashSetCasper[
      F[_]: Monad: Capture: NodeDiscovery: TransportLayer: Log: Time: ErrorHandler: SafetyOracle](
      activeRuntime: Runtime,
      validatorId: Option[ValidatorIdentity],
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
      runtime.put(activeRuntime)
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
      // at the sequence number identified by the (validator, base equivocation sequence number) pair of
      // each EquivocationRecord.
      private val equivocationsTracker: mutable.Set[EquivocationRecord] =
        new mutable.HashSet[EquivocationRecord]()

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
      def createBlock: F[Option[BlockMessage]] = validatorId match {
        case Some(vId @ ValidatorIdentity(publicKey, privateKey, sigAlgorithm)) =>
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
          } yield
            proposal.map(signBlock(_, dag, publicKey, privateKey, sigAlgorithm, vId.signFunction))

        case None => none[BlockMessage].pure[F]
      }

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
          Right((computedCheckpoint, _)) = knownStateHashesContainer
            .mapAndUpdate[(Checkpoint, Set[StateHash])](
              InterpreterUtil.computeDeploysCheckpoint(p,
                                                       r,
                                                       genesis,
                                                       _blockDag.get,
                                                       initStateHash,
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

      def blockDag: F[BlockDag] = Capture[F].capture { _blockDag.get }

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
                                                                     initStateHash,
                                                                     runtimeManager,
                                                                     knownStateHashesContainer))
          postedNeglectedEquivocationCheckStatus <- postTransactionsCheckStatus.traverse(
                                                     _ =>
                                                       neglectedEquivocationsCheckWithRecordUpdate(
                                                         b,
                                                         dag))
          postEquivocationCheckStatus <- postedNeglectedEquivocationCheckStatus.joinRight.traverse(
                                          _ => equivocationsCheck(b, dag))
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

      // See EquivocationRecord.scala for summary of algorithm.
      private def neglectedEquivocationsCheckWithRecordUpdate(
          block: BlockMessage,
          dag: BlockDag): F[Either[RejectableBlock, IncludeableBlock]] =
        Capture[F].capture {
          val neglectedEquivocationDetected =
            equivocationsTracker.foldLeft(false) {
              case (acc, equivocationRecord) =>
                getEquivocationDiscoveryStatus(block,
                                               dag,
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

      private def getEquivocationDiscoveryStatus(
          block: BlockMessage,
          dag: BlockDag,
          equivocationRecord: EquivocationRecord,
          equivocationChild: Set[BlockMessage]): EquivocationDiscoveryStatus = {
        val equivocatingValidator = equivocationRecord.equivocator
        val latestMessages        = toLatestMessages(block.justifications)
        if (equivocationDetectable(latestMessages.toSeq,
                                   dag,
                                   equivocationRecord,
                                   equivocationChild)) {
          if (latestMessages.contains(equivocatingValidator)) {
            EquivocationNeglected
          } else {
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
                                         dag: BlockDag,
                                         equivocationRecord: EquivocationRecord,
                                         equivocationChildren: Set[BlockMessage]): Boolean = {
        def maybeAddEquivocationChildren(
            justificationBlock: BlockMessage,
            dag: BlockDag,
            equivocatingValidator: Validator,
            equivocationBaseBlockSeqNum: SequenceNumber,
            equivocationChildren: Set[BlockMessage]): Set[BlockMessage] =
          if (justificationBlock.sender == equivocatingValidator) {
            if (justificationBlock.seqNum > equivocationBaseBlockSeqNum) {
              findJustificationParentWithSeqNum(justificationBlock,
                                                dag.blockLookup,
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
                val latestEquivocatingValidatorBlock = dag.blockLookup(blockHash)
                if (latestEquivocatingValidatorBlock.seqNum > equivocationBaseBlockSeqNum)
                  findJustificationParentWithSeqNum(latestEquivocatingValidatorBlock,
                                                    dag.blockLookup,
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
            val justificationBlock = dag.blockLookup(justificationBlockHash)
            if (equivocationRecord.equivocationDetectedBlockHashes.contains(justificationBlockHash)) {
              true
            } else {
              val equivocatingValidator       = equivocationRecord.equivocator
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
                equivocationDetectable(remainder,
                                       dag,
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
          case NeglectedEquivocation =>
            handleInvalidBlockEffect(status, block)
          case InvalidTransaction =>
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

            val newSeqNum = bd.currentSeqNum.updated(block.sender, block.seqNum)

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
              childMap = newChildMap,
              currentSeqNum = newSeqNum
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

  def fromConfig[
      F[_]: Monad: Capture: NodeDiscovery: TransportLayer: Log: Time: ErrorHandler: SafetyOracle,
      G[_]: Monad: Capture: Log: Time](conf: CasperConf, activeRuntime: Runtime)(
      implicit scheduler: Scheduler): G[MultiParentCasper[F]] =
    for {
      genesis <- Genesis.fromInputFiles[G](conf.bondsFile,
                                           conf.numValidators,
                                           conf.genesisPath,
                                           conf.walletsFile,
                                           activeRuntime)
      validatorId <- ValidatorIdentity.fromConfig[G](conf)
    } yield hashSetCasper[F](activeRuntime, validatorId, genesis)
}
