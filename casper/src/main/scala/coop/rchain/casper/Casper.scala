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
import coop.rchain.shared.AtomicSyncVar

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.immutable
import scala.collection.immutable.{HashMap, HashSet}
import scala.concurrent.SyncVar
import java.nio.file.Path

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
      private val expectedBlockRequests: mutable.HashSet[BlockHash] =
        new mutable.HashSet[BlockHash]()

      def addBlock(b: BlockMessage): F[Unit] =
        for {
          validSig    <- Validate.blockSignature[F](b)
          dag         <- blockDag
          validSender <- Validate.blockSender[F](b, genesis, dag)
          attempt <- if (!validSig) InvalidBlockSignature.pure[F]
                    else if (!validSender) InvalidSender.pure[F]
                    else attemptAdd(b)
          _ <- attempt match {
                case Valid => reAttemptBuffer
                case _     => ().pure[F]
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
          justifications = justificationProto(dag.latestMessages)
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
        if (knownStateHashesContainer.get.intersect(Set(hash)).isEmpty) {
          s"Tuplespace hash ${Base16.encode(hash.toByteArray)} not found!"
        } else {
          runtimeManager.storageRepr(hash)
        }
      }

      def close(): F[Unit] = Capture[F].capture {
        val _runtime = runtime.take()
        _runtime.close()
      }

      private def isValid(block: BlockMessage): F[Boolean] =
        for {
          dag <- Capture[F].capture { _blockDag.get }
          bn  <- Validate.blockNumber[F](block, dag)
          p   <- Validate.parents[F](block, genesis, dag)
          //only try checkpoint if other validity checks pass
          ch <- if (bn && p) validateTransactions(block)
               else Monad[F].pure[Option[RuntimeManager]](None)
          //_  <- if (bn && p && ch.isEmpty) Log[F].warn(Validate.ignore(block, "tuplespace hash could not be reproduced.")
          //      else ().pure[F]
          //TODO: Put tuplespace validation back in after we have
          //      tuplespace replay. Cannot put it in now because
          //      it fails on unforgable names (no way to replicate
          //      the same unforgable name without an execution trace).
        } yield bn && p //should be ch.isDefined

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
                      Capture[F].capture { expectedBlockRequests += hash } *>
                        CommUtil.sendBlockRequest[F](
                          BlockRequest(Base16.encode(hash.toByteArray), hash)))
            } yield ()
          case Equivocation =>
            Log[F].info(
              s"CASPER: Did not add block ${PrettyPrinter.buildString(block.blockHash)} as that would add an equivocation to the BlockDAG")
          case InvalidBlock =>
            Log[F].info(
              s"CASPER: Did not add invalid block ${PrettyPrinter.buildString(block.blockHash)}")
          case _ => throw new Error("Should never reach")
        }

      private def canAdd(block: BlockMessage): BlockStatus = {
        val dag            = _blockDag.get
        val parentsPresent = parents(block).forall(p => dag.blockLookup.contains(p))
        val justificationsPresent =
          block.justifications.forall(j => dag.blockLookup.contains(j.latestBlockHash))
        if (parentsPresent && justificationsPresent) {
          val justificationOfCreator = block.justifications
            .find {
              case Justification(validator: Validator, _) => validator == block.sender
            }
            .getOrElse(Justification.defaultInstance)
            .latestBlockHash
          val latestMessageOfCreator = dag.latestMessages.getOrElse(block.sender, ByteString.EMPTY)
          val isNotEquivocation      = justificationOfCreator == latestMessageOfCreator
          if (isNotEquivocation || expectedBlockRequests.contains(block.blockHash)) {
            Undecided
          } else {
            Equivocation
          }
        } else {
          MissingBlocks
        }
      }

      private def addToState(block: BlockMessage): F[Unit] =
        Capture[F].capture {
          expectedBlockRequests -= block.blockHash
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
              childMap = newChildMap
            )
          })
        }

      private def attemptAdd(b: BlockMessage): F[BlockStatus] =
        canAdd(b) match {
          case MissingBlocks =>
            for {
              _ <- addEffects(MissingBlocks, b)
            } yield MissingBlocks
          case Equivocation =>
            for {
              _ <- addEffects(Equivocation, b)
            } yield Equivocation
          case Undecided =>
            for {
              valid  <- isValid(b)
              result = if (valid) Valid else InvalidBlock
              _      <- addEffects(result, b)
            } yield result
          case _ => throw new Error("Should never reach")
        }

      private def reAttemptBuffer: F[Unit] = {
        def findAddedBlockMessages(
            attempts: List[(BlockMessage, BlockStatus)]): List[BlockMessage] =
          attempts.filter(_._2 == Valid).map(_._1)

        def removeInvalidBlocksFromBuffer(attempts: List[(BlockMessage, BlockStatus)]) =
          attempts.flatMap {
            case (b, InvalidBlock) =>
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
