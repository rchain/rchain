package coop.rchain.casper

import cats.{Applicative, Monad}
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.DagOperations
import coop.rchain.casper.util.ProtoUtil._
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.casper.util.rholang.{Checkpoint, InterpreterUtil}
import coop.rchain.catscontrib.{Capture, IOUtil}
import coop.rchain.p2p.Network.{ErrorHandler, KeysStore}
import coop.rchain.p2p.effects._
import coop.rchain.shared.AtomicSyncVar

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.immutable
import scala.collection.immutable.{HashMap, HashSet}

import java.nio.file.Path

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
  def tsCheckpoint(hash: ByteString): F[Option[Checkpoint]]
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
      def createBlock: F[Option[BlockMessage]] = Applicative[F].pure[Option[BlockMessage]](None)
      def blockDag: F[BlockDag]                = BlockDag().pure[F]
      def tsCheckpoint(hash: ByteString): F[Option[Checkpoint]] =
        Applicative[F].pure[Option[Checkpoint]](None)
    }

  //TODO: figure out Casper key management for validators
  def hashSetCasper[
      F[_]: Monad: Capture: NodeDiscovery: TransportLayer: Log: Time: Encryption: KeysStore: ErrorHandler](
      storageLocation: Path,
      storageSize: Long,
      genesis: BlockMessage)(implicit scheduler: Scheduler): MultiParentCasper[F] =
    //TODO: Get rid of all the mutable data structures and write proper FP code
    new MultiParentCasper[F] {
      type BlockHash = ByteString
      type Validator = ByteString

      val _blockDag: AtomicSyncVar[BlockDag] = new AtomicSyncVar(
        BlockDag().copy(
          blockLookup = HashMap[BlockHash, BlockMessage](genesis.blockHash -> genesis))
      )

      val checkpoints: AtomicSyncVar[Map[ByteString, Checkpoint]] = new AtomicSyncVar(
        InterpreterUtil
          .computeBlockCheckpoint(
            genesis,
            genesis,
            _blockDag.get,
            storageLocation,
            storageSize,
            HashMap.empty[ByteString, Checkpoint]
          )
          ._2
      )

      private val blockBuffer: mutable.HashSet[BlockMessage] =
        new mutable.HashSet[BlockMessage]()
      private val deployHist: mutable.HashSet[Deploy] = new mutable.HashSet[Deploy]()

      def addBlock(b: BlockMessage): F[Unit] = attemptAdd(b).flatMap {
        case None => ().pure[F]

        case Some(success) =>
          if (success)
            Log[F]
              .info(s"CASPER: Added ${PrettyPrinter.buildString(b.blockHash)}") *> reAttemptBuffer
          else Capture[F].capture { blockBuffer += b }
      }

      def contains(b: BlockMessage): F[Boolean] =
        Capture[F].capture {
          _blockDag.get.blockLookup.contains(b.blockHash)
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

      def createBlock: F[Option[BlockMessage]] = {
        /*
         * Logic:
         *  -Score each of the blockDAG heads extracted from the block messages via GHOST
         *  -Let P = subset of heads such that P contains no conflicts and the total score is maximized
         *  -Let R = subset of deploy messages which are not included in DAG obtained by following blocks in P
         *  -If R is non-empty then create a new block with parents equal to P and (non-conflicting) txns obtained from R
         *  -Else if R is empty and |P| > 1 then create a block with parents equal to P and no transactions
         *  -Else None
         */

        val orderedHeads = estimator

        val p = orderedHeads.map(_.foldLeft(List.empty[BlockMessage]) {
          case (acc, b) =>
            if (acc.forall(!conflicts(_, b, genesis, _blockDag.get))) {
              b :: acc
            } else {
              acc
            }
        })
        val r = p.map(blocks => {
          val remDeploys = deployHist.clone()
          DagOperations
            .bfTraverse(blocks)(parents(_).iterator.map(_blockDag.get.blockLookup))
            .foreach(b => {
              b.body.foreach(_.newCode.foreach(remDeploys -= _))
            })
          remDeploys.toSeq
        })

        val proposal = p.flatMap(parents => {
          val justifications = justificationProto(_blockDag.get.latestMessages)
          r.map(requests => {
            if (requests.nonEmpty || parents.length > 1) {
              val Right((tsCheckpoint, _)) =
                checkpoints.mapAndUpdate[(Checkpoint, Map[ByteString, Checkpoint])](
                  InterpreterUtil.computeDeploysCheckpoint(
                    parents,
                    requests,
                    genesis,
                    _blockDag.get,
                    storageLocation,
                    storageSize,
                    _
                  ),
                  _._2
                )
              val postState = RChainState()
                .withTuplespace(tsCheckpoint.hash)
                .withBonds(bonds(parents.head))
                .withBlockNumber(blockNumber(parents.head) + 1)
              //TODO: include reductions
              val body = Body()
                .withPostState(postState)
                .withNewCode(requests)
              val header = blockHeader(body, parents.map(_.blockHash))
              val block  = unsignedBlockProto(body, header, justifications)

              Some(block)
            } else {
              None
            }
          })
        })

        proposal
      }

      def blockDag: F[BlockDag] = Capture[F].capture { _blockDag.get }

      def tsCheckpoint(hash: ByteString): F[Option[Checkpoint]] = Capture[F].capture {
        checkpoints.get.get(hash)
      }

      //invalid blocks return None
      private def attemptAdd(block: BlockMessage): F[Option[Boolean]] =
        Monad[F]
          .pure[BlockMessage](block)
          .map(b => {
            val hash     = b.blockHash
            val bParents = parents(b)

            if (bParents.exists(p => !_blockDag.get.blockLookup.contains(p))) {
              //Cannot add a block if not all parents are known,
              //but can re-try in the future
              Some(false)
            } else if (b.justifications.exists(j =>
                         !_blockDag.get.blockLookup.contains(j.latestBlockHash))) {
              //Cannot add a block if not all justifications are known,
              //but can re-try in the future
              Some(false)
            } else {
              //Check that tuplespace hash is valid
              val Right((maybeCheckPoint, _)) =
                checkpoints.mapAndUpdate[(Option[Checkpoint], Map[ByteString, Checkpoint])](
                  InterpreterUtil.validateBlockCheckpoint(
                    b,
                    genesis,
                    _blockDag.get,
                    storageLocation,
                    storageSize,
                    _
                  ),
                  _._2
                )
              //TODO: check if block is an equivocation and update observed faults

              maybeCheckPoint.map(_ => {
                _blockDag.update(bd => {
                  //add current block as new child to each of its parents
                  val newChildMap = bParents.foldLeft(bd.childMap) {
                    case (acc, p) =>
                      val currChildren = acc.getOrElse(p, HashSet.empty[BlockHash])
                      acc.updated(p, currChildren + hash)
                  }

                  bd.copy(
                    blockLookup = bd.blockLookup.updated(hash, b),
                    //Assume that a non-equivocating validator must include
                    //its own latest message in the justification. Therefore,
                    //for a given validator the blocks are guaranteed to arrive in causal order.
                    latestMessages = bd.latestMessages.updated(b.sender, hash),
                    childMap = newChildMap
                  )
                })

                true
              })
            }
          })
          .flatMap {
            case t @ Some(true) =>
              //Add successful! Send block to peers and create tuplespace checkpoint
              CommUtil.sendBlock[F](block) *> Monad[F].pure[Option[Boolean]](t)

            //TODO: Ask peers for missing parents/justifications of blocks
            case f @ Some(false) => Monad[F].pure[Option[Boolean]](f)

            case _ =>
              Log[F].info(
                s"CASPER: Did not add invalid block ${PrettyPrinter.buildString(block.blockHash)}") *>
                Monad[F].pure[Option[Boolean]](None)
          }

      private def reAttemptBuffer: F[Unit] = {
        def findAddedBlockMessages(attempts: List[(BlockMessage, Boolean)]): List[BlockMessage] =
          attempts.filter(_._2).map(_._1)

        for {
          attempts <- blockBuffer.toList.traverse(b => attemptAdd(b).map(succ => b -> succ))
          validAttempts = attempts.flatMap {
            case (b, None) =>
              //do not re-attempt invalid blocks
              blockBuffer -= b
              None

            case (b, Some(success)) =>
              Some(b -> success)
          }
          _ <- findAddedBlockMessages(validAttempts) match {
                case Nil => ().pure[F]
                case addedBlocks =>
                  Capture[F].capture { addedBlocks.map { blockBuffer -= _ } } *> reAttemptBuffer
              }
        } yield ()
      }
    }
}
