package coop.rchain.casper

import cats.{Applicative, Monad}
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.Resource.ResourceClass.ProduceResource
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.DagOperations
import coop.rchain.casper.util.ProtoUtil._
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.catscontrib.{Capture, IOUtil}
import coop.rchain.p2p.Network.{ErrorHandler, KeysStore}
import coop.rchain.p2p.effects._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.immutable
import scala.collection.immutable.{HashMap, HashSet}

trait Casper[F[_], A] {
  def addBlock(b: BlockMessage): F[Unit]
  def contains(b: BlockMessage): F[Boolean]
  def deploy(d: Deploy): F[Unit]
  def estimator: F[A]
  def proposeBlock: F[Option[BlockMessage]]
  def sendBlockWhenReady: F[Unit]
}

trait MultiParentCasper[F[_]] extends Casper[F, IndexedSeq[BlockMessage]]

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
      def proposeBlock: F[Option[BlockMessage]] = Applicative[F].pure[Option[BlockMessage]](None)
      def sendBlockWhenReady: F[Unit]           = ().pure[F]
    }

  //TODO: figure out Casper key management for validators
  def hashSetCasper[
      F[_]: Monad: Capture: NodeDiscovery: TransportLayer: Log: Time: Encryption: KeysStore: ErrorHandler](
      id: ByteString,
      genesis: BlockMessage = genesisBlock): MultiParentCasper[F] =
    //TODO: Get rid of all the mutable data structures and write proper FP code
    new MultiParentCasper[F] {
      type BlockHash = ByteString
      type Validator = ByteString

      var blockDag: BlockDag = BlockDag().copy(
        blockLookup = HashMap[BlockHash, BlockMessage](genesis.blockHash -> genesis))
      private val blockBuffer: mutable.HashSet[BlockMessage] =
        new mutable.HashSet[BlockMessage]()
      private val deployHist: mutable.HashSet[Deploy] = new mutable.HashSet[Deploy]()
      private val deployBuff: mutable.HashSet[Deploy] = new mutable.HashSet[Deploy]()

      def addBlock(b: BlockMessage): F[Unit] =
        for {
          success <- attemptAdd(b)
          _ <- if (success)
                Log[F].info(s"CASPER: Added ${PrettyPrinter.buildString(b)}") *> reAttemptBuffer
              else Capture[F].capture { blockBuffer += b }
        } yield ()

      def contains(b: BlockMessage): F[Boolean] =
        Capture[F].capture {
          blockDag.blockLookup.contains(b.blockHash)
        }

      def deploy(d: Deploy): F[Unit] =
        for {
          _ <- Capture[F].capture {
                deployBuff += d
                deployHist += d
              }
          _ <- Log[F].info(s"CASPER: Received ${PrettyPrinter.buildString(d)}")
          _ <- sendBlockWhenReady
        } yield ()

      def estimator: F[IndexedSeq[BlockMessage]] =
        Capture[F].capture {
          Estimator.tips(blockDag, genesis)
        }

      def proposeBlock: F[Option[BlockMessage]] = {
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

        //TODO: define conflict model for blocks
        //(maybe wait until we are actually dealing with rholang terms)
        val p = orderedHeads.map(_.take(1)) //for now, just take top choice
        val r = p.map(blocks => {
          val remDeploys = deployHist.clone()
          DagOperations
            .bfTraverse(blocks)(parents(_).iterator.map(blockDag.blockLookup))
            .foreach(b => {
              b.body.foreach(_.newCode.foreach(remDeploys -= _))
            })
          remDeploys
        })

        val proposal = p.flatMap(parents => {
          //TODO: Compute this properly
          val parentPostState = parents.head.body.get.postState.get
          val justifications  = justificationProto(blockDag.latestMessages)
          r.map(requests => {
            if (requests.isEmpty) {
              if (parents.length > 1) {
                val body = Body()
                  .withPostState(parentPostState)
                val header = blockHeader(body, parents.map(_.blockHash))
                val block  = blockProto(body, header, justifications, id)

                Some(block)
              } else {
                None
              }
            } else {
              //TODO: only pick non-conflicting deploys
              val deploys = requests.take(10).toSeq
              //TODO: compute postState properly
              val newPostState = parentPostState
                .withBlockNumber(parentPostState.blockNumber + 1)
                .withResources(deploys.map(_.resource.get))
              //TODO: include reductions
              val body = Body()
                .withPostState(newPostState)
                .withNewCode(deploys)
              val header = blockHeader(body, parents.map(_.blockHash))
              val block  = blockProto(body, header, justifications, id)

              Some(block)
            }
          })
        })

        proposal.flatMap {
          case mb @ Some(block) =>
            Log[F].info(s"CASPER: Proposed ${PrettyPrinter.buildString(block)}") *>
              addBlock(block) *>
              CommUtil.sendBlock[F](block) *>
              estimator
                .map(_.head)
                .flatMap(forkchoice =>
                  Log[F].info(
                    s"CASPER: New fork-choice is ${PrettyPrinter.buildString(forkchoice)}")) *>
              Monad[F].pure[Option[BlockMessage]](mb)
          case _ => Monad[F].pure[Option[BlockMessage]](None)
        }
      }

      def sendBlockWhenReady: F[Unit] =
        for {
          _ <- if (deployBuff.size < 10) {
                Monad[F].pure(()) //nothing to do yet
              } else {
                val clearBuff = Capture[F].capture { deployBuff.clear() }
                proposeBlock *> clearBuff
              }
        } yield ()

      private def attemptAdd(block: BlockMessage): F[Boolean] =
        Monad[F]
          .pure[BlockMessage](block)
          .map(b => {
            val hash     = b.blockHash
            val bParents = parents(b)

            if (bParents.exists(p => !blockDag.blockLookup.contains(p))) {
              //cannot add a block if not all parents are known
              false
            } else if (b.justifications.exists(j =>
                         !blockDag.blockLookup.contains(j.latestBlockHash))) {
              //cannot add a block if not all justifications are known
              false
            } else {
              //TODO: check if block is an equivocation and update observed faults

              blockDag = blockDag.copy(blockLookup = blockDag.blockLookup + (hash -> b))

              //Assume that a non-equivocating validator must include
              //its own latest message in the justification. Therefore,
              //for a given validator the blocks are guaranteed to arrive in causal order.
              blockDag =
                blockDag.copy(latestMessages = blockDag.latestMessages.updated(b.sender, hash))

              //add current block as new child to each of its parents
              val newChildMap = bParents.foldLeft(blockDag.childMap) {
                case (acc, p) =>
                  val currChildren = acc.getOrElse(p, HashSet.empty[BlockHash])
                  acc.updated(p, currChildren + hash)
              }
              blockDag = blockDag.copy(childMap = newChildMap)
              true
            }
          })

      private def reAttemptBuffer: F[Unit] = {
        val attempts   = blockBuffer.toList.traverse(b => attemptAdd(b).map(succ => b -> succ))
        val maybeAdded = attempts.map(_.find(_._2).map(_._1))

        maybeAdded.flatMap {
          case Some(added) =>
            Capture[F].capture { blockBuffer -= added } *> reAttemptBuffer

          case None => ().pure[F]
        }
      }
    }
}
