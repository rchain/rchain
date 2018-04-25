package coop.rchain.casper

import cats.{Applicative, Monad}, cats.implicits._

import com.google.protobuf.ByteString

import coop.rchain.casper.protocol.{BlockMessage, Resource}
import coop.rchain.catscontrib.{Capture, Queue}

import scala.collection.mutable

trait Casper[F[_], A] {
  def addBlock(b: BlockMessage): F[Unit]
  def contains(b: BlockMessage): F[Boolean]
  def deploy(r: Resource): F[Unit]
  def estimator: F[A]
  def proposeBlock: F[Option[BlockMessage]]
}

trait MultiParentCasper[F[_]] extends Casper[F, IndexedSeq[BlockMessage]]

object MultiParentCasper extends MultiParentCasperInstances {
  def apply[F[_]](implicit instance: MultiParentCasper[F]): MultiParentCasper[F] = instance
}

sealed abstract class MultiParentCasperInstances {
  def noCasper[F[_]: Applicative]: MultiParentCasper[F] = new MultiParentCasper[F] {
    def addBlock(b: BlockMessage): F[Unit]    = ().pure[F]
    def contains(b: BlockMessage): F[Boolean] = false.pure[F]
    def deploy(r: Resource): F[Unit]          = ().pure[F]
    def estimator: F[IndexedSeq[BlockMessage]] =
      Applicative[F].pure[IndexedSeq[BlockMessage]](Vector(BlockMessage()))
    def proposeBlock: F[Option[BlockMessage]] = Applicative[F].pure[Option[BlockMessage]](None)
  }

  def hashSetCasper[F[_]: Monad: Capture]: MultiParentCasper[F] = new MultiParentCasper[F] {
    type BlockHash = ByteString
    type Validator = ByteString

    private val _childlessBlocks: mutable.HashSet[BlockHash] = new mutable.HashSet[BlockHash]()
    private val _latestMessages: mutable.HashMap[Validator, BlockHash] =
      new mutable.HashMap[Validator, BlockHash]()
    private val blockLookup: mutable.HashMap[BlockHash, BlockMessage] =
      new mutable.HashMap[BlockHash, BlockMessage]()
    private val blockBuffer: mutable.HashSet[BlockMessage] =
      new mutable.HashSet[BlockMessage]()

    def addBlock(b: BlockMessage): F[Unit] =
      for {
        success <- attemptAdd(b)
        _       <- if (success) reAttemptBuffer else Capture[F].capture { blockBuffer += b }
      } yield ()

    def contains(b: BlockMessage): F[Boolean] = Capture[F].capture {
      blockLookup.contains(b.blockHash)
    }

    def deploy(r: Resource): F[Unit]           = ???
    def estimator: F[IndexedSeq[BlockMessage]] = ???
    def proposeBlock: F[Option[BlockMessage]]  = ???

    def attemptAdd(b: BlockMessage): F[Boolean] = {
      val hash    = b.blockHash
      val parents = b.header.map(_.parentsHashList).getOrElse(List.empty[BlockHash])

      if (parents.exists(p => !blockLookup.contains(p))) {
        //cannot add a block if not all parents are known
        false.pure[F]
      } else if (b.justifications.exists(j => !blockLookup.contains(j.latestBlockHash))) {
        //cannot add a block if not all justifications are known
        false.pure[F]
      } else {
        //TODO: check if block is an equivocation and update observed faults

        Capture[F].capture {
          blockLookup += (hash -> b) //add new block to total set

          //Assume that a non-equivocating validator must include
          //its own latest message in the justification. Therefore,
          //for a given validator the blocks are guarenteed to arrive in causal order.
          _latestMessages.update(b.sender, hash)

          //remove blocks that are now no longer childless
          parents.filter(p => _childlessBlocks.contains(p)).foreach(_childlessBlocks -= _)

          //b must be childless because all parents of a block must already be in the DAG when it is added
          _childlessBlocks += hash
        } *> true.pure[F]
      }
    }

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
