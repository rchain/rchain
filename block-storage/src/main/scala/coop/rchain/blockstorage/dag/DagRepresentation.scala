package coop.rchain.blockstorage.dag

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.blockstorage.syntax._
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models.syntax._
import coop.rchain.models.{BlockHash, BlockMetadata}

import scala.collection.immutable.SortedMap

final case class DagRepresentation(
    dagSet: Set[BlockHash],
    childMap: Map[BlockHash, Set[BlockHash]],
    heightMap: SortedMap[Long, Set[BlockHash]],
    dagMessageState: DagMessageState[BlockHash, Validator]
) {
  // TODO: pick highest block from fringe until LFB is replaced with fringe completely
  lazy val lastFinalizedBlockHash: Option[BlockHash] =
    dagMessageState.latestFringe.toList.sortBy(_.height).lastOption.map(_.id)

  lazy val finalizedBlocksSet: Set[BlockHash] = {
    val latestFringe = dagMessageState.latestFringe
    latestFringe.flatMap(_.seen)
  }

  lazy val latestBlockNumber: Long = heightMap.lastOption.map { case (h, _) => h + 1 }.getOrElse(0L)

  def contains(blockHash: BlockHash): Boolean =
    blockHash.size == BlockHash.Length && dagSet.contains(blockHash)

  def children(blockHash: BlockHash): Option[Set[BlockHash]] = childMap.get(blockHash)

  def isFinalized(blockHash: BlockHash): Boolean = finalizedBlocksSet.contains(blockHash)

  def topoSort(
      startBlockNumber: Long,
      maybeEndBlockNumber: Option[Long]
  ): Option[Vector[Vector[BlockHash]]] = {
    val maxNumber   = latestBlockNumber
    val startNumber = Math.max(0, startBlockNumber)
    val endNumber   = maybeEndBlockNumber.map(Math.min(maxNumber, _)).getOrElse(maxNumber)
    val validRange  = startNumber >= 0 && startNumber <= endNumber
    validRange.guard[Option].as {
      heightMap
        .filterKeys(h => h >= startNumber && h <= endNumber)
        .map { case (_, v) => v.toVector }
        .toVector
    }
  }

  def find(truncatedHash: String): Option[BlockHash] =
    if (truncatedHash.length % 2 == 0) {
      val truncatedByteString = truncatedHash.unsafeHexToByteString
      dagSet.find(hash => hash.startsWith(truncatedByteString))
    } else {
      // if truncatedHash is odd length string we cannot convert it to ByteString with 8 bit resolution
      // because each symbol has 4 bit resolution. Need to make a string of even length by removing the last symbol,
      // then find all the matching hashes and choose one that matches the full truncatedHash string
      val truncatedByteString = truncatedHash.dropRight(1).unsafeHexToByteString
      dagSet.filter(_.startsWith(truncatedByteString)).find(_.toHexString.startsWith(truncatedHash))
    }
}

object DagRepresentation {
  def lastFinalizedBlockUnsafe[F[_]: Sync](dr: DagRepresentation): F[BlockHash] = {
    val errMsg =
      "DagState does not contain lastFinalizedBlock. Are you calling this on empty BlockDagStorage? Otherwise there is a bug."
    dr.lastFinalizedBlockHash.liftTo[F](new Exception(errMsg))
  }

  def latestMessageHash[F[_]: Sync: BlockDagStorage](
      dr: DagRepresentation,
      validator: Validator
  ): F[Option[BlockHash]] =
    dr.dagMessageState.latestMsgs.filter(_.sender == validator).map(_.id).headOption.pure

  def latestMessageHashes[F[_]: Sync: BlockDagStorage](
      dr: DagRepresentation
  ): F[Map[Validator, BlockHash]] =
    dr.dagMessageState.latestMsgs.map(m => (m.sender, m.id)).toMap.pure
}
