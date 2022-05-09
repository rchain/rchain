package coop.rchain.blockstorage.dag
import cats.effect.Sync
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.syntax._
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models.syntax._
import coop.rchain.models.{BlockHash, BlockMetadata}

import scala.collection.immutable.SortedMap

final case class DagRepresentation(
    dagSet: Set[BlockHash],
    latestMessagesHashes: Map[Validator, BlockHash],
    childMap: Map[BlockHash, Set[BlockHash]],
    heightMap: SortedMap[Long, Set[BlockHash]],
    invalidBlocksSet: Set[BlockHash],
    lastFinalizedBlockHash: Option[BlockHash],
    finalizedBlocksSet: Set[BlockHash],
    dagMessageState: DagMessageState[BlockHash, Validator] =
      DagMessageState(ByteString.EMPTY, Set(), Map()) // TEMP default
) {
  def contains(blockHash: BlockHash): Boolean =
    blockHash.size == BlockHash.Length && dagSet.contains(blockHash)

  def children(blockHash: BlockHash): Option[Set[BlockHash]] = childMap.get(blockHash)

  def isFinalized(blockHash: BlockHash): Boolean = finalizedBlocksSet.contains(blockHash)

  def latestBlockNumber: Long = heightMap.lastOption.map { case (h, _) => h + 1 }.getOrElse(0L)

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
  def empty: DagRepresentation =
    DagRepresentation(
      Set.empty[BlockHash],
      Map.empty[Validator, BlockHash],
      Map.empty[BlockHash, Set[BlockHash]],
      SortedMap.empty[Long, Set[BlockHash]],
      Set.empty[BlockHash],
      none[BlockHash],
      Set.empty[BlockHash],
      DagMessageState[BlockHash, Validator](ByteString.EMPTY, Set(), Map())
    )

  def lastFinalizedBlockUnsafe[F[_]: Sync](dr: DagRepresentation): F[BlockHash] = {
    val errMsg =
      "DagState does not contain lastFinalizedBlock. Are you calling this on empty BlockDagStorage? Otherwise there is a bug."
    dr.lastFinalizedBlockHash.liftTo[F](new Exception(errMsg))
  }

  def invalidBlocks[F[_]: Sync: BlockDagStorage](dr: DagRepresentation): F[Set[BlockMetadata]] =
    dr.invalidBlocksSet.toList.traverse(BlockDagStorage[F].lookupUnsafe).map(_.toSet)

  def latestMessageHash[F[_]: Sync: BlockDagStorage](
      dr: DagRepresentation,
      validator: Validator
  ): F[Option[BlockHash]] =
    dr.latestMessagesHashes.get(validator).pure

  def latestMessageHashes[F[_]: Sync: BlockDagStorage](
      dr: DagRepresentation
  ): F[Map[Validator, BlockHash]] =
    dr.latestMessagesHashes.pure

  def latestMessages[F[_]: Sync: BlockDagStorage](
      dr: DagRepresentation
  ): F[Map[Validator, BlockMetadata]] =
    dr.latestMessagesHashes.toList
      .traverse { case (s, h) => BlockDagStorage[F].lookupUnsafe(h).map((s, _)) }
      .map(_.toMap)
}
