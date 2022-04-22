package coop.rchain.blockstorage.dag
import cats.Applicative
import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.blockstorage.TopoSortFragmentParameterError
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
    finalizedBlocksSet: Set[BlockHash]
)

object DagRepresentation {
  def empty: DagRepresentation =
    DagRepresentation(
      Set.empty[BlockHash],
      Map.empty[Validator, BlockHash],
      Map.empty[BlockHash, Set[BlockHash]],
      SortedMap.empty[Long, Set[BlockHash]],
      Set.empty[BlockHash],
      none[BlockHash],
      Set.empty[BlockHash]
    )

  def contains[F[_]: Applicative](dr: DagRepresentation, blockHash: BlockHash): F[Boolean] =
    (blockHash.size == BlockHash.Length && dr.dagSet.contains(blockHash)).pure

  def children[F[_]: Applicative](
      dr: DagRepresentation,
      blockHash: BlockHash
  ): F[Option[Set[BlockHash]]] =
    dr.childMap.get(blockHash).pure

  def lastFinalizedBlock[F[_]: Sync](dr: DagRepresentation): F[BlockHash] = {
    val errMsg =
      "DagState does not contain lastFinalizedBlock. Are you calling this on empty BlockDagStorage? Otherwise there is a bug."
    dr.lastFinalizedBlockHash.liftTo[F](new Exception(errMsg))
  }

  def isFinalized[F[_]: Applicative](dr: DagRepresentation, blockHash: BlockHash): F[Boolean] =
    dr.finalizedBlocksSet.contains(blockHash).pure

  private def getMaxHeight(dr: DagRepresentation) =
    if (dr.heightMap.nonEmpty) dr.heightMap.last._1 + 1L else 0L

  def getHeightMap[F[_]: Applicative](dr: DagRepresentation): F[SortedMap[Long, Set[BlockHash]]] =
    dr.heightMap.pure[F]

  def latestBlockNumber[F[_]: Applicative](dr: DagRepresentation): F[Long] = getMaxHeight(dr).pure

  def topoSort[F[_]: Sync](
      dr: DagRepresentation,
      startBlockNumber: Long,
      maybeEndBlockNumber: Option[Long]
  ): F[Vector[Vector[BlockHash]]] = {
    val maxNumber   = getMaxHeight(dr)
    val startNumber = Math.max(0, startBlockNumber)
    val endNumber   = maybeEndBlockNumber.map(Math.min(maxNumber, _)).getOrElse(maxNumber)
    if (startNumber >= 0 && startNumber <= endNumber) {
      Sync[F].delay(
        dr.heightMap
          .filterKeys(h => h >= startNumber && h <= endNumber)
          .map { case (_, v) => v.toVector }
          .toVector
      )
    } else {
      Sync[F].raiseError(
        TopoSortFragmentParameterError(startNumber, endNumber)
      )
    }
  }

  def find[F[_]: Sync](dr: DagRepresentation, truncatedHash: String): F[Option[BlockHash]] =
    Sync[F].delay {
      if (truncatedHash.length % 2 == 0) {
        val truncatedByteString = truncatedHash.unsafeHexToByteString
        dr.dagSet.find(hash => hash.startsWith(truncatedByteString))
      } else {
        // if truncatedHash is odd length string we cannot convert it to ByteString with 8 bit resolution
        // because each symbol has 4 bit resolution. Need to make a string of even length by removing the last symbol,
        // then find all the matching hashes and choose one that matches the full truncatedHash string
        val truncatedByteString = truncatedHash.dropRight(1).unsafeHexToByteString
        dr.dagSet
          .filter(_.startsWith(truncatedByteString))
          .find(_.toHexString.startsWith(truncatedHash))
      }
    }

  def invalidBlocks[F[_]: Sync: BlockDagStorage](dr: DagRepresentation): F[Set[BlockMetadata]] =
    dr.invalidBlocksSet.toList.traverse(BlockDagStorage[F].lookupUnsafe).map(_.toSet)

  def latestMessageHash[F[_]: Sync: BlockDagStorage](
      dr: DagRepresentation,
      validator: Validator
  ): F[Option[BlockHash]] =
    dr.latestMessagesHashes.find(_._1 == validator).map(_._2).pure

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
