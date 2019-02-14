package coop.rchain.blockstorage

import java.nio.file.Path

import cats.data.EitherT
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.crypto.codec.Base16

sealed abstract class StorageError extends Exception

final case class CheckpointsDoNotStartFromZero(sortedCheckpoints: List[Path]) extends StorageError
final case class CheckpointsAreNotConsecutive(sortedCheckpoints: List[Path])  extends StorageError
final case class TopoSortLengthIsTooBig(length: Long)                         extends StorageError
final case class BlockSenderIsMalformed(block: BlockMessage)                  extends StorageError
final case class CheckpointDoesNotExist(offset: Long)                         extends StorageError

object StorageError {
  type StorageErr[A]        = Either[StorageError, A]
  type StorageErrT[F[_], A] = EitherT[F, StorageError, A]

  def errorMessage(ce: StorageError): String =
    ce match {
      case CheckpointsDoNotStartFromZero(sortedCheckpoints) =>
        s"Checkpoints do not start from block number 0: ${sortedCheckpoints.mkString(",")}"
      case CheckpointsAreNotConsecutive(sortedCheckpoints) =>
        s"Checkpoints are not consecutive: ${sortedCheckpoints.mkString(",")}"
      case TopoSortLengthIsTooBig(length) =>
        s"Topological sorting of length $length was requested while maximal length is ${Int.MaxValue}"
      case BlockSenderIsMalformed(block) =>
        s"Block ${Base16.encode(block.blockHash.toByteArray)} sender is malformed: ${Base16.encode(block.sender.toByteArray)}"
      case CheckpointDoesNotExist(offset) =>
        s"Requested a block with block number $offset, but there is no checkpoint for it"
    }

  implicit class StorageErrorToMessage(storageError: StorageError) {
    val message: String = StorageError.errorMessage(storageError)
  }
}
