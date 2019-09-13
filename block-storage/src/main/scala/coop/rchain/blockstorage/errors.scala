package coop.rchain.blockstorage

import java.nio.file.Path

import cats.data.EitherT
import coop.rchain.casper.protocol.{BlockMessage, BlockMessageProto}
import coop.rchain.crypto.codec.Base16

sealed abstract class StorageError extends Exception

final case class CheckpointsDoNotStartFromZero(sortedCheckpoints: List[Path]) extends StorageError
final case class CheckpointsAreNotConsecutive(sortedCheckpoints: List[Path])  extends StorageError
final case class TopoSortLengthIsTooBig(length: Long)                         extends StorageError
final case class BlockSenderIsMalformed(block: BlockMessage)                  extends StorageError
final case class CheckpointDoesNotExist(offset: Long)                         extends StorageError
final case object LatestMessagesLogIsMalformed                                extends StorageError
final case object EquivocationsTrackerLogIsMalformed                          extends StorageError
final case object LatestMessagesLogIsCorrupted                                extends StorageError
final case object DataLookupIsCorrupted                                       extends StorageError
final case object InvalidBlocksIsCorrupted                                    extends StorageError
final case object BlockHashesByDeployLogIsCorrupted                           extends StorageError

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
      case LatestMessagesLogIsMalformed =>
        "Latest messages log is malformed"
      case EquivocationsTrackerLogIsMalformed =>
        "Equivocations tracker log is malformed"
      case LatestMessagesLogIsCorrupted =>
        "Latest messages log is corrupted"
      case DataLookupIsCorrupted =>
        "Data lookup log is corrupted"
      case InvalidBlocksIsCorrupted =>
        "Invalid blocks log is corrupted"
      case BlockHashesByDeployLogIsCorrupted =>
        "Block hashes by deploy log is corrupted"
    }

  implicit class StorageErrorToMessage(storageError: StorageError) {
    val message: String = StorageError.errorMessage(storageError)
  }
}
