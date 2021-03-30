package coop.rchain.blockstorage

import cats.data.EitherT
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.crypto.codec.Base16

import java.nio.file.Path

sealed abstract class StorageError extends Exception

final case class CheckpointsDoNotStartFromZero(sortedCheckpoints: List[Path]) extends StorageError
final case class CheckpointsAreNotConsecutive(sortedCheckpoints: List[Path])  extends StorageError
final case class TopoSortLengthIsTooBig(length: Long)                         extends StorageError
final case class TopoSortFragmentParameterError(startBlockNumber: Long, endBlockNumber: Long)
    extends StorageError
final case class BlockSenderIsMalformed(block: BlockMessage) extends StorageError
final case object LastFinalizedBlockIsCorrupted              extends StorageError

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
      case TopoSortFragmentParameterError(startBlockNumber, endBlockNumber) =>
        s"Topological sorting of the dag with bad parameter ${startBlockNumber} and ${endBlockNumber}."
      case BlockSenderIsMalformed(block) =>
        s"Block ${Base16.encode(block.blockHash.toByteArray)} sender is malformed: ${Base16.encode(block.sender.toByteArray)}"
      case LastFinalizedBlockIsCorrupted =>
        "Last finalized block file is corrupted"
    }

  implicit class StorageErrorToMessage(storageError: StorageError) {
    val message: String = StorageError.errorMessage(storageError)
  }
}
