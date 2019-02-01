package coop.rchain.blockstorage

import java.nio.file.Path

import cats.Functor
import cats.data.EitherT
import coop.rchain.blockstorage.util.io.IOError
import coop.rchain.blockstorage.util.io.IOError.IOErrT
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.crypto.codec.Base16

sealed abstract class StorageError extends Exception

final case class CheckpointsDoNotStartFromZero(sortedCheckpoints: List[Path]) extends StorageError
final case class CheckpointsAreNotConsecutive(sortedCheckpoints: List[Path])  extends StorageError
final case class TopoSortLengthIsTooBig(length: Long)                         extends StorageError
final case class BlockSenderIsMalformed(block: BlockMessage)                  extends StorageError

sealed abstract class StorageIOError extends StorageError

final case class WrappedIOError(ioError: IOError) extends StorageIOError

final case class UnavailableReferencedCheckpoint(checkpointIndex: Int) extends StorageIOError

object StorageError {
  type StorageErr[A]        = Either[StorageError, A]
  type StorageErrT[F[_], A] = EitherT[F, StorageError, A]

  type StorageIOErr[A]        = Either[StorageIOError, A]
  type StorageIOErrT[F[_], A] = EitherT[F, StorageIOError, A]

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
      case WrappedIOError(ioError) =>
        ioError.message
      case UnavailableReferencedCheckpoint(checkpointIndex) =>
        s"Unavailable checkpoint: $checkpointIndex"
    }

  implicit class StorageErrorToMessage(storageError: StorageError) {
    val message: String = StorageError.errorMessage(storageError)
  }

  implicit class IOErrorTToStorageIOErrorT[F[_]: Functor, A](ioErrT: IOErrT[F, A]) {
    def toStorageIOErrT: StorageIOErrT[F, A] = ioErrT.leftMap(WrappedIOError.apply)
  }

  implicit def ioErrorTToStorageIoError[F[_]: Functor, A](
      ioError: IOErrT[F, A]
  ): StorageIOErrT[F, A] =
    ioError.leftMap(WrappedIOError.apply)
}
