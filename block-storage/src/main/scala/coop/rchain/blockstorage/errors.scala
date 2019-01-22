package coop.rchain.blockstorage
import java.io.{FileNotFoundException, IOException}
import java.nio.file._

import cats.data.EitherT
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.crypto.codec.Base16

sealed abstract class StorageError extends Exception

final case class CheckpointsDoNotStartFromZero(sortedCheckpoints: List[Path]) extends StorageError
final case class CheckpointsAreNotConsecutive(sortedCheckpoints: List[Path])  extends StorageError
final case class TopoSortLengthIsTooBig(length: Long)                         extends StorageError
final case class BlockSenderIsMalformed(block: BlockMessage)                  extends StorageError

sealed abstract class StorageIOError extends StorageError

final case class FileSeekFailed(exception: IOException)                     extends StorageIOError
final case class IntReadFailed(exception: IOException)                      extends StorageIOError
final case class ByteArrayReadFailed(exception: IOException)                extends StorageIOError
final case class IntWriteFailed(exception: IOException)                     extends StorageIOError
final case class ByteArrayWriteFailed(exception: IOException)               extends StorageIOError
final case class ClearFileFailed(exception: IOException)                    extends StorageIOError
final case class ClosingFailed(exception: IOException)                      extends StorageIOError
final case class FileNotFound(exception: FileNotFoundException)             extends StorageIOError
final case class FileSecurityViolation(exception: SecurityException)        extends StorageIOError
final case class FileIsNotDirectory(exception: NotDirectoryException)       extends StorageIOError
final case class UnavailableReferencedCheckpoint(checkpointIndex: Int)      extends StorageIOError
final case class UnsupportedFileOperation(e: UnsupportedOperationException) extends StorageIOError
final case class FileAlreadyExists(e: FileAlreadyExistsException)           extends StorageIOError
final case class DirectoryNotEmpty(e: DirectoryNotEmptyException)           extends StorageIOError
final case class AtomicMoveNotSupported(e: AtomicMoveNotSupportedException) extends StorageIOError
final case class UnexpectedIOStorageError(throwable: Throwable)             extends StorageIOError

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
        s"Block ${Base16.encode(block.blockHash.toByteArray)} sender is malformed: ${Base16.encode(
          block.sender.toByteArray)}"
      case FileSeekFailed(e) =>
        val msg = Option(e.getMessage).getOrElse("")
        s"File seek failed: $msg"
      case IntReadFailed(e) =>
        val msg = Option(e.getMessage).getOrElse("")
        s"Int read failed: $msg"
      case ByteArrayReadFailed(e) =>
        val msg = Option(e.getMessage).getOrElse("")
        s"Byte array read failed: $msg"
      case IntWriteFailed(e) =>
        val msg = Option(e.getMessage).getOrElse("")
        s"Int write failed: $msg"
      case ByteArrayWriteFailed(e) =>
        val msg = Option(e.getMessage).getOrElse("")
        s"Byte array write failed: $msg"
      case ClearFileFailed(e) =>
        val msg = Option(e.getMessage).getOrElse("")
        s"File clearing failed: $msg"
      case ClosingFailed(e) =>
        val msg = Option(e.getMessage).getOrElse("")
        s"File closing failed: $msg"
      case FileNotFound(e) =>
        val msg = Option(e.getMessage).getOrElse("")
        s"File not found: $msg"
      case FileSecurityViolation(e) =>
        val msg = Option(e.getMessage).getOrElse("")
        s"Security manager denied access to file: $msg"
      case FileIsNotDirectory(e) =>
        val msg = Option(e.getMessage).getOrElse("")
        s"File is not a directory: $msg"
      case UnavailableReferencedCheckpoint(checkpointIndex) =>
        s"Unavailable checkpoint: $checkpointIndex"
      case UnexpectedIOStorageError(t) =>
        val msg = Option(t.getMessage).getOrElse("")
        s"Unexpected error ocurred during reading: $msg"
    }

  implicit class StorageErrorToMessage(storageError: StorageError) {
    val message: String = StorageError.errorMessage(storageError)
  }
}
