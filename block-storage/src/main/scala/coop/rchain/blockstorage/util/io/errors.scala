package coop.rchain.blockstorage.util.io

import java.io.{EOFException, FileNotFoundException, IOException}
import java.nio.file._

import cats.Functor
import cats.effect.Sync
import cats.mtl.FunctorRaise

sealed trait IOError

final case class FileSeekFailed(exception: IOException)                     extends IOError
final case class IntReadFailed(exception: IOException)                      extends IOError
final case class ByteArrayReadFailed(exception: IOException)                extends IOError
final case class IntWriteFailed(exception: IOException)                     extends IOError
final case class ByteArrayWriteFailed(exception: IOException)               extends IOError
final case class SetLengthFailed(exception: IOException)                    extends IOError
final case class ClosingFailed(exception: IOException)                      extends IOError
final case class StreamFlushFailed(e: IOException)                          extends IOError
final case class FileNotFound(exception: FileNotFoundException)             extends IOError
final case class FileSecurityViolation(exception: SecurityException)        extends IOError
final case class FileIsNotDirectory(exception: NotDirectoryException)       extends IOError
final case class UnsupportedFileOperation(e: UnsupportedOperationException) extends IOError
final case class IllegalFileOperation(e: IllegalArgumentException)          extends IOError
final case class FileAlreadyExists(e: FileAlreadyExistsException)           extends IOError
final case class DirectoryNotEmpty(e: DirectoryNotEmptyException)           extends IOError
final case class AtomicMoveNotSupported(e: AtomicMoveNotSupportedException) extends IOError
final case class EndOfFile(e: EOFException)                                 extends IOError
final case class StreamWriteFailed(e: IOException)                          extends IOError
final case class FileReadFailed(e: IOException)                             extends IOError
final case class FileWriteFailed(e: IOException)                            extends IOError
final case class UnexpectedIOError(throwable: Throwable)                    extends IOError

final case class UnavailableReferencedCheckpoint(checkpointIndex: Int) extends IOError

object IOError {
  type RaiseIOError[F[_]] = FunctorRaise[F, IOError]

  object RaiseIOError {
    def apply[F[_]](implicit ev: RaiseIOError[F]): RaiseIOError[F] = ev
  }

  def raiseIOErrorThroughSync[F[_]: Sync]: FunctorRaise[F, IOError] =
    new FunctorRaise[F, IOError] {
      override val functor: Functor[F] =
        Functor[F]

      override def raise[A](e: IOError): F[A] =
        Sync[F].raiseError(IOError.ExceptionWrapper(e))
    }

  final case class ExceptionWrapper(e: IOError) extends Exception

  def errorMessage(error: IOError): String =
    error match {
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
      case SetLengthFailed(e) =>
        val msg = Option(e.getMessage).getOrElse("")
        s"Set file length failed: $msg"
      case ClosingFailed(e) =>
        val msg = Option(e.getMessage).getOrElse("")
        s"File closing failed: $msg"
      case StreamFlushFailed(e) =>
        val msg = Option(e.getMessage).getOrElse("")
        s"Stream flush failed: $msg"
      case FileNotFound(e) =>
        val msg = Option(e.getMessage).getOrElse("")
        s"File not found: $msg"
      case FileSecurityViolation(e) =>
        val msg = Option(e.getMessage).getOrElse("")
        s"Security manager denied access to file: $msg"
      case FileIsNotDirectory(e) =>
        val msg = Option(e.getMessage).getOrElse("")
        s"File is not a directory: $msg"
      case UnsupportedFileOperation(e) =>
        val msg = Option(e.getMessage).getOrElse("")
        s"Unsupported file operation: $msg"
      case IllegalFileOperation(e) =>
        val msg = Option(e.getMessage).getOrElse("")
        s"Illegal file operation: $msg"
      case FileAlreadyExists(e) =>
        val msg = Option(e.getMessage).getOrElse("")
        s"File already exists: $msg"
      case DirectoryNotEmpty(e) =>
        val msg = Option(e.getMessage).getOrElse("")
        s"Directory is not empty: $msg"
      case AtomicMoveNotSupported(e) =>
        val msg = Option(e.getMessage).getOrElse("")
        s"Atomic move is not supported: $msg"
      case EndOfFile(e) =>
        val msg = Option(e.getMessage).getOrElse("")
        s"End of file: $msg"
      case UnexpectedIOError(t) =>
        val msg = Option(t.getMessage).getOrElse("")
        s"Unexpected IO error occurred: $msg"
      case StreamWriteFailed(t) =>
        val msg = Option(t.getMessage).getOrElse("")
        s"Stream write failed: $msg"
      case FileReadFailed(t) =>
        val msg = Option(t.getMessage).getOrElse("")
        s"File read failed: $msg"
      case FileWriteFailed(t) =>
        val msg = Option(t.getMessage).getOrElse("")
        s"File write failed: $msg"
      case UnavailableReferencedCheckpoint(checkpointIndex) =>
        s"Unavailable checkpoint: $checkpointIndex"
    }

  implicit class IOErrorToMessage(ioError: IOError) {
    val message: String = errorMessage(ioError)
  }
}
