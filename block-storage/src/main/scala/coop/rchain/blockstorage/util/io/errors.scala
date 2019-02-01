package coop.rchain.blockstorage.util.io

import java.io.{FileNotFoundException, IOException}
import java.nio.file.{
  AtomicMoveNotSupportedException,
  DirectoryNotEmptyException,
  FileAlreadyExistsException,
  NotDirectoryException
}

import cats.data.EitherT

sealed trait IOError

final case class FileSeekFailed(exception: IOException)                     extends IOError
final case class IntReadFailed(exception: IOException)                      extends IOError
final case class ByteArrayReadFailed(exception: IOException)                extends IOError
final case class IntWriteFailed(exception: IOException)                     extends IOError
final case class ByteArrayWriteFailed(exception: IOException)               extends IOError
final case class ClearFileFailed(exception: IOException)                    extends IOError
final case class ClosingFailed(exception: IOException)                      extends IOError
final case class FileNotFound(exception: FileNotFoundException)             extends IOError
final case class FileSecurityViolation(exception: SecurityException)        extends IOError
final case class FileIsNotDirectory(exception: NotDirectoryException)       extends IOError
final case class UnsupportedFileOperation(e: UnsupportedOperationException) extends IOError
final case class FileAlreadyExists(e: FileAlreadyExistsException)           extends IOError
final case class DirectoryNotEmpty(e: DirectoryNotEmptyException)           extends IOError
final case class AtomicMoveNotSupported(e: AtomicMoveNotSupportedException) extends IOError
final case class UnexpectedIOError(throwable: Throwable)                    extends IOError

object IOError {
  type IOErr[A]        = Either[IOError, A]
  type IOErrT[F[_], A] = EitherT[F, IOError, A]

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
      case UnexpectedIOError(t) =>
        val msg = Option(t.getMessage).getOrElse("")
        s"Unexpected IO error occurred: $msg"
    }

  implicit class IOErrorToMessage(ioError: IOError) {
    val message: String = errorMessage(ioError)
  }
}
