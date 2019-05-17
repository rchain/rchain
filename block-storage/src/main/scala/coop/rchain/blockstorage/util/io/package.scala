package coop.rchain.blockstorage.util

import java.io.IOException
import java.nio.file._
import java.util.stream.Collectors

import cats.implicits._
import cats.effect.Sync
import coop.rchain.blockstorage.util.io.IOError.RaiseIOError

import scala.collection.JavaConverters._
import scala.util.control.NonFatal

package object io {
  private[io] def handleIo[F[_]: Sync: RaiseIOError, A](
      io: => A,
      handleIoException: IOException => IOError
  ): F[A] =
    handleIoF(io, e => RaiseIOError[F].raise[A](handleIoException(e)))

  private[io] def handleIoF[F[_]: Sync: RaiseIOError, A](
      io: => A,
      handleIoException: IOException => F[A]
  ): F[A] =
    Sync[F].defer {
      try {
        io.pure[F]
      } catch {
        case e: IOException =>
          handleIoException(e)
        case e: SecurityException =>
          RaiseIOError[F].raise[A](FileSecurityViolation(e))
        case e: UnsupportedOperationException =>
          RaiseIOError[F].raise[A](UnsupportedFileOperation(e))
        case e: IllegalArgumentException =>
          RaiseIOError[F].raise[A](IllegalFileOperation(e))
        case NonFatal(e) =>
          RaiseIOError[F].raise[A](UnexpectedIOError(e))
      }
    }

  def moveFile[F[_]: Sync: RaiseIOError](
      from: Path,
      to: Path,
      options: CopyOption*
  ): F[Path] =
    handleIo(
      Files.move(from, to, options: _*), {
        case e: FileAlreadyExistsException =>
          FileAlreadyExists(e)
        case e: DirectoryNotEmptyException =>
          DirectoryNotEmpty(e)
        case e: AtomicMoveNotSupportedException =>
          AtomicMoveNotSupported(e)
        case NonFatal(e) =>
          UnexpectedIOError(e)
      }
    )

  def isDirectory[F[_]: Sync: RaiseIOError](path: Path): F[Boolean] =
    handleIo(Files.isDirectory(path), UnexpectedIOError.apply)

  def isRegularFile[F[_]: Sync: RaiseIOError](path: Path): F[Boolean] =
    handleIo(Files.isRegularFile(path), UnexpectedIOError.apply)

  def notExists[F[_]: Sync: RaiseIOError](path: Path): F[Boolean] =
    handleIo(Files.notExists(path), UnexpectedIOError.apply)

  def exists[F[_]: Sync: RaiseIOError](path: Path): F[Boolean] =
    handleIo(Files.exists(path), UnexpectedIOError.apply)

  def makeDirectory[F[_]: Sync: RaiseIOError](dirPath: Path): F[Path] =
    handleIo(Files.createDirectories(dirPath), UnexpectedIOError.apply)

  def listInDirectory[F[_]: Sync: RaiseIOError](dirPath: Path): F[List[Path]] =
    for {
      files <- handleIo(Files.list(dirPath), {
                case e: NotDirectoryException => FileIsNotDirectory(e)
                case e                        => UnexpectedIOError(e)
              })
      filesList = files
        .collect(Collectors.toList[Path])
        .asScala
        .toList
    } yield filesList

  def listRegularFiles[F[_]: Sync: RaiseIOError](dirPath: Path): F[List[Path]] =
    for {
      files        <- listInDirectory(dirPath)
      regularFiles <- files.filterA[F](isRegularFile[F])
    } yield regularFiles

  def createTemporaryFile[F[_]: Sync: RaiseIOError](prefix: String, suffix: String): F[Path] =
    handleIo(Files.createTempFile(prefix, suffix), UnexpectedIOError.apply)

  def createSameDirectoryTemporaryFile[F[_]: Sync: RaiseIOError](original: Path): F[Path] = {
    val tmpFile = original.resolveSibling(original.getFileName + ".tmp")
    deleteIfExists(tmpFile) >> createFile(tmpFile)
  }

  def createNewFile[F[_]: Sync: RaiseIOError](filePath: Path): F[Boolean] =
    handleIo(filePath.toFile.createNewFile(), UnexpectedIOError.apply)

  def createFile[F[_]: Sync: RaiseIOError](filePath: Path): F[Path] =
    handleIo(Files.createFile(filePath), UnexpectedIOError.apply)

  def deleteIfExists[F[_]: Sync: RaiseIOError](filePath: Path): F[Boolean] =
    handleIo(Files.deleteIfExists(filePath), {
      case e: DirectoryNotEmptyException => DirectoryNotEmpty(e)
      case e                             => UnexpectedIOError(e)
    })

  def writeToFile[F[_]: Sync: RaiseIOError](
      filePath: Path,
      bytes: Array[Byte],
      options: OpenOption*
  ): F[Path] =
    handleIo(Files.write(filePath, bytes, options: _*), FileWriteFailed.apply)

  def readAllBytesFromFile[F[_]: Sync: RaiseIOError](filePath: Path): F[Array[Byte]] =
    handleIo(Files.readAllBytes(filePath), FileReadFailed.apply)
}
