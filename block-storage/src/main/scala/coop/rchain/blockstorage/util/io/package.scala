package coop.rchain.blockstorage.util

import java.io.{EOFException, IOException}
import java.nio.file._
import java.util.stream.Collectors

import cats.data.EitherT
import cats.implicits._
import cats.effect.Sync
import coop.rchain.blockstorage.util.io.IOError.{IOErr, IOErrT}

import scala.collection.JavaConverters._

package object io {
  private[io] def handleIo[F[_]: Sync, A](
      io: => A,
      handleIoException: IOException => IOError
  ): F[IOErr[A]] =
    Sync[F].delay { io }.attempt.map[IOErr[A]] {
      case Left(e: EOFException)                  => Left(EndOfFile(e))
      case Left(e: IOException)                   => Left(handleIoException(e))
      case Left(e: SecurityException)             => Left(FileSecurityViolation(e))
      case Left(e: UnsupportedOperationException) => Left(UnsupportedFileOperation(e))
      case Left(e: IllegalArgumentException)      => Left(IllegalFileOperation(e))
      case Left(e)                                => Left(UnexpectedIOError(e))
      case Right(v)                               => Right(v)
    }

  def moveFile[F[_]: Sync](
      from: Path,
      to: Path,
      options: CopyOption*
  ): F[IOErr[Path]] =
    handleIo(
      Files.move(from, to, options: _*), {
        case e: FileAlreadyExistsException =>
          FileAlreadyExists(e)
        case e: DirectoryNotEmptyException =>
          DirectoryNotEmpty(e)
        case e: AtomicMoveNotSupportedException =>
          AtomicMoveNotSupported(e)
        case e =>
          UnexpectedIOError(e)
      }
    )

  def isDirectory[F[_]: Sync](path: Path): F[IOErr[Boolean]] =
    handleIo(Files.isDirectory(path), UnexpectedIOError.apply)

  def isRegularFile[F[_]: Sync](path: Path): F[IOErr[Boolean]] =
    handleIo(Files.isRegularFile(path), UnexpectedIOError.apply)

  def makeDirectory[F[_]: Sync](dirPath: Path): F[IOErr[Unit]] =
    handleIo(dirPath.toFile.mkdir(), UnexpectedIOError.apply)

  def listInDirectory[F[_]: Sync](dirPath: Path): F[IOErr[List[Path]]] =
    (for {
      _ <- EitherT(makeDirectory(dirPath))
      files <- EitherT(handleIo(Files.list(dirPath), {
                case e: NotDirectoryException => FileIsNotDirectory(e)
                case e                        => UnexpectedIOError(e)
              }))
      filesList = files
        .collect(Collectors.toList[Path])
        .asScala
        .toList
    } yield filesList).value

  def listFiles[F[_]: Sync](dirPath: Path): F[IOErr[List[Path]]] = {
    type IOErrTF[A] = IOErrT[F, A]
    (for {
      inDirectoryList <- EitherT(listInDirectory(dirPath))
      directoryList   <- inDirectoryList.filterA[IOErrTF](f => EitherT(isRegularFile(f)))
    } yield directoryList).value
  }

  def createTemporaryFile[F[_]: Sync](prefix: String, suffix: String): F[IOErr[Path]] =
    handleIo(Files.createTempFile(prefix, suffix), UnexpectedIOError.apply)

  def createNewFile[F[_]: Sync](filePath: Path): F[IOErr[Boolean]] =
    handleIo(filePath.toFile.createNewFile(), UnexpectedIOError.apply)

  def createFile[F[_]: Sync](filePath: Path): F[IOErr[Unit]] =
    handleIo(Files.createFile(filePath), UnexpectedIOError.apply)

  def writeToFile[F[_]: Sync](filePath: Path, bytes: Array[Byte]): F[IOErr[Unit]] =
    handleIo(Files.write(filePath, bytes), ByteArrayWriteFailed.apply)

  def readAllBytesFromFile[F[_]: Sync](filePath: Path): F[IOErr[Array[Byte]]] =
    handleIo(Files.readAllBytes(filePath), ByteArrayReadFailed.apply)
}
