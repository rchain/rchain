package coop.rchain.blockstorage.util

import java.io.IOException
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
    Sync[F].delay {
      try {
        Right(io)
      } catch {
        case e: IOException                   => Left(handleIoException(e))
        case e: SecurityException             => Left(FileSecurityViolation(e))
        case e: UnsupportedOperationException => Left(UnsupportedFileOperation(e))
        case e                                => Left(UnexpectedIOError(e))
      }
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

  def makeDirectory[F[_]: Sync](dirPath: Path): F[IOErr[Boolean]] =
    handleIo(dirPath.toFile.mkdir(), UnexpectedIOError.apply)

  def listInDirectory[F[_]: Sync](dirPath: Path): F[IOErr[List[Path]]] =
    (for {
      files <- EitherT(handleIo(Files.list(dirPath), {
                case e: NotDirectoryException => FileIsNotDirectory(e)
                case e                        => UnexpectedIOError(e)
              }))
      filesList = files
        .collect(Collectors.toList[Path])
        .asScala
        .toList
    } yield filesList).value

  def listRegularFiles[F[_]: Sync](dirPath: Path): F[IOErr[List[Path]]] = {
    type IOErrTF[A] = IOErrT[F, A]
    (for {
      files        <- EitherT(listInDirectory(dirPath))
      regularFiles <- files.filterA[IOErrTF](f => EitherT(isRegularFile(f)))
    } yield regularFiles).value
  }
}
