package coop.rchain.blockstorage.util

import java.nio.file._
import java.util.stream.Collectors

import cats.data.EitherT
import cats.implicits._
import cats.effect.Sync
import coop.rchain.blockstorage.util.io.IOError.IOErrT

import scala.collection.JavaConverters._

package object io {
  def moveFile[F[_]: Sync](
      from: Path,
      to: Path,
      options: CopyOption*
  ): IOErrT[F, Path] =
    EitherT(Sync[F].delay { Files.move(from, to, options: _*) }.attempt).leftMap[IOError] {
      case e: UnsupportedOperationException =>
        UnsupportedFileOperation(e)
      case e: FileAlreadyExistsException =>
        FileAlreadyExists(e)
      case e: DirectoryNotEmptyException =>
        DirectoryNotEmpty(e)
      case e: AtomicMoveNotSupportedException =>
        AtomicMoveNotSupported(e)
      case e: SecurityException =>
        FileSecurityViolation(e)
      case t =>
        UnexpectedIOError(t)
    }

  def isDirectory[F[_]: Sync](path: Path): IOErrT[F, Boolean] =
    EitherT(Sync[F].delay { Files.isDirectory(path) }.attempt).leftMap[IOError] {
      case e: SecurityException => FileSecurityViolation(e)
      case t                    => UnexpectedIOError(t)
    }

  def isRegularFile[F[_]: Sync](path: Path): IOErrT[F, Boolean] =
    EitherT(Sync[F].delay { Files.isRegularFile(path) }.attempt).leftMap[IOError] {
      case e: SecurityException => FileSecurityViolation(e)
      case t                    => UnexpectedIOError(t)
    }

  def listInDirectory[F[_]: Sync](dirPath: Path): IOErrT[F, List[Path]] =
    for {
      _ <- EitherT(Sync[F].delay { dirPath.toFile.mkdir() }.attempt).leftMap[IOError] {
            case e: SecurityException => FileSecurityViolation(e)
            case t                    => UnexpectedIOError(t)
          }
      files <- EitherT(Sync[F].delay { Files.list(dirPath) }.attempt).leftMap[IOError] {
                case e: NotDirectoryException => FileIsNotDirectory(e)
                case e: SecurityException     => FileSecurityViolation(e)
                case t                        => UnexpectedIOError(t)
              }
      filesList = files
        .collect(Collectors.toList[Path])
        .asScala
        .toList
    } yield filesList

  def listFiles[F[_]: Sync](dirPath: Path): IOErrT[F, List[Path]] = {
    type IOErrTF[A] = IOErrT[F, A]
    for {
      inDirectoryList <- listInDirectory(dirPath)
      directoryList   <- inDirectoryList.filterA[IOErrTF](isRegularFile)
    } yield directoryList
  }
}
