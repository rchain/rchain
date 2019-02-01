package coop.rchain.blockstorage.util.io

import java.io.{FileNotFoundException, IOException, RandomAccessFile}
import java.nio.file.Path

import cats.data.EitherT
import cats.effect.Sync
import cats.implicits._
import IOError.IOErrT

import scala.language.higherKinds

final case class RandomAccessIO[F[_]: Sync](file: RandomAccessFile) {
  def close(): IOErrT[F, Unit] =
    EitherT(Sync[F].delay { file.close() }.attempt).leftMap[IOError] {
      case e: IOException =>
        ClosingFailed(e)
      case t =>
        UnexpectedIOError(t)
    }

  def write(bytes: Array[Byte]): IOErrT[F, Unit] =
    EitherT(Sync[F].delay { file.write(bytes) }.attempt).leftMap[IOError] {
      case e: IOException => ByteArrayWriteFailed(e)
      case e              => UnexpectedIOError(e)
    }

  def writeInt(v: Int): IOErrT[F, Unit] =
    EitherT(Sync[F].delay { file.writeInt(v) }.attempt).leftMap[IOError] {
      case e: IOException => IntWriteFailed(e)
      case e              => UnexpectedIOError(e)
    }

  def readInt: IOErrT[F, Int] =
    EitherT(Sync[F].delay { file.readInt() }.attempt).leftMap[IOError] {
      case e: IOException => IntReadFailed(e)
      case e              => UnexpectedIOError(e)
    }

  def readFully(buffer: Array[Byte]): IOErrT[F, Unit] =
    EitherT(Sync[F].delay { file.readFully(buffer) }.attempt).leftMap[IOError] {
      case e: IOException => ByteArrayReadFailed(e)
      case e              => UnexpectedIOError(e)
    }

  def length: IOErrT[F, Long] =
    EitherT(Sync[F].delay { file.length() }.attempt).leftMap(UnexpectedIOError.apply)

  def seek(offset: Long): IOErrT[F, Unit] =
    EitherT(Sync[F].delay { file.seek(offset) }.attempt).leftMap[IOError] {
      case e: IOException => FileSeekFailed(e)
      case e              => UnexpectedIOError(e)
    }

  def setLength(length: Long): IOErrT[F, Unit] =
    EitherT(Sync[F].delay { file.setLength(length) }.attempt).leftMap[IOError] {
      case e: IOException => ClearFileFailed(e)
      case t              => UnexpectedIOError(t)
    }
}

object RandomAccessIO {
  def open[F[_]: Sync](path: Path): IOErrT[F, RandomAccessIO[F]] =
    EitherT(Sync[F].delay { new RandomAccessFile(path.toFile, "rw") }.attempt)
      .leftMap[IOError] {
        case e: FileNotFoundException => FileNotFound(e)
        case e: SecurityException     => FileSecurityViolation(e)
        case t                        => UnexpectedIOError(t)
      }
      .map(file => RandomAccessIO[F](file))
}
