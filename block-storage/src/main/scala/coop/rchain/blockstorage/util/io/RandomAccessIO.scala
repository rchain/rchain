package coop.rchain.blockstorage.util.io

import java.io.{FileNotFoundException, RandomAccessFile}
import java.nio.file.Path

import cats.effect.Sync
import cats.implicits._
import coop.rchain.blockstorage.util.io.IOError.RaiseIOError

import scala.language.higherKinds

final case class RandomAccessIO[F[_]: Sync: RaiseIOError] private (
    private val file: RandomAccessFile
) {
  def close: F[Unit] =
    handleIo(file.close(), ClosingFailed.apply)

  def write(bytes: Array[Byte]): F[Unit] =
    handleIo(file.write(bytes), ByteArrayWriteFailed.apply)

  def writeInt(v: Int): F[Unit] =
    handleIo(file.writeInt(v), IntWriteFailed.apply)

  def readInt: F[Int] =
    handleIo(file.readInt(), IntReadFailed.apply)

  def readFully(buffer: Array[Byte]): F[Unit] =
    handleIo(file.readFully(buffer), ByteArrayReadFailed.apply)

  def length: F[Long] =
    handleIo(file.length(), UnexpectedIOError.apply)

  def seek(offset: Long): F[Unit] =
    handleIo(file.seek(offset), FileSeekFailed.apply)

  def setLength(length: Long): F[Unit] =
    handleIo(file.setLength(length), SetLengthFailed.apply)
}

object RandomAccessIO {
  sealed abstract class Mode(val representation: String)
  case object Read      extends Mode("r")
  case object Write     extends Mode("w")
  case object ReadWrite extends Mode("rw")

  def open[F[_]: Sync: RaiseIOError](path: Path, mode: Mode): F[RandomAccessIO[F]] =
    handleIo(new RandomAccessFile(path.toFile, mode.representation), {
      case e: FileNotFoundException => FileNotFound(e)
      case e                        => UnexpectedIOError(e)
    }).map(RandomAccessIO.apply[F])
}
