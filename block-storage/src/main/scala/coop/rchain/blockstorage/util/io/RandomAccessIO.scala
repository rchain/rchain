package coop.rchain.blockstorage.util.io

import java.io.{EOFException, FileNotFoundException, RandomAccessFile}
import java.nio.file.Path

import cats.effect.Sync
import cats.implicits._
import com.google.protobuf.ByteString
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

  def readInt: F[Option[Int]] =
    handleIoF(file.readInt().some, {
      case _: EOFException => none[Int].pure[F]
      case e               => RaiseIOError[F].raise[Option[Int]](IntReadFailed(e))
    })

  def readFully(buffer: Array[Byte]): F[Option[Unit]] =
    handleIoF(file.readFully(buffer).some, {
      case _: EOFException => none[Unit].pure[F]
      case e               => RaiseIOError[F].raise[Option[Unit]](ByteArrayReadFailed(e))
    })

  def readByteString(len: Int): F[Option[ByteString]] = {
    val bytes = Array.ofDim[Byte](len)
    readFully(bytes).map(_.map(_ => ByteString.copyFrom(bytes)))
  }

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
