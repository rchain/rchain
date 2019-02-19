package coop.rchain.blockstorage.util.io

import java.io.{FileNotFoundException, RandomAccessFile}
import java.nio.file.Path

import cats.effect.Sync
import cats.implicits._
import IOError.IOErr

import scala.language.higherKinds

final case class RandomAccessIO[F[_]: Sync] private (private val file: RandomAccessFile) {
  def close(): F[IOErr[Unit]] =
    handleIo(file.close(), ClosingFailed.apply)

  def write(bytes: Array[Byte]): F[IOErr[Unit]] =
    handleIo(file.write(bytes), ByteArrayWriteFailed.apply)

  def writeInt(v: Int): F[IOErr[Unit]] =
    handleIo(file.writeInt(v), IntWriteFailed.apply)

  def readInt: F[IOErr[Int]] =
    handleIo(file.readInt(), IntReadFailed.apply)

  def readFully(buffer: Array[Byte]): F[IOErr[Unit]] =
    handleIo(file.readFully(buffer), ByteArrayReadFailed.apply)

  def length: F[IOErr[Long]] =
    handleIo(file.length(), UnexpectedIOError.apply)

  def seek(offset: Long): F[IOErr[Unit]] =
    handleIo(file.seek(offset), FileSeekFailed.apply)

  def setLength(length: Long): F[IOErr[Unit]] =
    handleIo(file.setLength(length), SetLengthFailed.apply)
}

object RandomAccessIO {
  def open[F[_]: Sync](path: Path): F[IOErr[RandomAccessIO[F]]] =
    handleIo(new RandomAccessFile(path.toFile, "rw"), {
      case e: FileNotFoundException => FileNotFound(e)
      case e                        => UnexpectedIOError(e)
    }).map(_.right.map(RandomAccessIO.apply[F]))
}
