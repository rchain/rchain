package coop.rchain.blockstorage.util.io

import java.io.{FileNotFoundException, FileOutputStream}
import java.nio.file.Path

import cats.effect.Sync
import cats.syntax.functor._
import coop.rchain.blockstorage.util.io.IOError.IOErr

final case class FileOutputStreamIO[F[_]: Sync] private (private val stream: FileOutputStream) extends AutoCloseable {
  def write(bytes: Array[Byte]): F[IOErr[Unit]] =
    handleIo(stream.write(bytes), ByteArrayWriteFailed.apply)

  def flush: F[IOErr[Unit]] =
    handleIo(stream.flush(), StreamFlushFailed.apply)

  def close(): F[IOErr[Unit]] =
    handleIo(stream.close(), ClosingFailed.apply)
}

object FileOutputStreamIO {
  def open[F[_]: Sync](path: Path, append: Boolean): F[IOErr[FileOutputStreamIO[F]]] =
    handleIo(new FileOutputStream(path.toFile, append), {
      case e: FileNotFoundException => FileNotFound(e)
      case e                        => UnexpectedIOError(e)
    }).map(_.right.map(FileOutputStreamIO.apply[F]))
}
