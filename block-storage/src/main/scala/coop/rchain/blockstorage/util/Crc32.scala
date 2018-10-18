package coop.rchain.blockstorage.util

import java.nio.ByteBuffer
import java.util.zip.CRC32

import cats.Monad
import cats.implicits._

final class Crc32[F[_]: Monad](internal: CRC32) {
  def update(bytes: Array[Byte]): F[Unit] =
    internal.update(bytes).pure[F]

  def update(byteBuffer: ByteBuffer): F[Unit] =
    internal.update(byteBuffer).pure[F]

  def value: F[Long] =
    internal.getValue.pure[F]

  def bytes: F[Array[Byte]] =
    value.map { value =>
      val byteBuffer = ByteBuffer.allocate(8)
      byteBuffer.putLong(value)
      byteBuffer.array()
    }

  def reset: F[Unit] =
    internal.reset().pure[F]
}

object Crc32 {
  def apply[F[_]: Monad](initialBytes: Array[Byte]): Crc32[F] = {
    val crc = new CRC32()
    crc.update(initialBytes)
    new Crc32(crc)
  }
  def empty[F[_]: Monad](): Crc32[F] = new Crc32(new CRC32())
}
