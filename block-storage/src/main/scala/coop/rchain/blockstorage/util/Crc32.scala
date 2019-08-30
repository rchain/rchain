package coop.rchain.blockstorage.util

import java.nio.ByteBuffer
import java.util.zip.CRC32

import cats.effect.Sync
import cats.implicits._
import coop.rchain.shared.Language.ignore

final class Crc32[F[_]: Sync] private (internal: CRC32) {
  def update(bytes: Array[Byte]): F[Unit] =
    Sync[F].delay { internal.update(bytes) }

  def update(byteBuffer: ByteBuffer): F[Unit] =
    Sync[F].delay { internal.update(byteBuffer) }

  def value: F[Long] =
    Sync[F].delay { internal.getValue }

  def bytes: F[Array[Byte]] =
    value >>= { value =>
      Sync[F].delay {
        val byteBuffer = ByteBuffer.allocate(8)
        ignore { byteBuffer.putLong(value) }
        byteBuffer.array()
      }
    }

  def reset: F[Unit] =
    Sync[F].delay { internal.reset() }
}

object Crc32 {
  def empty[F[_]: Sync](): Crc32[F] = new Crc32(new CRC32())
}
