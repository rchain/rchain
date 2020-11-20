package coop.rchain.rspace.history

import java.nio.ByteBuffer

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.shared.ByteVectorOps.RichByteVector
import coop.rchain.store.KeyValueStoreManager
import scodec.bits.BitVector

// This is helper method to create Store instances with KeyValueStoreManager.
object StoreFactory {
  def keyValueStore[F[_]: Sync: KeyValueStoreManager](dbName: String): F[Store[F]] =
    for {
      store <- KeyValueStoreManager[F].store(dbName)
    } yield new Store[F] {

      override def get(key: ByteBuffer): F[Option[BitVector]] =
        store.get(Seq(key), BitVector(_)).map(_.head)

      override def put(key: ByteBuffer, value: ByteBuffer): F[Unit] =
        store.put[ByteBuffer](Seq((key, value)), identity)

      override def get(key: Blake2b256Hash): F[Option[BitVector]] =
        get(key.bytes.toDirectByteBuffer)

      override def get[T](
          keys: Seq[Blake2b256Hash],
          fromBuffer: ByteBuffer => T
      ): F[Seq[Option[T]]] =
        store.get(keys.map(_.bytes.toDirectByteBuffer), fromBuffer)

      override def put(key: Blake2b256Hash, value: BitVector): F[Unit] = {
        val directKey   = key.bytes.toDirectByteBuffer
        val directValue = value.toByteVector.toDirectByteBuffer
        put(directKey, directValue)
      }

      override def put[T](
          data: Seq[(Blake2b256Hash, T)],
          toBuffer: T => ByteBuffer
      ): F[Unit] = {
        val rawData = data.map { case (k, v) => (k.bytes.toDirectByteBuffer, v) }
        store.put(rawData, toBuffer)
      }

      override def put(data: Seq[(Blake2b256Hash, BitVector)]): F[Unit] =
        put(data, (x: BitVector) => x.toByteVector.toDirectByteBuffer)

      override def close(): F[Unit] = ().pure
    }
}
