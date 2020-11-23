package coop.rchain.casper.storage

import java.nio.ByteBuffer

import cats.effect.Sync
import coop.rchain.store.KeyValueStore
import org.rocksdb.{RocksDB, RocksIterator, WriteBatch, WriteOptions}

final case class RocksDbKeyValueStore[F[_]: Sync](
    rocksDb: RocksDB
) extends KeyValueStore[F] {

  // GET
  override def get[T](keys: Seq[ByteBuffer], fromBuffer: ByteBuffer => T): F[Seq[Option[T]]] =
    Sync[F].delay(
      keys.map { key =>
        val keyBytes = bufferToArray(key)
        Option(rocksDb.get(keyBytes)).map { bytes =>
          fromBuffer(arrayToBuffer(bytes))
        }
      }
    )

  // PUT
  override def put[T](kvPairs: Seq[(ByteBuffer, T)], toBuffer: T => ByteBuffer): F[Unit] =
    Sync[F].delay {
      val batch = new WriteBatch
      kvPairs.foreach {
        case (key, value) =>
          val keyBytes = bufferToArray(key)
          val valBytes = bufferToArray(toBuffer(value))
          batch.put(keyBytes, valBytes)
      }
      // Put batch
      val writeOpt = new WriteOptions().setSync(true)
      rocksDb.write(writeOpt, batch)
    }

  // DELETE
  override def delete(keys: Seq[ByteBuffer]): F[Int] =
    Sync[F].delay {
      val batch = new WriteBatch
      val deleted = keys.foldLeft(0) { (acc, key) =>
        val keyBytes = bufferToArray(key)
        batch.delete(keyBytes)
        acc + 1
      }
      // Delete batch
      val writeOpt = new WriteOptions().setSync(true)
      rocksDb.write(writeOpt, batch)
      // TODO: check which keys existed
      deleted
    }

  // ITERATE
  override def iterate[T](f: Iterator[(ByteBuffer, ByteBuffer)] => T): F[T] =
    Sync[F].delay {
      val iterBuffer = new RocksDbIterator(rocksDb.newIterator) map {
        case KV(key, value) =>
          val keyBuff = arrayToBuffer(key)
          val valBuff = arrayToBuffer(value)
          (keyBuff, valBuff)
      }
      f(iterBuffer)
    }

  private def bufferToArray(buffer: ByteBuffer): Array[Byte] = {
    val c   = buffer.duplicate()
    val arr = new Array[Byte](c.remaining)
    val _   = c.get(arr)
    arr
  }

  private def arrayToBuffer(bytes: Array[Byte]): ByteBuffer =
    ByteBuffer.wrap(bytes).asReadOnlyBuffer()
}

private final case class KV(key: Array[Byte], value: Array[Byte])

private class RocksDbIterator(iter: RocksIterator) extends Iterator[KV] {

  iter.seekToFirst()

  override def hasNext: Boolean =
    iter.isValid

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  override def next(): KV = {
    if (!hasNext) {
      throw new NoSuchElementException
    }
    val kv = getKV
    iter.next
    kv
  }

  private def getKV =
    KV(iter.key, iter.value)
}
