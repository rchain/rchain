package coop.rchain.rspace.test

import coop.rchain.rspace.examples._
import java.nio.ByteBuffer
import java.util.concurrent.locks.StampedLock
import coop.rchain.rspace.{KeyValueAmbry, Serialize, Store}
import coop.rchain.rspace.datamodels.BytesList

import scala.collection.mutable

case class InMemoryStore[C, P, A, K]()(implicit
                                       sc: Serialize[C],
                                       sp: Serialize[P],
                                       sa: Serialize[A],
                                       sk: Serialize[K],
                                       sbl: Serialize[BytesList])
    extends Store[C, P, A, K]
    with KeyValueAmbry {

  case class InMemoryKeyValueTable() extends KeyValueTable {
    private[InMemoryStore] val _hashMap = new mutable.HashMap[H, ByteBuffer]()

    override def put(txn: T, keyBuffer: H, valueBuffer: ByteBuffer): Unit =
      _hashMap.put(keyBuffer, valueBuffer)

    override def get(txn: T, keyBuffer: H): Option[ByteBuffer] =
      _hashMap.get(keyBuffer)

    override def delete(txn: T, keyBuffer: H): Unit =
      _hashMap.remove(keyBuffer)

    override def isEmpty(txn: T): Boolean = _hashMap.isEmpty

    override def drop(txn: T): Unit = _hashMap.clear()

    override def iterateKeys(txn: T): Iterator[H] = _hashMap.keysIterator
  }

  private[rspace] type T = Long

  val lock = new StampedLock()

  override val _dbKeys: KeyValueTable  = InMemoryKeyValueTable()
  override val _dbAs: KeyValueTable    = InMemoryKeyValueTable()
  override val _dbPsKs: KeyValueTable  = InMemoryKeyValueTable()
  override val _dbJoins: KeyValueTable = InMemoryKeyValueTable()

  private[rspace] def createTxnRead(): T = lock.readLock()

  private[rspace] def createTxnWrite(): T = lock.writeLock()

  private[rspace] def withTxn[R](txn: T)(f: T => R): R =
    try {
      f(txn)
    } finally {
      lock.unlock(txn)
    }

  override def close(): Unit = ()

  override def toByteBuffer(bytes: Array[Byte]): H = ByteBuffer.wrap(bytes)
}

object InMemoryStore {
  /* UGLY HACK FOR TESTING */
  def roundTrip[A <: Serializable](a: A): A = {
    val ser = makeSerializeFromSerializable[A]
    ser.decode(ser.encode(a)).fold(throw _, identity)
  }

  def create[C, P, A, K]()(implicit sc: Serialize[C],
                           sp: Serialize[P],
                           sa: Serialize[A],
                           sk: Serialize[K]): InMemoryStore[C, P, A, K] =
    new InMemoryStore[C, P, A, K]()(sc, sp, sa, sk, Store.bytesListInstance)
}
