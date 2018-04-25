package coop.rchain.rspace

import java.nio.ByteBuffer
import java.nio.file.Path

import coop.rchain.rspace.util._
import org.lmdbjava.DbiFlags.MDB_CREATE
import org.lmdbjava._
import scodec.bits.BitVector

import scala.collection.AbstractIterator

/**
  * The main store class.
  *
  * To create an instance, use [[LMDBStore.create]].
  */
class LMDBStore[C, P, A, K](env: Env[ByteBuffer])(implicit
                                                  sc: Serialize[C],
                                                  sp: Serialize[P],
                                                  sa: Serialize[A],
                                                  sk: Serialize[K],
                                                  allocator: IBufferAllocator)
    extends Store[C, P, A, K]
    with KeyValueAmbry {

  private[rspace] case class LMDBKvTable(_dbi: Dbi[ByteBuffer]) extends KeyValueTable {
    override def isEmpty(txn: T): Boolean =
      withResource(_dbi.iterate(txn)) { !_.hasNext }

    override def get(txn: T, keyBuffer: H): Option[ByteBuffer] =
      Option(_dbi.get(txn, keyBuffer))

    override def put(txn: T, keyBuffer: H, valueBuffer: ByteBuffer): Unit =
      _dbi.put(txn, keyBuffer, valueBuffer)

    override def delete(txn: T, keyBuffer: H): Unit =
      _dbi.delete(txn, keyBuffer)

    override def drop(txn: T): Unit = _dbi.drop(txn)

    def close(): Unit = _dbi.close()

    override def iterateKeys(txn: T): Iterator[H] = {
      case class CustomKeysIterator(it: CursorIterator[H])
          extends AbstractIterator[H]
          with AutoCloseable {
        override def hasNext: Boolean   = it.hasNext
        override def next(): ByteBuffer = it.next.key()
        override def close(): Unit      = it.close()
      }

      val keyRange: KeyRange[ByteBuffer] = KeyRange.all()
      CustomKeysIterator(_dbi.iterate(txn, keyRange))
    }
  }

  import coop.rchain.rspace.LMDBStore._

  override val _dbKeys = LMDBKvTable(env.openDbi(keysTableName, MDB_CREATE))
  override val _dbWaitingContinuations = LMDBKvTable(
    env.openDbi(continuationsTableName, MDB_CREATE))
  override val _dbData  = LMDBKvTable(env.openDbi(dataTableName, MDB_CREATE))
  override val _dbJoins = LMDBKvTable(env.openDbi(joinsTableName, MDB_CREATE))

  private[rspace] type T = Txn[ByteBuffer]

  private[rspace] def createTxnRead(): T = env.txnRead

  private[rspace] def createTxnWrite(): T = env.txnWrite

  private[rspace] def withTxn[R](txn: T)(f: T => R): R =
    try {
      val ret: R = f(txn)
      txn.commit()
      ret
    } catch {
      case ex: Throwable =>
        txn.abort()
        throw ex
    } finally {
      txn.close()
    }

  override def close(): Unit = {
    _dbKeys.close()
    _dbData.close()
    _dbWaitingContinuations.close()
    _dbJoins.close()
    env.close()
  }
}

object LMDBStore {
  private[LMDBStore] val keysTableName: String          = "Keys"
  private[LMDBStore] val continuationsTableName: String = "WaitingContinuations"
  private[LMDBStore] val dataTableName: String          = "Data"
  private[LMDBStore] val joinsTableName: String         = "Joins"

  object directBufferAllocator extends IBufferAllocator {
    override def toByteBuffer(bytes: Array[Byte]): ByteBuffer = {
      val byteBuffer = ByteBuffer.allocateDirect(bytes.length)
      byteBuffer.put(bytes).flip()
      byteBuffer
    }

    override def toByteBuffer(bitVector: BitVector): ByteBuffer = {
      val bytes          = bitVector.bytes
      val bb: ByteBuffer = ByteBuffer.allocateDirect(bytes.size.toInt)
      bytes.copyToBuffer(bb)
      bb.flip()
      bb
    }
  }

  /**
    * Creates an instance of [[LMDBStore]]
    *
    * @param path    Path to the database files
    * @param mapSize Maximum size of the database, in bytes
    * @tparam C A type representing a channel
    * @tparam P A type representing a pattern
    * @tparam A A type representing a piece of data
    * @tparam K A type representing a continuation
    */
  def create[C, P, A, K](path: Path, mapSize: Long)(implicit sc: Serialize[C],
                                                    sp: Serialize[P],
                                                    sa: Serialize[A],
                                                    sk: Serialize[K]): LMDBStore[C, P, A, K] = {

    val env: Env[ByteBuffer] =
      Env.create().setMapSize(mapSize).setMaxDbs(8).open(path.toFile)

    new LMDBStore[C, P, A, K](env)(sc, sp, sa, sk, directBufferAllocator)
  }
}
