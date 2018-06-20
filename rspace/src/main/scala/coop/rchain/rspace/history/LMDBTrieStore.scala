package coop.rchain.rspace.history

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.shared.Resources.withResource
import coop.rchain.shared.AttemptOps._
import coop.rchain.shared.ByteVectorOps._
import org.lmdbjava.DbiFlags.MDB_CREATE
import org.lmdbjava._
import scodec.Codec
import scodec.bits.BitVector

import scala.collection.JavaConverters._

class LMDBTrieStore[K, V] private (val env: Env[ByteBuffer],
                                   _dbTrie: Dbi[ByteBuffer],
                                   _dbRoot: Dbi[ByteBuffer])(implicit
                                                             codecK: Codec[K],
                                                             codecV: Codec[V])
    extends ITrieStore[Txn[ByteBuffer], K, V] {

  private[rspace] def createTxnRead(): Txn[ByteBuffer] = env.txnRead

  private[rspace] def createTxnWrite(): Txn[ByteBuffer] = env.txnWrite

  private[rspace] def withTxn[R](txn: Txn[ByteBuffer])(f: Txn[ByteBuffer] => R): R =
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

  private[rspace] def put(txn: Txn[ByteBuffer], key: Blake2b256Hash, value: Trie[K, V]): Unit = {
    val encodedKey   = Codec[Blake2b256Hash].encode(key).get
    val encodedValue = Codec[Trie[K, V]].encode(value).get
    val keyBuff      = encodedKey.bytes.toDirectByteBuffer
    val valBuff      = encodedValue.bytes.toDirectByteBuffer
    _dbTrie.put(txn, keyBuff, valBuff)
  }

  private[rspace] def get(txn: Txn[ByteBuffer], key: Blake2b256Hash): Option[Trie[K, V]] = {
    val encodedKey = Codec[Blake2b256Hash].encode(key).get
    val keyBuff    = encodedKey.bytes.toDirectByteBuffer
    Option(_dbTrie.get(txn, keyBuff)).map { (buffer: ByteBuffer) =>
      // ht: Yes, I want to throw an exception if deserialization fails
      Codec[Trie[K, V]].decode(BitVector(buffer)).map(_.value).get
    }
  }

  private[rspace] def toMap: Map[Blake2b256Hash, Trie[K, V]] =
    withTxn(createTxnRead()) { txn =>
      withResource(_dbTrie.iterate(txn)) { (it: CursorIterator[ByteBuffer]) =>
        it.asScala.foldLeft(Map.empty[Blake2b256Hash, Trie[K, V]]) {
          (map: Map[Blake2b256Hash, Trie[K, V]], x: CursorIterator.KeyVal[ByteBuffer]) =>
            val key   = Codec[Blake2b256Hash].decode(BitVector(x.key())).map(_.value).get
            val value = Codec[Trie[K, V]].decode(BitVector(x.`val`())).map(_.value).get
            map + ((key, value))
        }
      }
    }

  def close(): Unit =
    _dbTrie.close()

  private[rspace] def clear(txn: Txn[ByteBuffer]): Unit = {
    _dbTrie.drop(txn)
    _dbRoot.drop(txn)
  }

  private[rspace] def getRoot(txn: Txn[ByteBuffer], branch: Branch): Option[Blake2b256Hash] = {
    val encodedBranch     = Codec[Branch].encode(branch).get
    val encodedBranchBuff = encodedBranch.bytes.toDirectByteBuffer
    Option(_dbRoot.get(txn, encodedBranchBuff)).map { (buffer: ByteBuffer) =>
      Codec[Blake2b256Hash].decode(BitVector(buffer)).map(_.value).get
    }
  }

  private[rspace] def putRoot(txn: Txn[ByteBuffer], branch: Branch, hash: Blake2b256Hash): Unit = {
    val encodedBranch     = Codec[Branch].encode(branch).get
    val encodedBranchBuff = encodedBranch.bytes.toDirectByteBuffer
    val encodedHash       = Codec[Blake2b256Hash].encode(hash).get
    val encodedHashBuff   = encodedHash.bytes.toDirectByteBuffer
    if (!_dbRoot.put(txn, encodedBranchBuff, encodedHashBuff)) {
      throw new Exception(s"could not persist: $hash")
    }
  }
}

object LMDBTrieStore {

  def create[K, V](env: Env[ByteBuffer])(implicit
                                         codecK: Codec[K],
                                         codecV: Codec[V]): LMDBTrieStore[K, V] = {
    val dbTrie: Dbi[ByteBuffer]  = env.openDbi("Trie", MDB_CREATE)
    val dbRoots: Dbi[ByteBuffer] = env.openDbi("Roots", MDB_CREATE)
    new LMDBTrieStore[K, V](env, dbTrie, dbRoots)
  }
}
