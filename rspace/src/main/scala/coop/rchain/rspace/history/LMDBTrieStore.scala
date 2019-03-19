package coop.rchain.rspace.history

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.nio.file.Path

import cats.effect.Sync

import scala.collection.JavaConverters._
import coop.rchain.rspace.{Blake2b256Hash, LMDBOps, Serialize, _}
import coop.rchain.rspace.internal._
import coop.rchain.shared.ByteVectorOps._
import coop.rchain.shared.Resources.withResource
import org.lmdbjava._
import org.lmdbjava.DbiFlags.MDB_CREATE
import scodec.Codec
import scodec.bits.{BitVector, ByteVector}

import scala.collection.concurrent.TrieMap

@SuppressWarnings(Array("org.wartremover.warts.Throw", "org.wartremover.warts.NonUnitStatements")) // TODO stop throwing exceptions
class LMDBTrieStore[F[_], K, V] private (
    val env: Env[ByteBuffer],
    protected[this] val databasePath: Path,
    _dbTrie: Dbi[ByteBuffer],
    _dbRoot: Dbi[ByteBuffer],
    _dbPastRoots: Dbi[ByteBuffer],
    _dbEmptyRoots: Dbi[ByteBuffer]
)(
    implicit
    codecK: Codec[K],
    codecV: Codec[V],
    val syncF: Sync[F]
) extends ITrieStore[Txn[ByteBuffer], K, V]
    with LMDBOps[F] {

  private[this] val trieCache: TrieMap[Blake2b256Hash, Trie[K, V]] = TrieMap.empty

  override protected def metricsSource: String = RSpaceMetricsSource + ".history.lmdb"

  private[rspace] def put(txn: Txn[ByteBuffer], key: Blake2b256Hash, value: Trie[K, V]): Unit = {
    _dbTrie.put(txn, key, value)
    trieCache.put(key, value)
  }

  private[rspace] def get(txn: Txn[ByteBuffer], key: Blake2b256Hash): Option[Trie[K, V]] =
    trieCache.get(key).orElse {
      _dbTrie.get(txn, key)(Codec[Trie[K, V]]).map { value =>
        trieCache.put(key, value)
        value
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

  override def close(): Unit = {
    super.close()

    _dbTrie.close()
    _dbRoot.close()
    _dbPastRoots.close()
    _dbEmptyRoots.close()
  }

  private[rspace] def clear(txn: Txn[ByteBuffer]): Unit = {
    _dbTrie.drop(txn)
    _dbRoot.drop(txn)
    _dbPastRoots.drop(txn)
    _dbEmptyRoots.drop(txn)
  }

  private[rspace] def getRoot(txn: Txn[ByteBuffer], branch: Branch): Option[Blake2b256Hash] =
    _dbRoot.get(txn, branch)(Codec[Branch], Codec[Blake2b256Hash])

  private[rspace] def persistAndGetRoot(
      txn: Txn[ByteBuffer],
      branch: Branch
  ): Option[Blake2b256Hash] =
    getRoot(txn, branch)
      .map { currentRoot =>
        val hashBytes =
          Codec[Blake2b256Hash].encode(currentRoot).map(_.bytes.toDirectByteBuffer).get
        val branchBytes = Codec[Branch].encode(branch).map(_.bytes.toDirectByteBuffer).get
        _dbPastRoots.put(txn, hashBytes, branchBytes)
        currentRoot
      }

  private[rspace] def putRoot(txn: Txn[ByteBuffer], branch: Branch, hash: Blake2b256Hash): Unit =
    _dbRoot.put(txn, branch, hash)(Codec[Branch], Codec[Blake2b256Hash])

  private[rspace] def validateAndPutRoot(
      txn: Txn[ByteBuffer],
      branch: Branch,
      hash: Blake2b256Hash
  ): Unit =
    getRoot(txn, branch)
      .find(_ == hash)
      .orElse {
        val hashBytes  = Codec[Blake2b256Hash].encode(hash).map(_.bytes.toDirectByteBuffer).get
        val maybeValue = Option(_dbPastRoots.get(txn, hashBytes))
        maybeValue
          .map(_ => hash)
          .map(hash => {
            putRoot(txn, branch, hash)
            hash
          })
      }
      .getOrElse(throw new Exception(s"Unknown root."))

  override private[rspace] def getEmptyRoot(txn: Txn[ByteBuffer]) =
    Option(_dbEmptyRoots.get(txn, LMDBTrieStore.emptyRootKey))
      .map(bytes => Codec[Blake2b256Hash].decode(BitVector(bytes)).map(_.value).get)
      .getOrElse(throw new Exception(s"Missing empty root."))

  override private[rspace] def putEmptyRoot(txn: Txn[ByteBuffer], hash: Blake2b256Hash): Unit =
    _dbEmptyRoots
      .put(
        txn,
        LMDBTrieStore.emptyRootKey,
        Codec[Blake2b256Hash].encode(hash).map(_.bytes.toDirectByteBuffer).get
      )
}

object LMDBTrieStore {

  def create[F[_], K, V](env: Env[ByteBuffer], path: Path)(
      implicit
      codecK: Codec[K],
      codecV: Codec[V],
      syncF: Sync[F]
  ): LMDBTrieStore[F, K, V] = {
    val dbTrie: Dbi[ByteBuffer]      = env.openDbi("Trie", MDB_CREATE)
    val dbRoots: Dbi[ByteBuffer]     = env.openDbi("Roots", MDB_CREATE)
    val dbEmptyRoot: Dbi[ByteBuffer] = env.openDbi("EmptyRoot", MDB_CREATE)
    val dbPastRoots: Dbi[ByteBuffer] = env.openDbi("PastRoots", MDB_CREATE)
    new LMDBTrieStore[F, K, V](env, path, dbTrie, dbRoots, dbPastRoots, dbEmptyRoot)
  }

  private val stringSerialize: Serialize[String] = new Serialize[String] {

    def encode(a: String): ByteVector =
      ByteVector.view(a.getBytes(StandardCharsets.UTF_8))

    def decode(bytes: ByteVector): Either[Throwable, String] =
      Right(new String(bytes.toArray, StandardCharsets.UTF_8))
  }

  private val stringCodec: Codec[String] = stringSerialize.toCodec

  private val emptyRootKey = stringCodec.encode("emptyRoot").get.bytes.toDirectByteBuffer
}
