package coop.rchain.rspace.history

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.nio.file.Path

import coop.rchain.rspace.{Blake2b256Hash, LMDBOps, Serialize}
import coop.rchain.rspace.internal._
import coop.rchain.shared.Resources.withResource
import coop.rchain.shared.ByteVectorOps._
import org.lmdbjava.DbiFlags.MDB_CREATE
import org.lmdbjava._
import scodec.Codec
import scodec.bits.{BitVector, ByteVector}

import scala.collection.JavaConverters._
import scala.collection.immutable.Seq

class LMDBTrieStore[K, V] private (
    val env: Env[ByteBuffer],
    protected[this] val databasePath: Path,
    _dbTrie: Dbi[ByteBuffer],
    _dbRoot: Dbi[ByteBuffer],
    _dbPastRoots: Dbi[ByteBuffer],
    _dbEmptyRoots: Dbi[ByteBuffer]
)(
    implicit
    codecK: Codec[K],
    codecV: Codec[V]
) extends ITrieStore[Txn[ByteBuffer], K, V]
    with LMDBOps {

  private[rspace] def put(txn: Txn[ByteBuffer], key: Blake2b256Hash, value: Trie[K, V]): Unit =
    _dbTrie.put(txn, key, value)

  private[rspace] def get(txn: Txn[ByteBuffer], key: Blake2b256Hash): Option[Trie[K, V]] =
    _dbTrie.get(txn, key)(Codec[Trie[K, V]])

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
        val pastRoots = getPastRootsInBranch(txn, branch).filter(_ != currentRoot)
        (currentRoot, currentRoot +: pastRoots)
      }
      .map {
        case (currentRoot, updatedPastRoots) =>
          _dbPastRoots.put(txn, branch, updatedPastRoots)
          currentRoot
      }

  private[rspace] def putRoot(txn: Txn[ByteBuffer], branch: Branch, hash: Blake2b256Hash): Unit =
    _dbRoot.put(txn, branch, hash)(Codec[Branch], Codec[Blake2b256Hash])

  private[rspace] def getAllPastRoots(txn: Txn[ByteBuffer]): Seq[Blake2b256Hash] =
    withResource(_dbPastRoots.iterate(txn)) { it: CursorIterator[ByteBuffer] =>
      it.asScala.foldLeft(Seq.empty[Blake2b256Hash]) { (acc, keyVal) =>
        acc ++ Codec[Seq[Blake2b256Hash]].decode(BitVector(keyVal.`val`())).map(_.value).get
      }
    }

  private[this] def getPastRootsInBranch(
      txn: Txn[ByteBuffer],
      branch: Branch
  ): Seq[Blake2b256Hash] =
    _dbPastRoots.get(txn, branch)(Codec[Branch], Codec[Seq[Blake2b256Hash]]).getOrElse(Seq.empty)

  private[rspace] def validateAndPutRoot(
      txn: Txn[ByteBuffer],
      branch: Branch,
      hash: Blake2b256Hash
  ): Unit =
    getRoot(txn, branch)
      .find(_ == hash)
      .orElse {
        getPastRootsInBranch(txn, branch)
          .find(_ == hash)
          .map { blake: Blake2b256Hash =>
            putRoot(txn, branch, blake)
            blake
          }
      }
      .orElse {
        getAllPastRoots(txn)
          .find(_ == hash)
          .map { blake: Blake2b256Hash =>
            putRoot(txn, branch, blake)
            blake
          }
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

  def create[K, V](env: Env[ByteBuffer], path: Path)(
      implicit
      codecK: Codec[K],
      codecV: Codec[V]
  ): LMDBTrieStore[K, V] = {
    val dbTrie: Dbi[ByteBuffer]      = env.openDbi("Trie", MDB_CREATE)
    val dbRoots: Dbi[ByteBuffer]     = env.openDbi("Roots", MDB_CREATE)
    val dbEmptyRoot: Dbi[ByteBuffer] = env.openDbi("EmptyRoot", MDB_CREATE)
    val dbPastRoots: Dbi[ByteBuffer] = env.openDbi("PastRoots", MDB_CREATE)
    new LMDBTrieStore[K, V](env, path, dbTrie, dbRoots, dbPastRoots, dbEmptyRoot)
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
