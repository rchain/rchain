package coop.rchain.rspace.history

import java.nio.ByteBuffer
import java.nio.file.Path

import coop.rchain.rspace.{Blake2b256Hash, LMDBOps}
import coop.rchain.rspace.internal._
import coop.rchain.shared.ByteVectorOps._
import coop.rchain.shared.Resources.withResource
import org.lmdbjava.DbiFlags.MDB_CREATE
import org.lmdbjava._
import scodec.Codec
import scodec.bits.BitVector

import scala.collection.JavaConverters._
import scala.collection.immutable.Seq

class LMDBTrieStore[K, V] private (val env: Env[ByteBuffer],
                                   protected[this] val databasePath: Path,
                                   _dbTrie: Dbi[ByteBuffer],
                                   _dbRoot: Dbi[ByteBuffer],
                                   _dbPastRoots: Dbi[ByteBuffer])(implicit
                                                                  codecK: Codec[K],
                                                                  codecV: Codec[V])
    extends ITrieStore[Txn[ByteBuffer], K, V]
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

  def close(): Unit = {
    _dbTrie.close()
    _dbRoot.close()
    _dbPastRoots.close()
  }

  private[rspace] def clear(txn: Txn[ByteBuffer]): Unit = {
    _dbTrie.drop(txn)
    _dbRoot.drop(txn)
    _dbPastRoots.drop(txn)
  }

  private[rspace] def getRoot(txn: Txn[ByteBuffer], branch: Branch): Option[Blake2b256Hash] =
    _dbRoot.get(txn, branch)(Codec[Branch], Codec[Blake2b256Hash])

  private[rspace] def persistAndGetRoot(txn: Txn[ByteBuffer],
                                        branch: Branch): Option[Blake2b256Hash] =
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
    withResource(_dbPastRoots.iterate(txn)) { (it: CursorIterator[ByteBuffer]) =>
      it.asScala.foldLeft(Seq.empty[Blake2b256Hash]) { (acc, keyVal) =>
        acc ++ Codec[Seq[Blake2b256Hash]].decode(BitVector(keyVal.`val`())).map(_.value).get
      }
    }

  private[this] def getPastRootsInBranch(txn: Txn[ByteBuffer],
                                         branch: Branch): Seq[Blake2b256Hash] =
    _dbPastRoots.get(txn, branch)(Codec[Branch], Codec[Seq[Blake2b256Hash]]).getOrElse(Seq.empty)

  private[rspace] def validateAndPutRoot(txn: Txn[ByteBuffer],
                                         branch: Branch,
                                         hash: Blake2b256Hash): Unit =
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
}

object LMDBTrieStore {

  def create[K, V](env: Env[ByteBuffer], path: Path)(implicit
                                                     codecK: Codec[K],
                                                     codecV: Codec[V]): LMDBTrieStore[K, V] = {
    val dbTrie: Dbi[ByteBuffer]      = env.openDbi("Trie", MDB_CREATE)
    val dbRoots: Dbi[ByteBuffer]     = env.openDbi("Roots", MDB_CREATE)
    val dbPastRoots: Dbi[ByteBuffer] = env.openDbi("PastRoots", MDB_CREATE)
    new LMDBTrieStore[K, V](env, path, dbTrie, dbRoots, dbPastRoots)
  }
}
