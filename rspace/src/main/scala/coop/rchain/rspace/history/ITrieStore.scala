package coop.rchain.rspace.history

import coop.rchain.rspace.util.ignore

import scala.concurrent.SyncVar

trait ITrieStore[T, K, V] {

  private[rspace] def createTxnRead(): T

  private[rspace] def createTxnWrite(): T

  private[rspace] def withTxn[R](txn: T)(f: T => R): R

  private[rspace] val workingRootHash: SyncVar[Blake2b256Hash] = new SyncVar[Blake2b256Hash]()

  private[rspace] def reset(hash: Blake2b256Hash): Unit = {
    ignore { workingRootHash.take() }
    workingRootHash.put(hash)
  }

  private[rspace] def put(txn: T, key: Blake2b256Hash, value: Trie[K, V]): Unit

  private[rspace] def get(txn: T, key: Blake2b256Hash): Option[Trie[K, V]]

  private[rspace] def toMap: Map[Blake2b256Hash, Trie[K, V]]

  def close(): Unit
}
