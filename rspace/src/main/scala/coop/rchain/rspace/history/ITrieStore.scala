package coop.rchain.rspace.history

import scala.concurrent.SyncVar

trait ITrieStore[T, K, V] {

  private[rspace] def createTxnRead(): T

  private[rspace] def createTxnWrite(): T

  private[rspace] def withTxn[R](txn: T)(f: T => R): R

  private[rspace] val workingRootHash: SyncVar[Blake2b256Hash] = new SyncVar[Blake2b256Hash]()

  private[rspace] def put(txn: T, key: Blake2b256Hash, value: Trie[K, V]): Unit

  private[rspace] def get(txn: T, key: Blake2b256Hash): Option[Trie[K, V]]
}
