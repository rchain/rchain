package coop.rchain.rspace.test

import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.history.{ITrieStore, Trie}

class DummyTrieStore[T, K, V] extends ITrieStore[T, K, V] {

  private[rspace] def createTxnRead(): T = ???

  private[rspace] def createTxnWrite(): T = ???

  private[rspace] def withTxn[R](txn: T)(f: T => R): R = ???

  private[rspace] def getRoot(txn: T): Option[Blake2b256Hash] = ???

  private[rspace] def putRoot(txn: T, hash: Blake2b256Hash): Unit = ???

  private[rspace] def put(txn: T, key: Blake2b256Hash, value: Trie[K, V]): Unit = ???

  private[rspace] def get(txn: T, key: Blake2b256Hash): Option[Trie[K, V]] = ???

  private[rspace] def toMap: Map[Blake2b256Hash, Trie[K, V]] = ???

  def close(): Unit = ???

  private[rspace] def clear(txn: T): Unit = ???
}
