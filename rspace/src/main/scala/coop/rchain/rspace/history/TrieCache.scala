package coop.rchain.rspace.history
import coop.rchain.rspace.{Blake2b256Hash}

import scala.collection.immutable.{Map => ImmutableMap}
import scala.collection.immutable.Seq
import scala.collection.mutable.Map

private[rspace] trait CachedItem[TItem] {
  def asOption : Option[TItem]
}

private[rspace] case class LoadedItem[TItem](value: TItem) extends CachedItem[TItem] {
  override def asOption : Option[TItem] = Some(value)
}

private[rspace] case class StoredItem[TItem](value: TItem) extends CachedItem[TItem] {
  override def asOption : Option[TItem] = Some(value)
}

private[rspace] case class AbsentItem[TItem]() extends CachedItem[TItem] {
  override def asOption : Option[TItem] = None
}

private[rspace] class TrieCache[T, K, V](trieStore: ITrieStore[T, K, V], trieStoreOwner: Boolean = false) extends  ITrieStore[T, K, V] {

  private[this] val absentRootValue = AbsentItem[Blake2b256Hash]()

  val _dbRoot : Map[Branch, CachedItem[Blake2b256Hash]] = Map.empty

  private[this] val absentTrieValue = AbsentItem[Trie[K, V]]()

  val _dbTrie : Map[Blake2b256Hash, CachedItem[Trie[K, V]]] = Map.empty

  //private[this] val absentPastRootValue = AbsentItem[Seq[Blake2b256Hash]]()

  val _dbPastRoots : Map[Branch, CachedItem[Seq[Blake2b256Hash]]] = Map.empty

  private[this] val absentEmptyRootValue = AbsentItem[Blake2b256Hash]()

  var _dbEmptyRoot : CachedItem[Blake2b256Hash] = absentEmptyRootValue;

  private[rspace] def createTxnRead(): T = trieStore.createTxnRead()

  //that's not a bug - trie cache accesses trieStore for reading always,
  //except the special case applyCache()
  private[rspace] def createTxnWrite(): T = trieStore.createTxnRead()

  private[rspace] def withTxn[R](txn: T)(f: T => R): R = trieStore.withTxn(txn)(f)

  override private[rspace] def put(txn: T, key: Blake2b256Hash, value: Trie[K, V]): Unit = {
    _dbTrie.put(key, StoredItem(value))
  }

  override private[rspace] def get(txn: T, key: Blake2b256Hash): Option[Trie[K, V]] = {
    _dbTrie.get(key) match {
      case None =>
        trieStore.get(txn, key) match {
          case optValue @ Some(value) =>
            _dbTrie.put(key, LoadedItem(value))
            optValue
          case None =>
            _dbTrie.put(key, absentTrieValue)
            None
        }
      case Some(result) =>
        result.asOption
    }
  }

  override private[rspace] def getRoot(txn: T, branch: Branch): Option[Blake2b256Hash] = {
    _dbRoot.get(branch) match {
      case None => //first access to trieStore for the key
        trieStore.getRoot(txn, branch) match {
          case optValue @ Some(value) =>
            _dbRoot.put(branch, LoadedItem(value))
            optValue
          case None =>
            _dbRoot.put(branch, absentRootValue)
            None
        }
      case Some(result) =>
        result.asOption //we already looked up this branch, result cached
    }
  }

  override private[rspace] def putRoot(txn: T, branch: Branch, hash: Blake2b256Hash): Unit = {
    _dbRoot.put(branch, StoredItem(hash))
  }

  override private[rspace] def persistAndGetRoot(txn: T,
                                        branch: Branch) : Option[Blake2b256Hash] = {
    getRoot(txn, branch)
      .map { currentRoot =>
        val pastRoots = getPastRootsInBranch(txn, branch).filter(_ != currentRoot)
        (currentRoot, currentRoot +: pastRoots)
      }
      .map {
        case (currentRoot, updatedPastRoots) =>
          _dbPastRoots.put(branch, StoredItem(updatedPastRoots))
          currentRoot
      }
  }

  override private[rspace] def getPastRootsInBranch(txn: T, branch: Branch): Seq[Blake2b256Hash] = {
    _dbPastRoots.get(branch) match {
      case None =>
        val cold = trieStore.getPastRootsInBranch(txn, branch)
        _dbPastRoots.put(branch, LoadedItem(cold))
        cold
      case Some(result)=>
        result.asOption.getOrElse(Seq.empty)
    }
  }

  override private[rspace] def getAllPastRoots(txn: T) :  Seq[Blake2b256Hash] =
    throw new NotImplementedError("Unexpected call of TrieCache.getAllPastRoots()")

  override private[rspace] def validateAndPutRoot(txn: T, branch: Branch,  hash: Blake2b256Hash) : Unit =
    throw new NotImplementedError("Unexpected call of TrieCache.validateAndPutRoot()")

  override private[rspace] def getEmptyRoot(txn: T) : Blake2b256Hash = {
    _dbEmptyRoot match {
      case LoadedItem(value) =>
        value
      case StoredItem(value) =>
        value
      case AbsentItem() => {
        val hash = trieStore.getEmptyRoot(txn)
        _dbEmptyRoot = LoadedItem(hash)
        hash
      }
    }
  }

  override private[rspace] def putEmptyRoot(txn: T, hash: Blake2b256Hash): Unit = {
    _dbEmptyRoot = StoredItem(hash)
  }

  override private[rspace] def toMap: ImmutableMap[Blake2b256Hash, Trie[K, V]] = {
    trieStore.withTxn(trieStore.createTxnWrite()) {
      txn => trieStore.applyCache(txn, this)
    }
    trieStore.toMap
  }

  override private[rspace] def clear(txn: T): Unit = {
    _dbRoot.clear()
    _dbTrie.clear()
    _dbPastRoots.clear()
    _dbEmptyRoot = absentEmptyRootValue
    trieStore.clear(txn)
  }

  override def close(): Unit = {
    if(trieStoreOwner)
      trieStore.close()
  }

  override private[rspace] def applyCache(
      txn: T,
      trieCache: TrieCache[T, K, V]): Unit = throw new NotImplementedError("Can't apply cache to cache?!")
}
