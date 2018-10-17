package coop.rchain.rspace.history
import coop.rchain.rspace.{Blake2b256Hash}

import scala.collection.immutable.{Map => ImmutableMap}
import scala.collection.immutable.Seq
import scala.collection.mutable.Map

private[rspace] class TrieCache[T, K, V](trieStore: ITrieStore[T, K, V], trieStoreOwner: Boolean = false) extends  ITrieStore[T, K, V] {

  val _dbRoot : Map[Branch, Option[Blake2b256Hash]] = Map.empty

  val _dbTrie : Map[Blake2b256Hash, Option[Trie[K, V]]] = Map.empty

  val _dbPastRoots : Map[Branch, Option[Seq[Blake2b256Hash]]] = Map.empty

  var _dbEmptyRoot : Option[Option[Blake2b256Hash]] = None

  private[rspace] def createTxnRead(): T = trieStore.createTxnRead()

  private[rspace] def createTxnWrite(): T = trieStore.createTxnWrite()

  private[rspace] def withTxn[R](txn: T)(f: T => R): R = trieStore.withTxn(txn)(f)

  override private[rspace] def put(txn: T, key: Blake2b256Hash, value: Trie[K, V]): Unit = {
    _dbTrie.put(key, Some(value))
  }

  override private[rspace] def get(txn: T, key: Blake2b256Hash): Option[Trie[K, V]] = {
    _dbTrie.get(key) match {
      case None =>
        val cold = trieStore.get(txn, key)
        _dbTrie.put(key, cold)
        cold
      case Some(result) =>
        result
    }
  }

  override private[rspace] def getRoot(txn: T, branch: Branch): Option[Blake2b256Hash] = {
    _dbRoot.get(branch) match {
      case None => //first access to trieStore for the key
        val cold = trieStore.getRoot(txn, branch)
        _dbRoot.put(branch, cold)
        cold
      case Some(result) =>
        result //we already looked up this branch, result cached
    }
  }

  override private[rspace] def putRoot(txn: T, branch: Branch, hash: Blake2b256Hash): Unit = {
    _dbRoot.put(branch, Some(hash))
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
          _dbPastRoots.put(branch, Some(updatedPastRoots))
          currentRoot
      }
  }

  override private[rspace] def getPastRootsInBranch(txn: T, branch: Branch): Seq[Blake2b256Hash] = {
    _dbPastRoots.get(branch) match {
      case None =>
        val cold = trieStore.getPastRootsInBranch(txn, branch)
        _dbPastRoots.put(branch, Some(cold))
        cold
      case Some(result)=>
        result.getOrElse(Seq.empty)
    }
  }

  //TODO: change getAllPastRoots to return Branch->Seq[Blake2b256Hash] and cache results
  //in _dbPastRoots
  override private[rspace] def getAllPastRoots(txn: T) :  Seq[Blake2b256Hash] = {
    val pastRoots = trieStore.getAllPastRoots(txn)
    pastRoots
  }

  override private[rspace] def validateAndPutRoot(txn: T, branch: Branch,  hash: Blake2b256Hash) : Unit = {
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

  override private[rspace] def getEmptyRoot(txn: T) : Blake2b256Hash = {
    _dbEmptyRoot match {
      case None =>
        val cold = trieStore.getEmptyRoot(txn)
        _dbEmptyRoot = Some(Some(cold))
        cold
      case Some(Some(result)) =>
        result
      case Some(None) =>
        throw new LookupException("Empty root not found")
    }
  }

  override private[rspace] def putEmptyRoot(txn: T, hash: Blake2b256Hash): Unit = {
    _dbEmptyRoot = Some(Some(hash))
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
    _dbEmptyRoot = None
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
