package coop.rchain.rspace.history
import coop.rchain.rspace.{Blake2b256Hash, InMemTransaction, InMemoryOps}
import kamon.Kamon

import scala.collection.immutable.Seq

case class State[K, V](
    _dbTrie: Map[Blake2b256Hash, Trie[K, V]],
    _dbRoot: Map[Branch, Blake2b256Hash],
    _dbPastRoots: Map[Branch, Seq[Blake2b256Hash]],
    _dbEmptyRoot: Option[Blake2b256Hash]
) {

  def changeTrie(newTrie: Map[Blake2b256Hash, Trie[K, V]]): State[K, V] =
    State(newTrie, _dbRoot, _dbPastRoots, _dbEmptyRoot)

  def changeRoot(newRoot: Map[Branch, Blake2b256Hash]): State[K, V] =
    State(_dbTrie, newRoot, _dbPastRoots, _dbEmptyRoot)

  def changePastRoots(newPastRoots: Map[Branch, Seq[Blake2b256Hash]]): State[K, V] =
    State(_dbTrie, _dbRoot, newPastRoots, _dbEmptyRoot)

  def changeEmptyRoot(emptyRoot: Blake2b256Hash): State[K, V] =
    State(_dbTrie, _dbRoot, _dbPastRoots, Some(emptyRoot))
}

object State {
  def empty[K, V]: State[K, V] = State[K, V](Map.empty, Map.empty, Map.empty, None)
}

class InMemoryTrieStore[K, V]
    extends InMemoryOps[State[K, V]]
    with ITrieStore[InMemTransaction[State[K, V]], K, V] {

  override def emptyState: State[K, V] = State.empty

  private[this] val refine       = Map("path" -> "inmemTrie")
  private[this] val entriesGauge = Kamon.gauge("entries").refine(refine)

  override private[rspace] def updateGauges(): Unit =
    withTxn(createTxnRead())(_.readState { state =>
      entriesGauge.set(state._dbTrie.size.toLong)
    })

  override private[rspace] def getRoot(
      txn: InMemTransaction[State[K, V]],
      branch: Branch
  ): Option[Blake2b256Hash] =
    txn.readState(state => state._dbRoot.get(branch))

  override private[rspace] def persistAndGetRoot(
      txn: InMemTransaction[State[K, V]],
      branch: Branch
  ): Option[Blake2b256Hash] =
    getRoot(txn, branch)
      .map { currentRoot =>
        val pastRoots = getPastRootsInBranch(txn, branch).filter(_ != currentRoot)
        (currentRoot, currentRoot +: pastRoots)
      }
      .map {
        case (currentRoot, updatedPastRoots) =>
          txn.writeState(
            state => (state.changePastRoots(state._dbPastRoots + (branch -> updatedPastRoots)), ())
          )
          currentRoot
      }

  private[this] def getPastRootsInBranch(
      txn: InMemTransaction[State[K, V]],
      branch: Branch
  ): Seq[Blake2b256Hash] =
    txn.readState(state => state._dbPastRoots.getOrElse(branch, Seq.empty))

  override private[rspace] def putRoot(
      txn: InMemTransaction[State[K, V]],
      branch: Branch,
      hash: Blake2b256Hash
  ): Unit =
    txn.writeState(state => (state.changeRoot(state._dbRoot + (branch -> hash)), ()))

  override private[rspace] def getAllPastRoots(
      txn: InMemTransaction[State[K, V]]
  ): Seq[Blake2b256Hash] =
    txn.readState(
      state =>
        state._dbPastRoots.values.foldLeft(Seq.empty[Blake2b256Hash])((acc, value) => acc ++ value)
    )

  override private[rspace] def validateAndPutRoot(
      txn: InMemTransaction[State[K, V]],
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
      .orElse(throw new Exception(s"Unknown root."))

  override private[rspace] def put(
      txn: InMemTransaction[State[K, V]],
      key: Blake2b256Hash,
      value: Trie[K, V]
  ): Unit =
    txn.writeState(state => (state.changeTrie(state._dbTrie + (key -> value)), ()))

  override private[rspace] def get(
      txn: InMemTransaction[State[K, V]],
      key: Blake2b256Hash
  ): Option[Trie[K, V]] =
    txn.readState(state => state._dbTrie.get(key))

  override private[rspace] def toMap: Map[Blake2b256Hash, Trie[K, V]] =
    withTxn(createTxnRead()) { txn =>
      txn.readState(state => state._dbTrie)
    }

  override private[rspace] def clear(txn: InMemTransaction[State[K, V]]): Unit =
    txn.writeState(_ => (State.empty, ()))

  override private[rspace] def getEmptyRoot(txn: InMemTransaction[State[K, V]]) =
    txn.readState(_._dbEmptyRoot.getOrElse(throw new LookupException("Empty root not found")))

  override private[rspace] def putEmptyRoot(
      txn: InMemTransaction[State[K, V]],
      hash: Blake2b256Hash
  ): Unit =
    txn.writeState(state => (state.changeEmptyRoot(hash), ()))
}

object InMemoryTrieStore {
  def create[K, V](): ITrieStore[InMemTransaction[State[K, V]], K, V] =
    new InMemoryTrieStore[K, V]()
}
