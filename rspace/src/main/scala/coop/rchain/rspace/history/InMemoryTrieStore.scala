package coop.rchain.rspace.history

import scala.collection.immutable.Seq

import coop.rchain.rspace.{Blake2b256Hash, InMemoryOps, InMemTransaction, _}

import kamon.Kamon

final case class State[K, V](
    _dbTrie: Map[Blake2b256Hash, Trie[K, V]],
    _dbRoot: Map[Branch, Blake2b256Hash],
    _dbPastRoots: Set[Blake2b256Hash],
    _dbEmptyRoot: Option[Blake2b256Hash]
) {

  def changeTrie(newTrie: Map[Blake2b256Hash, Trie[K, V]]): State[K, V] =
    State(newTrie, _dbRoot, _dbPastRoots, _dbEmptyRoot)

  def changeRoot(newRoot: Map[Branch, Blake2b256Hash]): State[K, V] =
    State(_dbTrie, newRoot, _dbPastRoots, _dbEmptyRoot)

  def changePastRoots(newPastRoots: Set[Blake2b256Hash]): State[K, V] =
    State(_dbTrie, _dbRoot, newPastRoots, _dbEmptyRoot)

  def changeEmptyRoot(emptyRoot: Blake2b256Hash): State[K, V] =
    State(_dbTrie, _dbRoot, _dbPastRoots, Some(emptyRoot))
}

object State {
  def empty[K, V]: State[K, V] = State[K, V](Map.empty, Map.empty, Set.empty, None)
}

@SuppressWarnings(Array("org.wartremover.warts.Throw", "org.wartremover.warts.NonUnitStatements")) // TODO stop throwing exceptions
class InMemoryTrieStore[K, V]
    extends InMemoryOps[State[K, V]]
    with ITrieStore[InMemTransaction[State[K, V]], K, V] {

  override def emptyState: State[K, V] = State.empty

  private[this] val MetricsSource = RSpaceMetricsSource + ".history"
  private[this] val refine        = Map("path" -> "inmemTrie")
  private[this] val entriesGauge  = Kamon.gauge(MetricsSource + ".entries").refine(refine)

  private[rspace] def updateGauges(): Unit =
    updateGauges { txn =>
      txn.readState { state =>
        entriesGauge.set(state._dbTrie.size.toLong)
      }
    }

  private[rspace] override def getRoot(
      txn: InMemTransaction[State[K, V]],
      branch: Branch
  ): Option[Blake2b256Hash] =
    txn.readState(state => state._dbRoot.get(branch))

  private[rspace] override def persistAndGetRoot(
      txn: InMemTransaction[State[K, V]],
      branch: Branch
  ): Option[Blake2b256Hash] =
    getRoot(txn, branch)
      .map { currentRoot =>
        txn.writeState(
          state => (state.changePastRoots(state._dbPastRoots + currentRoot), ())
        )
        currentRoot
      }

  private[rspace] override def putRoot(
      txn: InMemTransaction[State[K, V]],
      branch: Branch,
      hash: Blake2b256Hash
  ): Unit =
    txn.writeState(state => (state.changeRoot(state._dbRoot + (branch -> hash)), ()))

  private[rspace] def getAllPastRoots(
      txn: InMemTransaction[State[K, V]]
  ): Set[Blake2b256Hash] =
    txn.readState(
      state => state._dbPastRoots
    )

  private[rspace] override def validateAndPutRoot(
      txn: InMemTransaction[State[K, V]],
      branch: Branch,
      hash: Blake2b256Hash
  ): Unit =
    getRoot(txn, branch)
      .find(_ == hash)
      .orElse {
        getAllPastRoots(txn)
          .find(_ == hash)
          .map { blake: Blake2b256Hash =>
            putRoot(txn, branch, blake)
            blake
          }
      }
      .orElse(throw new Exception(s"Unknown root."))

  private[rspace] override def put(
      txn: InMemTransaction[State[K, V]],
      key: Blake2b256Hash,
      value: Trie[K, V]
  ): Unit =
    txn.writeState(state => (state.changeTrie(state._dbTrie + (key -> value)), ()))

  private[rspace] override def get(
      txn: InMemTransaction[State[K, V]],
      key: Blake2b256Hash
  ): Option[Trie[K, V]] =
    txn.readState(state => state._dbTrie.get(key))

  private[rspace] override def toMap: Map[Blake2b256Hash, Trie[K, V]] =
    withTxn(createTxnRead()) { txn =>
      txn.readState(state => state._dbTrie)
    }

  private[rspace] override def clear(txn: InMemTransaction[State[K, V]]): Unit =
    txn.writeState(_ => (State.empty, ()))

  private[rspace] override def getEmptyRoot(txn: InMemTransaction[State[K, V]]) =
    txn.readState(_._dbEmptyRoot.getOrElse(throw new LookupException("Empty root not found")))

  private[rspace] override def putEmptyRoot(
      txn: InMemTransaction[State[K, V]],
      hash: Blake2b256Hash
  ): Unit =
    txn.writeState(state => (state.changeEmptyRoot(hash), ()))
}

object InMemoryTrieStore {
  def create[K, V](): ITrieStore[InMemTransaction[State[K, V]], K, V] =
    new InMemoryTrieStore[K, V]()
}
