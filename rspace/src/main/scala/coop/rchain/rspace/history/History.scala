package coop.rchain.rspace.history

import cats.Parallel
import cats.effect.{Async, Sync}
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.instances.RadixHistory
import coop.rchain.store.KeyValueStore
import scodec.bits.ByteVector

/**
  * History definition represents key-value API for RSpace tuple space
  *
  * [[History]] contains only references to data stored on keys ([[KeySegment]]).
  *
  * [[ColdStoreInstances.ColdKeyValueStore]] holds full data referenced by [[LeafPointer]] in [[History]].
  */
trait History[F[_]] {
  type HistoryF <: History[F]

  /**
    * Read operation on the Merkle tree
    */
  def read(key: KeySegment): F[Option[Blake2b256Hash]]

  /**
    * Insert/update/delete operations on the underlying Merkle tree (key-value store)
    */
  def process(actions: List[HistoryAction]): F[HistoryF]

  /**
    * Get the root of the Merkle tree
    */
  def root: Blake2b256Hash

  /**
    * Returns History with specified root pointer
    */
  def reset(root: Blake2b256Hash): F[HistoryF]
}

object History {
  val emptyRootHash: Blake2b256Hash = RadixHistory.emptyRootHash

  def create[F[_]: Async: Sync: Parallel](
      root: Blake2b256Hash,
      store: KeyValueStore[F]
  ): F[RadixHistory[F]] = RadixHistory(root, RadixHistory.createStore(store))
}
