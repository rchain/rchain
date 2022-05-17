package coop.rchain.rspace.history

import cats.effect.{Concurrent, Sync}
import cats.{Parallel, Show}
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.History._
import coop.rchain.rspace.history.instances.RadixHistory
import coop.rchain.store.KeyValueStore
import scodec.bits.ByteVector

/**
  * History definition represents key-value API for RSpace tuple space
  *
  * [[History]] contains only references to data stored on keys ([[KeyPath]]).
  *
  * [[ColdStoreInstances.ColdKeyValueStore]] holds full data referenced by [[LeafPointer]] in [[History]].
  */
trait History[F[_]] {
  type HistoryF <: History[F]

  /**
    * Read operation on the Merkle tree
    */
  def read(key: ByteVector): F[Option[Blake2b256Hash]]

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

  def create[F[_]: Concurrent: Sync: Parallel](
      root: Blake2b256Hash,
      store: KeyValueStore[F]
  ): F[RadixHistory[F]] = RadixHistory(root, RadixHistory.createStore(store))

  type KeyPath = Seq[Byte]
  type KeyWord = Byte

  // Key segment type class implementation for sequence of bytes
  implicit val kpKs = new KeySegment[KeyPath, KeyWord] {
    override def size(ks: KeyPath): Int                   = ks.size
    override def isEmpty(ks: KeyPath): Boolean            = ks.isEmpty
    override def headOption(ks: KeyPath): Option[Byte]    = ks.headOption
    override def tailOption(ks: KeyPath): Option[KeyPath] = Some(ks.tail)
    override def empty: KeyPath                           = Seq.empty[Byte]
    override def :+(k: KeyPath, w: KeyWord): KeyPath      = k :+ w
    override def ++(k1: KeyPath, k2: KeyPath): KeyPath    = k1 ++ k2
  }
  implicit val kpShow = new Show[KeyPath] {
    override def show(t: KeyPath): String = t.map(_.toString).mkString(" ")
  }
}
