package coop.rchain.rspace.history

import cats.Parallel
import cats.effect.Concurrent
import cats.syntax.all._
import coop.rchain.metrics.Span
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.merger.StateMerger
import coop.rchain.rspace.serializers.ScodecSerialize.{DatumB, JoinsB, WaitingContinuationB}
import coop.rchain.rspace.state.instances.{RSpaceExporterStore, RSpaceImporterStore}
import coop.rchain.rspace.state.{RSpaceExporter, RSpaceImporter}
import coop.rchain.rspace.{Channel, HotStoreAction, HotStoreTrieAction}
import coop.rchain.shared.{Log, Serialize}
import coop.rchain.store.{KeyValueStore, LazyAdHocKeyValueCache}

/**
  * Pointer to data in history (Datums, Continuations or Joins)
  * @param state - state hash
  * @param hash - hash of a leaf
  */
final case class HistoryPointer(state: Blake2b256Hash, hash: Blake2b256Hash)

/**
  * Cache of decoded values from history
  */
final case class HistoryCache[F[_], P, A, K](
    dtsCache: LazyAdHocKeyValueCache[F, HistoryPointer, Seq[DatumB[A]]],
    wksCache: LazyAdHocKeyValueCache[F, HistoryPointer, Seq[WaitingContinuationB[P, K]]],
    jnsCache: LazyAdHocKeyValueCache[F, HistoryPointer, Seq[JoinsB]]
)

trait HistoryRepository[F[_], C, P, A, K] {
  def checkpoint(actions: List[HotStoreAction]): F[HistoryRepository[F, C, P, A, K]]

  def doCheckpoint(actions: Seq[HotStoreTrieAction]): F[HistoryRepository[F, C, P, A, K]]

  def reset(root: Blake2b256Hash): F[HistoryRepository[F, C, P, A, K]]

  def history: History[F]

  def exporter: F[RSpaceExporter[F]]

  def importer: F[RSpaceImporter[F]]

  def stateMerger: StateMerger[F]

  def getHistoryReader(stateHash: Blake2b256Hash): HistoryReader[F, Blake2b256Hash, P, A, K]

  def root: Blake2b256Hash
}

object HistoryRepositoryInstances {

  val PREFIX_DATUM: Byte = 0x00
  val PREFIX_KONT: Byte  = 0x01
  val PREFIX_JOINS: Byte = 0x02

  def lmdbRepository[F[_]: Concurrent: Parallel: Log: Span, P, A, K](
      historyKeyValueStore: KeyValueStore[F],
      rootsKeyValueStore: KeyValueStore[F],
      coldKeyValueStore: KeyValueStore[F]
  )(
      implicit
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K]
  ): F[HistoryRepository[F, Channel, P, A, K]] = {
    // Roots store
    val rootsRepository = new RootRepository[F](
      RootsStoreInstances.rootsStore[F](rootsKeyValueStore)
    )
    for {
      currentRoot <- rootsRepository.currentRoot()
      // History store
      historyStore = HistoryStoreInstances.historyStore[F](historyKeyValueStore)
      history      = HistoryInstances.merging(currentRoot, historyStore)
      // Cold store
      coldStore = ColdStoreInstances.coldStore[F](coldKeyValueStore)
      // RSpace importer/exporter / directly operates on Store (lmdb)
      exporter = RSpaceExporterStore[F](historyKeyValueStore, coldKeyValueStore, rootsKeyValueStore)
      importer = RSpaceImporterStore[F](historyKeyValueStore, coldKeyValueStore, rootsKeyValueStore)
    } yield HistoryRepositoryImpl[F, P, A, K](
      history,
      rootsRepository,
      coldStore,
      exporter,
      importer,
      sp,
      sa,
      sk
    )
  }
}
