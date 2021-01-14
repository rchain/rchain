package coop.rchain.rspace.history

import cats.Parallel
import cats.effect.Concurrent
import cats.syntax.all._
import coop.rchain.rspace.state.instances.{RSpaceExporterStore, RSpaceImporterStore}
import coop.rchain.rspace.state.{RSpaceExporter, RSpaceImporter}
import coop.rchain.rspace.{Blake2b256Hash, HistoryReader, HotStoreAction}
import coop.rchain.store.KeyValueStore
import scodec.Codec

trait HistoryRepository[F[_], C, P, A, K] extends HistoryReader[F, C, P, A, K] {
  def checkpoint(actions: List[HotStoreAction]): F[HistoryRepository[F, C, P, A, K]]

  def reset(root: Blake2b256Hash): F[HistoryRepository[F, C, P, A, K]]

  def history: History[F]

  def exporter: F[RSpaceExporter[F]]

  def importer: F[RSpaceImporter[F]]
}

object HistoryRepositoryInstances {

  def lmdbRepository[F[_]: Concurrent: Parallel, C, P, A, K](
      historyKeyValueStore: KeyValueStore[F],
      rootsKeyValueStore: KeyValueStore[F],
      coldKeyValueStore: KeyValueStore[F]
  )(
      implicit codecC: Codec[C],
      codecP: Codec[P],
      codecA: Codec[A],
      codecK: Codec[K]
  ): F[HistoryRepository[F, C, P, A, K]] = {
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
    } yield HistoryRepositoryImpl[F, C, P, A, K](
      history,
      rootsRepository,
      coldStore,
      exporter,
      importer
    )
  }
}
