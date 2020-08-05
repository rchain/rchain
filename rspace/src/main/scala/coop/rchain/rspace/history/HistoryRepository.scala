package coop.rchain.rspace.history

import cats.implicits._
import cats.effect.Concurrent
import cats.Parallel
import coop.rchain.rspace.state.{RSpaceExporter, RSpaceImporter}
import coop.rchain.rspace.state.instances.{RSpaceExporterImpl, RSpaceImporterImpl}
import coop.rchain.rspace.{Blake2b256Hash, HistoryReader, HotStoreAction}
import coop.rchain.store.KeyValueStoreManager
import org.lmdbjava.EnvFlags
import scodec.Codec

trait HistoryRepository[F[_], C, P, A, K] extends HistoryReader[F, C, P, A, K] {
  def checkpoint(actions: List[HotStoreAction]): F[HistoryRepository[F, C, P, A, K]]

  def reset(root: Blake2b256Hash): F[HistoryRepository[F, C, P, A, K]]

  def history: History[F]

  def close(): F[Unit]

  def exporter: F[RSpaceExporter[F]]

  def importer: F[RSpaceImporter[F]]
}

final case class LMDBStorageConfig(
    path: String,
    mapSize: Long,
    maxReaders: Int = 2048,
    maxDbs: Int = 2,
    flags: List[EnvFlags] = Nil,
    dbNamePrefix: String = "db"
)
final case class LMDBRSpaceStorageConfig(
    coldStore: StoreConfig,
    historyStore: StoreConfig,
    rootsStore: StoreConfig
)

object HistoryRepositoryInstances {

  def lmdbRepository[F[_]: Concurrent: Parallel, C, P, A, K](
      config: LMDBRSpaceStorageConfig
  )(
      implicit codecC: Codec[C],
      codecP: Codec[P],
      codecA: Codec[A],
      codecK: Codec[K]
  ): F[HistoryRepository[F, C, P, A, K]] =
    for {
      // Roots store
      rootsLMDBStore  <- StoreInstances.lmdbStore[F](config.rootsStore)
      rootsRepository = new RootRepository[F](RootsStoreInstances.rootsStore[F](rootsLMDBStore))
      currentRoot     <- rootsRepository.currentRoot()
      // Cold store
      coldLMDBStore <- StoreInstances.lmdbStore[F](config.coldStore)
      coldStore     = ColdStoreInstances.coldStore[F](coldLMDBStore)
      // History store
      historyLMDBStore <- StoreInstances.lmdbStore[F](config.historyStore)
      historyStore     = HistoryStoreInstances.historyStore[F](historyLMDBStore)
      history          = HistoryInstances.merging(currentRoot, historyStore)
      // RSpace importer/exporter / directly operates on Store (lmdb)
      exporter = RSpaceExporterImpl[F](historyLMDBStore, coldLMDBStore, rootsLMDBStore)
      importer = RSpaceImporterImpl[F](historyLMDBStore, coldLMDBStore, rootsLMDBStore)
    } yield HistoryRepositoryImpl[F, C, P, A, K](
      history,
      rootsRepository,
      coldStore,
      exporter,
      importer
    )
}
