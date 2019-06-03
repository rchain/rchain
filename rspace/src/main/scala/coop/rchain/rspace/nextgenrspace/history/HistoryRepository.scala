package coop.rchain.rspace.nextgenrspace.history

import cats.implicits._
import cats.effect.Sync
import coop.rchain.rspace.{Blake2b256Hash, HistoryReader, HotStoreAction}
import org.lmdbjava.EnvFlags
import scodec.Codec

trait HistoryRepository[F[_], C, P, A, K] extends HistoryReader[F, C, P, A, K] {
  def checkpoint(actions: List[HotStoreAction]): F[HistoryRepository[F, C, P, A, K]]

  def reset(root: Blake2b256Hash): F[HistoryRepository[F, C, P, A, K]]

  def history: History[F]

  def close(): F[Unit]
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
    pointerBlockStore: StoreConfig,
    rootsStore: StoreConfig
)

object HistoryRepositoryInstances {

  def lmdbRepository[F[_]: Sync, C, P, A, K](
      config: LMDBRSpaceStorageConfig
  )(
      implicit codecC: Codec[C],
      codecP: Codec[P],
      codecA: Codec[A],
      codecK: Codec[K]
  ): F[HistoryRepository[F, C, P, A, K]] = {
    val rootsLMDBStore  = StoreInstances.lmdbStore[F](config.rootsStore)
    val rootsRepository = new RootRepository[F](RootsStoreInstances.rootsStore[F](rootsLMDBStore))

    for {
      currentRoot      <- rootsRepository.currentRoot()
      coldLMDBStore    = StoreInstances.lmdbStore[F](config.coldStore)
      coldStore        = ColdStoreInstances.coldStore[F](coldLMDBStore)
      historyLMDBStore = StoreInstances.lmdbStore[F](config.historyStore)
      historyStore     = HistoryStoreInstances.historyStore[F](historyLMDBStore)
      pointerLMDBStore = StoreInstances.lmdbStore[F](config.pointerBlockStore)
      pointerBlockStore = PointerBlockStoreInstances.pointerBlockStore[F](
        pointerLMDBStore
      )
      history = HistoryInstances.noMerging(currentRoot, historyStore, pointerBlockStore)
    } yield HistoryRepositoryImpl[F, C, P, A, K](history, rootsRepository, coldStore)
  }
}
