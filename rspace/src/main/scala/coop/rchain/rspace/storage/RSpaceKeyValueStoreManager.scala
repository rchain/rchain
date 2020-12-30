package coop.rchain.rspace.storage

import java.nio.file.Path

import cats.effect.Concurrent
import cats.effect.concurrent.{Deferred, Ref}
import cats.syntax.all._
import coop.rchain.shared.Log
import coop.rchain.store.{KeyValueStore, KeyValueStoreManager, LmdbStoreManager}

object RSpaceKeyValueStoreManager {
  def apply[F[_]: Concurrent: Log](dirPath: Path): F[KeyValueStoreManager[F]] =
    Concurrent[F].delay(new RSpaceKeyValueStoreManager(dirPath))
}

private final case class RSpaceKeyValueStoreManager[F[_]: Concurrent: Log](
    dirPath: Path
) extends KeyValueStoreManager[F] {

  // Giga and tera bytes
  val gb = 1024L * 1024L * 1024L
  val tb = 1024 * gb

  private case class LmdbEnvConfig(
      lmdbDir: String,
      // Max LMDB environment (file) size
      maxEnvSize: Long = 1 * gb,
      DBName: String
  )

  // Config name is used as a sub-folder for LMDB files
  private val coldStoreEnvConfig =
    LmdbEnvConfig(lmdbDir = "cold", maxEnvSize = 1 * tb, DBName = "db")
  private val historyStoreEnvConfig =
    LmdbEnvConfig(lmdbDir = "history", maxEnvSize = 1 * tb, DBName = "db")
  private val rootsStoreEnvConfig =
    LmdbEnvConfig(lmdbDir = "roots", maxEnvSize = 1 * tb, DBName = "db")
  private val channelsStoreEnvConfig =
    LmdbEnvConfig(lmdbDir = "channels", maxEnvSize = 1 * tb, DBName = "db")

  private val dbInstanceMapping: Map[String, LmdbEnvConfig] = Map[String, LmdbEnvConfig](
    ("cold", coldStoreEnvConfig),
    ("history", historyStoreEnvConfig),
    ("roots", rootsStoreEnvConfig),
    ("channels", channelsStoreEnvConfig)
  )

  private case class StoreState(
      envs: Map[String, Deferred[F, KeyValueStoreManager[F]]]
  )

  private val managersState = Ref.unsafe(StoreState(Map.empty))

  // Creates database uniquely defined by the name
  override def store(dbName: String): F[KeyValueStore[F]] =
    for {
      // Find DB ref / first time create deferred object
      newDefer <- Deferred[F, KeyValueStoreManager[F]]
      action <- managersState.modify { st =>
                 val cfg @ LmdbEnvConfig(lmdbDir, _, _) = dbInstanceMapping(dbName)
                 val (isNew, defer)                     = st.envs.get(lmdbDir).fold((true, newDefer))((false, _))
                 val newSt                              = st.copy(envs = st.envs.updated(lmdbDir, defer))
                 (newSt, (isNew, defer, cfg))
               }
      (isNew, defer, manCfg) = action
      _                      <- createLmdbManger(manCfg, defer).whenA(isNew)
      manager                <- defer.get
      database               <- manager.store(manCfg.DBName)
    } yield database

  private def createLmdbManger(config: LmdbEnvConfig, defer: Deferred[F, KeyValueStoreManager[F]]) =
    for {
      manager <- LmdbStoreManager(dirPath.resolve(config.lmdbDir), config.maxEnvSize)
      _       <- defer.complete(manager)
    } yield ()

  override def shutdown: F[Unit] = {
    import cats.instances.vector._
    for {
      st <- managersState.get
      // Shutdown all store managers
      _ <- st.envs.values.toVector.traverse_(_.get.flatMap(_.shutdown))
    } yield ()
  }
}
