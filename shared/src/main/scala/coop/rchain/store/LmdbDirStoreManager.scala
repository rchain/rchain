package coop.rchain.shared.store

import java.nio.file.Path

import cats.effect.Concurrent
import cats.effect.concurrent.{Deferred, Ref}
import cats.syntax.all._
import coop.rchain.shared.Log
import coop.rchain.shared.store.LmdbDirStoreManager._
import coop.rchain.store.{KeyValueStore, KeyValueStoreManager, LmdbStoreManager}

object LmdbDirStoreManager {
  def apply[F[_]: Concurrent: Log](
      dirPath: Path,
      dbInstanceMapping: Map[String, LmdbEnvConfig],
      legacyDBHandle: (String, LmdbEnvConfig) => String = (dbName, _) => dbName
  ): F[KeyValueStoreManager[F]] =
    Concurrent[F].delay(new LmdbDirStoreManager(dirPath, dbInstanceMapping, legacyDBHandle))

  // Giga and tera bytes
  val gb = 1024L * 1024L * 1024L
  val tb = 1024 * gb
  final case class LmdbEnvConfig(
      name: String,
      // Max LMDB environment (file) size
      maxEnvSize: Long = 1 * gb
  )
}

// The idea for this class is to manage multiple of key-value lmdb databases.
// For LMDB this allows control which databases are part of the same environment (file).
private final case class LmdbDirStoreManager[F[_]: Concurrent: Log](
    dirPath: Path,
    dbInstanceMapping: Map[String, LmdbEnvConfig],
    legacyDBHandle: (String, LmdbEnvConfig) => String
) extends KeyValueStoreManager[F] {

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
                 val cfg @ LmdbEnvConfig(manName, _) = dbInstanceMapping(dbName)
                 val (isNew, defer)                  = st.envs.get(manName).fold((true, newDefer))((false, _))
                 val newSt                           = st.copy(envs = st.envs.updated(manName, defer))
                 (newSt, (isNew, defer, cfg))
               }
      (isNew, defer, manCfg) = action
      _                      <- createLmdbManger(manCfg, defer).whenA(isNew)
      manager                <- defer.get
      dataBaseName           = legacyDBHandle(dbName, dbInstanceMapping(dbName))
      database               <- manager.store(dataBaseName)
    } yield database

  private def createLmdbManger(config: LmdbEnvConfig, defer: Deferred[F, KeyValueStoreManager[F]]) =
    for {
      manager <- LmdbStoreManager(dirPath.resolve(config.name), config.maxEnvSize)
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
