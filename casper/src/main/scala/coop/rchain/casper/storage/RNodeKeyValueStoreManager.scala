package coop.rchain.casper.storage

import java.nio.file.Path

import cats.effect.Concurrent
import cats.effect.concurrent.{Deferred, Ref}
import cats.syntax.all._
import coop.rchain.shared.Log
import coop.rchain.store.{KeyValueStore, KeyValueStoreManager}

object RNodeKeyValueStoreManager {
  def apply[F[_]: Concurrent: Log](dirPath: Path): F[KeyValueStoreManager[F]] =
    Concurrent[F].delay(new RNodeKeyValueStoreManager(dirPath))
}

// The idea for this class is to manage all instances of key-value databases for RNode.
// For LMDB this allows control which databases are part of the same environment (file).
private final case class RNodeKeyValueStoreManager[F[_]: Concurrent: Log](
    dirPath: Path
) extends KeyValueStoreManager[F] {

  // Giga and tera bytes
  val gb = 1024L * 1024L * 1024L
  val tb = 1024 * gb

  private case class LmdbEnvConfig(
      name: String,
      // Max LMDB environment (file) size
      maxEnvSize: Long = 1 * gb
  )

  // Config name is used as a sub-folder for LMDB files
  private val blockStorageEnvConfig = LmdbEnvConfig(name = "blockstorage", maxEnvSize = 1 * tb)
  private val dagStorageEnvConfig   = LmdbEnvConfig(name = "dagstorage", maxEnvSize = 100 * gb)
  // Temporary storage / cache
  private val casperBufferEnvConfig = LmdbEnvConfig(name = "casperbuffer")
  private val reportingEnvConfig    = LmdbEnvConfig(name = "reporting", maxEnvSize = 10 * tb)

  // Database name to store instance name mapping (sub-folder for LMDB store)
  // - keys with the same instance will be in one LMDB file (environment)
  private val dbInstanceMapping: Map[String, LmdbEnvConfig] = Map[String, LmdbEnvConfig](
    // Block storage
    ("blocks", blockStorageEnvConfig),
    // Block metadata storage
    ("blocks-approved", dagStorageEnvConfig),
    ("block-metadata", dagStorageEnvConfig),
    ("equivocation-tracker", dagStorageEnvConfig),
    ("latest-messages", dagStorageEnvConfig),
    ("invalid-blocks", dagStorageEnvConfig),
    ("deploy-index", dagStorageEnvConfig),
    // Reporting (trace) cache
    ("reporting-cache", reportingEnvConfig),
    // CasperBuffer
    ("parents-map", casperBufferEnvConfig)
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
                 val cfg @ LmdbEnvConfig(manName, _) = dbInstanceMapping(dbName)
                 val (isNew, defer)                  = st.envs.get(manName).fold((true, newDefer))((false, _))
                 val newSt                           = st.copy(envs = st.envs.updated(manName, defer))
                 (newSt, (isNew, defer, cfg))
               }
      (isNew, defer, manCfg) = action
      _                      <- createLmdbManger(manCfg, defer).whenA(isNew)
      manager                <- defer.get
      database               <- manager.store(dbName)
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
