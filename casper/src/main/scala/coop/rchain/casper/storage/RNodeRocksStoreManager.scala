package coop.rchain.casper.storage

import java.nio.file.Path

import cats.effect.Concurrent
import cats.effect.concurrent.{Deferred, Ref}
import cats.syntax.all._
import coop.rchain.shared.Log
import coop.rchain.store.{KeyValueStore, KeyValueStoreManager}

object RNodeRocksStoreManager {
  def apply[F[_]: Concurrent: Log](dirPath: Path): F[KeyValueStoreManager[F]] =
    Concurrent[F].delay(new RNodeRocksStoreManager(dirPath))
}

// The idea for this class is to manage all instances of key-value databases for RNode.
private final case class RNodeRocksStoreManager[F[_]: Concurrent: Log](
    dirPath: Path
) extends KeyValueStoreManager[F] {

  // Giga and tera byte
  val gb = 1024L * 1024L * 1024L
  val tb = 1024 * gb

  trait DbConfig {
    val name: String
  }

  private case class LmdbEnvConfig(
      name: String,
      // Max LMDB environment (file) size
      maxEnvSize: Long = 1 * gb
  ) extends DbConfig

  private case class RocksDbConfig(
      name: String
  ) extends DbConfig

  // Config name is used as a sub-folder for LMDB files
//  private val blockStorageEnvConfig = LmdbEnvConfig(name = "blockstorage", maxEnvSize = 1 * tb)
//  private val dagStorageEnvConfig   = LmdbEnvConfig(name = "dagstorage", maxEnvSize = 100 * gb)
//  // Temporary storage / cache
//  private val reportingEnvConfig    = LmdbEnvConfig(name = "reporting", maxEnvSize = 10 * tb)
//  private val casperBufferEnvConfig = LmdbEnvConfig(name = "casperbuffer")

  // Config name is used as a sub-folder for LMDB files
  private val blockStorageEnvConfig = RocksDbConfig(name = "blockstorage")
  private val dagStorageEnvConfig   = RocksDbConfig(name = "dagstorage")
  // Temporary storage / cache
  private val reportingEnvConfig    = RocksDbConfig(name = "reporting")
  private val casperBufferEnvConfig = RocksDbConfig(name = "casperbuffer")

  // Database name to store instance name mapping (sub-folder for LMDB store)
  // - keys with the same instance will be in one LMDB file (environment)
  private val dbInstanceMapping: Map[String, DbConfig] = Map(
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
                 val cfg            = dbInstanceMapping(dbName)
                 val manName        = cfg.name
                 val (isNew, defer) = st.envs.get(manName).fold((true, newDefer))((false, _))
                 val newSt          = st.copy(envs = st.envs.updated(manName, defer))
                 (newSt, (isNew, defer, cfg))
               }
      (isNew, defer, manCfg) = action
      createManager = manCfg match {
        case cfg: LmdbEnvConfig => createLmdbManger(cfg, defer)
        case cfg: RocksDbConfig => createRocksDbManger(cfg, defer)
      }
      _        <- createManager.whenA(isNew)
      manager  <- defer.get
      database <- manager.store(dbName)
    } yield database

  private def createLmdbManger(config: LmdbEnvConfig, defer: Deferred[F, KeyValueStoreManager[F]]) =
    for {
      manager <- LmdbStoreManager(dirPath.resolve(config.name), config.maxEnvSize)
      _       <- defer.complete(manager)
    } yield ()

  private def createRocksDbManger(
      config: RocksDbConfig,
      defer: Deferred[F, KeyValueStoreManager[F]]
  ) =
    for {
      manager <- RocksDbStoreManager[F](dirPath.resolve(config.name))
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
