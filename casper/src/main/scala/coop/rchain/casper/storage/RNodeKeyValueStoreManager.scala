package coop.rchain.casper.storage

import java.nio.file.Path

import cats.effect.Concurrent
import cats.effect.concurrent.{Deferred, Ref}
import cats.syntax.all._
import coop.rchain.shared.Log
import coop.rchain.shared.store.LmdbDirStoreManager._
import coop.rchain.shared.store.LmdbDirStoreManager
import coop.rchain.store.KeyValueStoreManager

object RNodeKeyValueStoreManager {
  // Config name is used as a sub-folder for LMDB files
  private val blockStorageEnvConfig = LmdbEnvConfig(name = "blockstorage", maxEnvSize = 1 * tb)
  private val dagStorageEnvConfig   = LmdbEnvConfig(name = "dagstorage", maxEnvSize = 100 * gb)
  // Temporary storage / cache
  private val casperBufferEnvConfig = LmdbEnvConfig(name = "casperbuffer")
  private val reportingEnvConfig    = LmdbEnvConfig(name = "reporting", maxEnvSize = 10 * tb)

  // Database name to store instance name mapping (sub-folder for LMDB store)
  // - keys with the same instance will be in one LMDB file (environment)
  private val rnodeDbMapping: Map[String, LmdbEnvConfig] = Map[String, LmdbEnvConfig](
    // Block storage
    ("blocks", blockStorageEnvConfig),
    // Block metadata storage
    ("blocks-approved", dagStorageEnvConfig),
    ("block-metadata", dagStorageEnvConfig),
    ("equivocation-tracker", dagStorageEnvConfig),
    ("latest-messages", dagStorageEnvConfig),
    ("invalid-blocks", dagStorageEnvConfig),
    ("deploy-index", dagStorageEnvConfig),
    ("last-finalized-block", dagStorageEnvConfig),
    // Reporting (trace) cache
    ("reporting-cache", reportingEnvConfig),
    // CasperBuffer
    ("parents-map", casperBufferEnvConfig)
  )

  def apply[F[_]: Concurrent: Log](dirPath: Path): F[KeyValueStoreManager[F]] =
    LmdbDirStoreManager(dirPath, rnodeDbMapping)
}
