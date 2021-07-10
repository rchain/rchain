package coop.rchain.casper.storage

import cats.effect.Concurrent
import cats.syntax.all._
import coop.rchain.shared.Log
import coop.rchain.store.LmdbDirStoreManager.{gb, tb, Db, LmdbEnvConfig}
import coop.rchain.store.{KeyValueStoreManager, LmdbDirStoreManager}

import java.nio.file.Path

object RNodeKeyValueStoreManager {
  def apply[F[_]: Concurrent: Log](
      dirPath: Path,
      legacyRSpacePaths: Boolean = false
  ): F[KeyValueStoreManager[F]] =
    LmdbDirStoreManager[F](dirPath, rnodeDbMapping(legacyRSpacePaths).toMap)

  // Config name is used as a sub-folder for LMDB files

  // RSpace
  private val rspaceHistoryEnvConfig = LmdbEnvConfig(name = "rspace/history", maxEnvSize = 1 * tb)
  private val rspaceColdEnvConfig    = LmdbEnvConfig(name = "rspace/cold", maxEnvSize = 1 * tb)
  // Temporary channel store, remove in hard fork
  private val rspaceChannelsMapEnvConfig =
    LmdbEnvConfig(name = "rspace/channels", maxEnvSize = 1 * tb)
  // RSpace evaluator
  private val evalHistoryEnvConfig = LmdbEnvConfig(name = "eval/history", maxEnvSize = 1 * tb)
  private val evalColdEnvConfig    = LmdbEnvConfig(name = "eval/cold", maxEnvSize = 1 * tb)
  // Blocks
  private val blockStorageEnvConfig  = LmdbEnvConfig(name = "blockstorage", maxEnvSize = 1 * tb)
  private val dagStorageEnvConfig    = LmdbEnvConfig(name = "dagstorage", maxEnvSize = 100 * gb)
  private val deployStorageEnvConfig = LmdbEnvConfig(name = "deploystorage", maxEnvSize = 1 * gb)
  // Temporary storage / cache
  private val casperBufferEnvConfig = LmdbEnvConfig(name = "casperbuffer")
  private val reportingEnvConfig    = LmdbEnvConfig(name = "reporting", maxEnvSize = 10 * tb)
  private val transactionEnvConfig  = LmdbEnvConfig(name = "transaction")

  // Legacy RSpace paths
  val legacyRSpacePathPrefix = "rspace/casper/v2"
  private def legacyEnvConfig(dir: String) =
    LmdbEnvConfig(s"$legacyRSpacePathPrefix/$dir", maxEnvSize = 1 * tb)

  // Database name to store instance name mapping (sub-folder for LMDB store)
  // - keys with the same instance will be in one LMDB file (environment)
  def rnodeDbMapping(legacyRSpacePaths: Boolean = false): Seq[(Db, LmdbEnvConfig)] =
    Seq(
      // Block storage
      (Db("blocks"), blockStorageEnvConfig),
      // Block metadata storage
      (Db("blocks-approved"), dagStorageEnvConfig),
      (Db("block-metadata"), dagStorageEnvConfig),
      (Db("equivocation-tracker"), dagStorageEnvConfig),
      (Db("latest-messages"), dagStorageEnvConfig),
      (Db("invalid-blocks"), dagStorageEnvConfig),
      (Db("deploy-index"), dagStorageEnvConfig),
      (Db("last-finalized-block"), dagStorageEnvConfig),
      // Deploy storage
      (Db("deploy_storage"), deployStorageEnvConfig),
      // Reporting (trace) cache
      (Db("reporting-cache"), reportingEnvConfig),
      // CasperBuffer
      (Db("parents-map"), casperBufferEnvConfig),
      // Rholang evaluator store
      (Db("eval-history"), evalHistoryEnvConfig),
      (Db("eval-roots"), evalHistoryEnvConfig),
      (Db("eval-cold"), evalColdEnvConfig),
      // transaction store
      (Db("transaction"), transactionEnvConfig)
    ) ++ (
      // RSpace
      if (!legacyRSpacePaths) {
        // History and roots maps are part of the same LMDB file (environment)
        Seq(
          (Db("rspace-history"), rspaceHistoryEnvConfig),
          (Db("rspace-roots"), rspaceHistoryEnvConfig),
          (Db("rspace-cold"), rspaceColdEnvConfig),
          // TEMP: remove after hard fork
          (Db("rspace-channels"), rspaceChannelsMapEnvConfig)
        )
      } else
        // Legacy config has the same database name for all maps
        Seq(
          (Db("rspace-history", nameOverride = "db".some), legacyEnvConfig("history")),
          (Db("rspace-roots", nameOverride = "db".some), legacyEnvConfig("roots")),
          (Db("rspace-cold", nameOverride = "db".some), legacyEnvConfig("cold")),
          (Db("rspace-channels", nameOverride = "db".some), legacyEnvConfig("channels"))
        )
    )
}
