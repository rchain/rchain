package coop.rchain.rspace.storage

import java.nio.file.Path

import cats.effect.Concurrent
import coop.rchain.shared.Log
import coop.rchain.shared.store.LmdbDirStoreManager._
import coop.rchain.shared.store.LmdbDirStoreManager
import coop.rchain.store.{KeyValueStoreManager}

object RSpaceKeyValueStoreManager {
  // Config name is used as a sub-folder for LMDB files
  private val coldStoreEnvConfig =
    LmdbEnvConfig(name = "cold", maxEnvSize = 1 * tb)
  private val historyStoreEnvConfig =
    LmdbEnvConfig(name = "history", maxEnvSize = 1 * tb)
  private val rootsStoreEnvConfig =
    LmdbEnvConfig(name = "roots", maxEnvSize = 1 * tb)
  private val channelsStoreEnvConfig =
    LmdbEnvConfig(name = "channels", maxEnvSize = 1 * tb)

  val rspaceDbMapping: Map[String, LmdbEnvConfig] = Map[String, LmdbEnvConfig](
    ("db-cold", coldStoreEnvConfig),
    ("db-history", historyStoreEnvConfig),
    ("db-roots", rootsStoreEnvConfig),
    ("channels", channelsStoreEnvConfig)
  )
  def apply[F[_]: Concurrent: Log](dirPath: Path): F[KeyValueStoreManager[F]] =
    LmdbDirStoreManager(dirPath, rspaceDbMapping)
}
