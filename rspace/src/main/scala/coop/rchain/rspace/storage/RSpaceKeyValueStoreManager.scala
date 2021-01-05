package coop.rchain.rspace.storage

import java.nio.file.Path

import cats.effect.Concurrent
import coop.rchain.shared.Log
import coop.rchain.shared.store.LmdbDirStoreManager._
import coop.rchain.shared.store.LmdbDirStoreManager
import coop.rchain.store.{KeyValueStoreManager}

object RSpaceKeyValueStoreManager {
  def rspaceDbMapping(mapSize: Long = 1 * tb): Map[String, LmdbEnvConfig] =
    Map[String, LmdbEnvConfig](
      ("cold", LmdbEnvConfig(name = "cold", maxEnvSize = mapSize)),
      ("history", LmdbEnvConfig(name = "history", maxEnvSize = mapSize)),
      ("roots", LmdbEnvConfig(name = "roots", maxEnvSize = mapSize)),
      ("channels", LmdbEnvConfig(name = "channels", maxEnvSize = mapSize))
    )

  def apply[F[_]: Concurrent: Log](dirPath: Path): F[KeyValueStoreManager[F]] =
    LmdbDirStoreManager(dirPath, rspaceDbMapping())

  def apply[F[_]: Concurrent: Log](dirPath: Path, mapSize: Long): F[KeyValueStoreManager[F]] =
    LmdbDirStoreManager(dirPath, rspaceDbMapping(mapSize))
}
