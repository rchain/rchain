package coop.rchain.blockstorage.dag

import java.nio.file.Path

import cats.effect.Sync
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.blockstorage.BlockHashesByDeployLogIsCorrupted
import coop.rchain.blockstorage.util.io.IOError.RaiseIOError
import coop.rchain.blockstorage.dag.codecs._
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.shared.Log

object DeployPersistentIndex {
  type PersistentDeployIndex[F[_]] = PersistentIndex[F, DeployId, BlockHash]

  def load[F[_]: Sync: Log: RaiseIOError](
      logPath: Path,
      crcPath: Path
  ): F[PersistentDeployIndex[F]] =
    PersistentIndex.load[F, DeployId, BlockHash](
      logPath,
      crcPath,
      BlockHashesByDeployLogIsCorrupted
    )(Sync[F], Log[F], RaiseIOError[F], codecDeployId, codecBlockHash)
}
