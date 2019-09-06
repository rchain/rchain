package coop.rchain.blockstorage.dag

import java.nio.file.Path

import cats.effect.Sync
import coop.rchain.blockstorage.InvalidBlocksIsCorrupted
import coop.rchain.blockstorage.dag.codecs._
import coop.rchain.blockstorage.util.io.IOError.RaiseIOError
import coop.rchain.models.BlockMetadata
import coop.rchain.shared.Log
import scodec.codecs._

object InvalidBlocksPersistentIndex {
  type PersistentInvalidBlocksIndex[F[_]] = PersistentIndex[F, BlockMetadata, Unit]

  def load[F[_]: Sync: Log: RaiseIOError](
      logPath: Path,
      crcPath: Path
  ): F[PersistentInvalidBlocksIndex[F]] =
    PersistentIndex.load[F, BlockMetadata, Unit](
      logPath,
      crcPath,
      InvalidBlocksIsCorrupted
    )(Sync[F], Log[F], RaiseIOError[F], codecBlockMetadata, ignore(0))
}
