package coop.rchain.blockstorage.dag

import java.nio.file.Path

import cats.effect.Sync
import coop.rchain.blockstorage.LatestMessagesLogIsCorrupted
import coop.rchain.blockstorage.dag.codecs._
import coop.rchain.blockstorage.util.io.IOError.RaiseIOError
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.shared.Log

object LatestMessagesPersistentIndex {
  type PersistentLatestMessagesIndex[F[_]] = PersistentIndex[F, Validator, BlockHash]

  def load[F[_]: Sync: Log: RaiseIOError](
      logPath: Path,
      crcPath: Path
  ): F[PersistentLatestMessagesIndex[F]] =
    PersistentIndex.load[F, Validator, BlockHash](
      logPath,
      crcPath,
      LatestMessagesLogIsCorrupted
    )(Sync[F], Log[F], RaiseIOError[F], codecValidator, codecBlockHash)
}
