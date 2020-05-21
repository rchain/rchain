package coop.rchain.blockstorage.dag

import java.nio.file.Path

import cats.effect.Sync
import coop.rchain.blockstorage.LatestMessagesLogIsCorrupted
import coop.rchain.blockstorage.dag.codecs._
import coop.rchain.blockstorage.util.io.IOError.RaiseIOError
import coop.rchain.models.Validator.Validator
import coop.rchain.shared.Log
import scodec.codecs._

object SlashedMessagePersistentIndex {
  type SlashedMessagePersistentIndex[F[_]] = PersistentIndex[F, Validator, Unit]

  def load[F[_]: Sync: Log: RaiseIOError](
      logPath: Path,
      crcPath: Path
  ): F[SlashedMessagePersistentIndex[F]] =
    PersistentIndex.load[F, Validator, Unit](
      logPath,
      crcPath,
      LatestMessagesLogIsCorrupted
    )(Sync[F], Log[F], RaiseIOError[F], codecValidator, ignore(0))
}
