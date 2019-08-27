package coop.rchain.blockstorage.dag

import java.nio.file.Path

import cats.Monad
import cats.implicits._
import cats.effect.Sync
import coop.rchain.blockstorage.EquivocationsTrackerLogIsMalformed
import coop.rchain.blockstorage.dag.codecs._
import coop.rchain.blockstorage.util.io.IOError.RaiseIOError
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.EquivocationRecord
import coop.rchain.models.EquivocationRecord.SequenceNumber
import coop.rchain.models.Validator.Validator
import coop.rchain.shared.Log
import scodec.codecs._

object EquivocationTrackerPersistentIndex {
  class PersistentEquivocationTrackerIndex[F[_]: Monad](
      private val persistentIndex: PersistentIndex[F, (Validator, SequenceNumber), Set[BlockHash]]
  ) {
    def add(record: EquivocationRecord): F[Unit] =
      persistentIndex.add(
        (record.equivocator, record.equivocationBaseBlockSeqNum),
        record.equivocationDetectedBlockHashes
      )

    def addAll(records: List[EquivocationRecord]): F[Unit] =
      persistentIndex.addAll(
        records.map {
          case EquivocationRecord(equivocator, seqNum, blockHashes) =>
            (equivocator, seqNum) -> blockHashes
        }
      )

    def data: F[Set[EquivocationRecord]] =
      persistentIndex.data.map(_.map {
        case ((equivocator, seqNum), blockHashes) =>
          EquivocationRecord(equivocator, seqNum, blockHashes)
      }.toSet)

    def close: F[Unit] =
      persistentIndex.close
  }

  def load[F[_]: Sync: Log: RaiseIOError](
      logPath: Path,
      crcPath: Path
  ): F[PersistentEquivocationTrackerIndex[F]] =
    PersistentIndex
      .load[F, (Validator, SequenceNumber), Set[BlockHash]](
        logPath,
        crcPath,
        EquivocationsTrackerLogIsMalformed
      )(Sync[F], Log[F], RaiseIOError[F], codecValidator ~ codecSeqNum, codecBlockHashSet)
      .map(index => new PersistentEquivocationTrackerIndex(index))
}
