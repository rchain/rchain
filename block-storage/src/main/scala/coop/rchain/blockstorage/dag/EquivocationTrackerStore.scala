package coop.rchain.blockstorage.dag

import cats.Monad
import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.EquivocationRecord
import coop.rchain.models.EquivocationRecord.SequenceNumber
import coop.rchain.models.Validator.Validator
import coop.rchain.shared.Log
import coop.rchain.shared.syntax._
import coop.rchain.store.KeyValueTypedStore

object EquivocationTrackerStore {
  def apply[F[_]: Sync: Log](
      store: KeyValueTypedStore[F, (Validator, SequenceNumber), Set[BlockHash]]
  ): F[EquivocationTrackerStore[F]] =
    Sync[F].delay(new EquivocationTrackerStore(store))

  class EquivocationTrackerStore[F[_]: Sync](
      private val store: KeyValueTypedStore[F, (Validator, SequenceNumber), Set[BlockHash]]
  ) {
    def add(record: EquivocationRecord): F[Unit] =
      store.put(
        (record.equivocator, record.equivocationBaseBlockSeqNum),
        record.equivocationDetectedBlockHashes
      )

    def addAll(records: List[EquivocationRecord]): F[Unit] =
      store.put(
        records.map {
          case EquivocationRecord(equivocator, seqNum, blockHashes) =>
            (equivocator, seqNum) -> blockHashes
        }
      )

    def data: F[Set[EquivocationRecord]] =
      store.toMap.map(_.map {
        case ((equivocator, seqNum), blockHashes) =>
          EquivocationRecord(equivocator, seqNum, blockHashes)
      }.toSet)
  }
}
