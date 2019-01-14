package coop.rchain.blockstorage
import java.nio.file.Path

import cats.data.EitherT
import coop.rchain.casper.protocol.BlockMessage

sealed abstract class StorageError extends Exception

final case class CheckpointsDoNotStartFromZero(sortedCheckpoints: List[Path]) extends StorageError
final case class CheckpointsAreNotConsecutive(sortedCheckpoints: List[Path]) extends StorageError
final case class TopoSortLengthIsTooBig(length: Long) extends StorageError
final case class BlockSenderIsMalformed(block: BlockMessage) extends StorageError

object StorageError {
  type StorageErr[A]        = Either[StorageError, A]
  type StorageErrT[F[_], A] = EitherT[F, StorageError, A]
}
