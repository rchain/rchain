package coop.rchain.blockstorage

import coop.rchain.casper.protocol.BlockMessage

import java.nio.file.Path

sealed abstract class StorageError extends Exception

final case class CheckpointsDoNotStartFromZero(sortedCheckpoints: List[Path]) extends StorageError
final case class CheckpointsAreNotConsecutive(sortedCheckpoints: List[Path])  extends StorageError
final case class TopoSortFragmentParameterError(startBlockNumber: Long, endBlockNumber: Long)
    extends StorageError
final case class BlockSenderIsMalformed(block: BlockMessage) extends StorageError

object StorageError {
  type StorageErr[A] = Either[StorageError, A]
}
