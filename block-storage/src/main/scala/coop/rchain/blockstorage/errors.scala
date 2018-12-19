package coop.rchain.blockstorage
import java.nio.file.Path

import coop.rchain.blockstorage.BlockDagFileStorage.Checkpoint
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.crypto.codec.Base16

object errors {
  sealed abstract class BlockDagStorageError(message: String) extends Throwable(message) {
    def this(message: String, cause: Throwable) {
      this(message)
      initCause(cause)
    }
  }

  final case class CheckpointsDoNotStartFromZero(sortedCheckpoints: List[Path])
      extends BlockDagStorageError(
        s"Checkpoints do not start from block number 0: ${sortedCheckpoints.mkString(",")}"
      )

  final case class CheckpointsAreNotConsecutive(sortedCheckpoints: List[Path])
      extends BlockDagStorageError(
        s"Checkpoints are not consecutive: ${sortedCheckpoints.mkString(",")}"
      )

  final case class TopoSortLengthIsTooBig(length: Long)
      extends BlockDagStorageError(
        s"Topological sorting of length $length was requested while maximal length is ${Int.MaxValue}"
      )

  final case class BlockSenderIsMalformed(block: BlockMessage)
      extends BlockDagStorageError(
        s"Block ${Base16.encode(block.blockHash.toByteArray)} sender is malformed: ${Base16
          .encode(block.sender.toByteArray)}"
      )
}
