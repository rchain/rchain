package coop.rchain.blockstorage
import coop.rchain.blockstorage.BlockDagFileStorage.Checkpoint

object errors {
  sealed abstract class BlockDagStorageError(message: String) extends Throwable(message) {
    def this(message: String, cause: Throwable) {
      this(message)
      initCause(cause)
    }
  }

  final case class CheckpointsDoNotStartFromZero(sortedCheckpoints: List[Checkpoint])
      extends BlockDagStorageError(
        s"Checkpoints do not start from block number 0: ${sortedCheckpoints.map(_.path.getFileName).mkString(",")}"
      )

  final case class CheckpointsAreNotConsecutive(sortedCheckpoints: List[Checkpoint])
      extends BlockDagStorageError(
        s"Checkpoints are not consecutive: ${sortedCheckpoints.map(_.path.getFileName).mkString(",")}"
      )

  final case class TopoSortLengthIsTooBig(length: Long)
      extends BlockDagStorageError(
        s"Topological sorting of length $length was requested while maximal length is ${Int.MaxValue}"
      )
}
