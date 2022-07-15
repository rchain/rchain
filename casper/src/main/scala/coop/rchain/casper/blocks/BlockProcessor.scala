package coop.rchain.casper.blocks

import cats.effect.{Concurrent, Timer}
import cats.syntax.all._
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.casper._
import coop.rchain.casper.protocol.{BlockMessage, CommUtil}
import coop.rchain.casper.rholang.RuntimeManager
import coop.rchain.casper.syntax.casperSyntaxCommUtil
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.shared.syntax._
import coop.rchain.shared.{Log, Time}
import fs2.Stream
import fs2.concurrent.Queue

object BlockProcessor {

  /**
    * Logic for processing incoming blocks
    * - input block must have all dependencies in the DAG
    * - blocks created by node itself are not held here, but in Proposer
    */
  def apply[F[_]: Concurrent: Timer: Time: RuntimeManager: BlockDagStorage: BlockStore: CommUtil: Log: Metrics: Span](
      inputBlocks: Stream[F, BlockMessage],
      validatedQueue: Queue[F, BlockMessage],
      shardId: String,
      minPhloPrice: Long
  ): Stream[F, (BlockMessage, ValidBlockProcessing)] =
    inputBlocks.parEvalMapUnorderedProcBounded { block =>
      for {
        // Validate block and add it to the DAG
        result <- validateAndAddToDag(block, shardId, minPhloPrice)

        // Notify finished block validation
        _ <- validatedQueue.enqueue1(block)

        // Broadcast block to the peers
        _ <- CommUtil[F].sendBlockHash(block.blockHash, block.sender)
      } yield (block, result)
    }

  def validateAndAddToDag[F[_]: Concurrent: Timer: Time: RuntimeManager: BlockDagStorage: BlockStore: CommUtil: Log: Metrics: Span](
      block: BlockMessage,
      shardId: String,
      minPhloPrice: Long
  ): F[ValidBlockProcessing] =
    for {
      result <- MultiParentCasper.validate(block, shardId, minPhloPrice)
      dag    <- BlockDagStorage[F].getRepresentation
      // TODO: legacy code returns updated DAG
      updatedDag <- result
                     .map { blockMeta =>
                       BlockDagStorage[F].insert(blockMeta, block)
                     }
                     .leftMap {
                       // TODO: refactor/remove all this nonsense with Either/BlockError/ValidBlock statuses!
                       case (blockMeta, _: InvalidBlock) =>
                         // TODO: error should already in BlockMetadata
                         BlockDagStorage[F].insert(blockMeta, block)
                       // TODO: legacy code, raise error in this case
                       case _ => dag.pure[F] // this should never happen
                     }
                     .merge
      // TODO: TEMP result trimmed to satisfy existing code
    } yield result.as(BlockStatus.valid).leftMap(_._2)

}
