package coop.rchain.casper.blocks

import cats.effect.{Concurrent, Timer}
import cats.syntax.all._
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.casper._
import coop.rchain.casper.protocol.{BlockMessage, CommUtil}
import coop.rchain.casper.rholang.RuntimeManager
import coop.rchain.casper.syntax._
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.shared.Log
import coop.rchain.shared.syntax._
import fs2.Stream
import fs2.concurrent.Queue

object BlockProcessor {

  /**
    * Logic for processing incoming blocks
    * - input block must have all dependencies in the DAG
    * - blocks created by node itself are not processed here, but in Proposer
    */
  def apply[F[_]: Concurrent: Timer: RuntimeManager: BlockDagStorage: BlockStore: CommUtil: Log: Metrics: Span](
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

  def validateAndAddToDag[F[_]: Concurrent: Timer: RuntimeManager: BlockDagStorage: BlockStore: CommUtil: Log: Metrics: Span](
      block: BlockMessage,
      shardId: String,
      minPhloPrice: Long
  ): F[ValidBlockProcessing] =
    for {
      result <- MultiParentCasper.validate(block, shardId, minPhloPrice)

      blockMeta = result.leftMap(_._1).merge
      _         <- BlockDagStorage[F].insert(blockMeta, block)

      // TODO: refactor/remove all this nonsense with Either/BlockError/ValidBlock statuses!
      // - result trimmed to satisfy existing code
    } yield result.as(BlockStatus.valid).leftMap(_._2)

}
