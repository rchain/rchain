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
import coop.rchain.shared.syntax.sharedSyntaxFs2Stream
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
      // Casper state to validate block against
      getCasperSnapshot: F[CasperSnapshot]
  ): Stream[F, (BlockMessage, ValidBlockProcessing)] =
    inputBlocks.parEvalMapUnorderedProcBounded { block =>
      for {
        result <- validateAndAddToDag(block, getCasperSnapshot)

        // Log block validation result
        blockStr = PrettyPrinter.buildString(block, short = true)
        _        <- Log[F].info(s"Block $blockStr validated: $result")

        // Notify finished block validation
        _ <- validatedQueue.enqueue1(block)

        // Broadcast block to the peers
        _ <- CommUtil[F].sendBlockHash(block.blockHash, block.sender)
      } yield (block, result)
    }

  def validateAndAddToDag[F[_]: Concurrent: Timer: Time: RuntimeManager: BlockDagStorage: BlockStore: CommUtil: Log: Metrics: Span](
      block: BlockMessage,
      getCasperSnapshot: F[CasperSnapshot]
  ): F[ValidBlockProcessing] =
    for {
      snapshot <- getCasperSnapshot
      result   <- MultiParentCasper.validate(block, snapshot)
      dag      <- BlockDagStorage[F].getRepresentation
      // TODO: legacy code returns updated DAG
      updatedDag <- result
                     .as(MultiParentCasper.handleValidBlock(block))
                     .leftMap {
                       case i: InvalidBlock => MultiParentCasper.handleInvalidBlock(block, i, dag)
                       // TODO: legacy code, raise error in this case
                       case _ => dag.pure[F] // this should never happen
                     }
                     .merge
    } yield result

}
