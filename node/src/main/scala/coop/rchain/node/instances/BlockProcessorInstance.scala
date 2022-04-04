package coop.rchain.node.instances

import cats.effect.Concurrent
import cats.effect.concurrent.Ref
import cats.syntax.all._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper.blocks.BlockProcessor
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.{PrettyPrinter, ProposeFunction, ValidBlockProcessing}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.shared.Log
import fs2.Stream
import fs2.concurrent.Queue

object BlockProcessorInstance {
  def create[F[_]: Concurrent: BlockStore: Log](
      inputProcessingBlocks: Stream[F, BlockHash],
      finishedProcessing: Queue[F, BlockHash],
      blockProcessor: BlockProcessor[F],
      state: Ref[F, Set[BlockHash]],
      triggerProposeF: Option[ProposeFunction[F]],
      autoPropose: Boolean
  ): Stream[F, (BlockMessage, ValidBlockProcessing)] = {

    // Node can handle `parallelism` blocks in parallel, or they will be queued
    val parallelism = 100

    val out = inputProcessingBlocks
      .evalMap(BlockStore[F].getUnsafe)
      .map { b =>
        {
          val blockStr                           = PrettyPrinter.buildString(b, short = true)
          val logStarted                         = Log[F].info(s"Block $blockStr processing started.")
          def logResult(r: ValidBlockProcessing) = Log[F].info(s"Block $blockStr validated: $r")
          val logFinished                        = Log[F].info(s"Block $blockStr processing finished.")

          Stream
            .eval(state.modify(s => (s + b.blockHash, s.contains(b.blockHash))))
            // stop here if hash is in processing already
            .filterNot(
              inProcessing => inProcessing
            )
            .evalTap { _ =>
              logStarted
            }
            .evalMap(
              _ =>
                blockProcessor.validateWithEffects(b) >>= { r =>
                  logResult(r).as(b, r)
                }
            )
            .evalTap { _ =>
              for {
                bufferPendants <- blockProcessor.getDependencyFreeFromBuffer
                inProcess      <- state.get
                _ <- bufferPendants
                      .filterNot(b => inProcess.contains(b.blockHash))
                      .traverse(b => finishedProcessing.enqueue1(b.blockHash))
                _ <- logFinished
              } yield ()
            }
            .evalTap { _ =>
              triggerProposeF.traverse(_(true)) whenA autoPropose
            }
            // ensure to remove hash from state
            .onFinalize(state.update(s => s - b.blockHash))
        }
      }
      .parJoin(parallelism)
    out
  }
}
