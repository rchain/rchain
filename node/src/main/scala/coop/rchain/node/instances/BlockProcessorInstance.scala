package coop.rchain.node.instances

import cats.effect.Concurrent
import cats.effect.concurrent.{Deferred, Ref}
import cats.instances.list._
import cats.syntax.all._
import coop.rchain.casper.blocks.BlockProcessor
import coop.rchain.casper.blocks.proposer.{BugError, ProposeResult}
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.{
  BlockError,
  BlockStatus,
  Casper,
  PrettyPrinter,
  ValidBlock,
  ValidBlockProcessing
}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.shared.Log
import fs2.concurrent.Queue
import fs2.Stream

object BlockProcessorInstance {
  def create[F[_]: Concurrent: Log](
      blocksQueue: Queue[F, (Casper[F], BlockMessage)],
      blockProcessor: BlockProcessor[F],
      state: Ref[F, Set[BlockHash]],
      proposerQueue: Queue[F, (Casper[F], Deferred[F, Option[Int]])],
      autopropose: Boolean
  ): Stream[F, (Casper[F], BlockMessage, ValidBlockProcessing)] = {

    // Node can handle `parallelism` blocks in parallel, or they will be queued
    val parallelism = 100

    val in = blocksQueue.dequeue

    val out = in
      .map { i =>
        {
          val (c, b)                             = i
          val blockStr                           = PrettyPrinter.buildString(b, short = true)
          val logNotOfInterest                   = Log[F].info(s"Block $blockStr is not of interest. Dropped")
          val logMalformed                       = Log[F].info(s"Block $blockStr is not of malformed. Dropped")
          val logMissingDeps                     = Log[F].info(s"Block $blockStr missing dependencies.")
          val logStarted                         = Log[F].info(s"Block $blockStr processing started.")
          def logResult(r: ValidBlockProcessing) = Log[F].info(s"Block $blockStr validated: ${r}")
          val logFinished                        = Log[F].info(s"Block $blockStr processing finished.")

          Stream
            .eval(state.modify(s => (s + b.blockHash, s.contains(b.blockHash))))
            // stop here if hash is in processing already
            .filterNot(
              inProcessing => inProcessing
            )
            .evalFilter(
              _ =>
                blockProcessor.checkIfOfInterest(c, b) >>= { r =>
                  logNotOfInterest.unlessA(r).as(r)
                }
            )
            .evalFilter(
              _ =>
                blockProcessor.checkIfWellFormedAndStore(b) >>= { r =>
                  logMalformed.unlessA(r).as(r)
                }
            )
            .evalTap { _ =>
              logStarted
            }
            .evalFilter(
              _ =>
                blockProcessor.checkDependenciesWithEffects(c, b) >>= { r =>
                  logMissingDeps.unlessA(r).as(r)
                }
            )
            .evalMap(
              _ =>
                blockProcessor.validateWithEffects(c, b, None) >>= { r =>
                  logResult(r).as(c, b, r)
                }
            )
            .evalTap { _ =>
              for {
                bufferPendants <- c.getDependencyFreeFromBuffer
                inProcess      <- state.get
                _ <- bufferPendants
                      .filterNot(b => inProcess.contains(b.blockHash))
                      .traverse(b => blocksQueue.enqueue1(c, b))
                _ <- logFinished
              } yield ()
            }
            .evalTap { v =>
              // we don't care about result of propose here, whether its started or not.
              // so let deferred be not used
              (Deferred[F, Option[Int]] >>= { d =>
                proposerQueue.enqueue1((v._1, d))
              }).whenA(autopropose)
            }
            // ensure to remove hash from state
            .onFinalize(state.update(s => s - b.blockHash))
        }
      }
      .parJoin(parallelism)

    out
  }
}
