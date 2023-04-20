package coop.rchain.node.instances

import cats.effect.Async
import cats.syntax.all._
import coop.rchain.casper.PrettyPrinter
import coop.rchain.casper.blocks.proposer._
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.state.instances.ProposerState
import coop.rchain.shared.Log
import fs2.Stream
import fs2.concurrent.Channel
import cats.effect.{Deferred, Ref}
import cats.effect.std.{PQueue, Semaphore}

object ProposerInstance {
  def create[F[_]: Async: Log](
      proposeRequestsQueue: Channel[F, (Boolean, Deferred[F, ProposerResult])],
      proposer: Proposer[F],
      state: Ref[F, ProposerState[F]]
  ): Stream[F, (ProposeResult, Option[BlockMessage])] = {
    // stream of requests to propose
    val in = proposeRequestsQueue.stream

    // max number of concurrent attempts to propose. Actual propose can happen only one at a time, but clients
    // are free to make propose attempt. In that case proposeID returned will be None.
    val maxConcurrentAttempts = 100

    val out = Stream
    // propose permit
      .eval(for {
        lock    <- Semaphore[F](1)
        trigger <- PQueue.bounded[F, Int](1)
        // initial position for propose trigger - inactive
        _ <- trigger.offer(1)
      } yield (lock, trigger))
      .flatMap {
        case (lock, trigger) =>
          in.map { i =>
              val (isAsync, proposeIDDef) = i

              Stream
                .eval(lock.tryAcquire)
                // if propose is in progress - resolve proposeID to ProposerEmpty result and stop here.
                // Cock the trigger, so propose is called again after the one that occupies the lock finishes.
                .evalFilter { v =>
                  (trigger.tryTake >> proposeIDDef.complete(ProposerResult.empty))
                    .unlessA(v)
                    .as(v)
                }
                // execute propose
                .evalMap { _ =>
                  for {
                    _ <- Log[F].info("Propose started")
                    // deferred for new propose result, update state
                    rDef <- Deferred[F, (ProposeResult, Option[BlockMessage])]
                    _ <- state
                          .update { s =>
                            s.copy(currProposeResult = rDef.some)
                          }
                    r <- proposer.propose(isAsync, proposeIDDef)
                    // complete deferred with propose result, update state
                    _ <- rDef.complete(r)
                    _ <- state
                          .update { s =>
                            s.copy(latestProposeResult = r.some, currProposeResult = None)
                          }
                    _ <- lock.release

                    (result, blockHashOpt) = r
                    infoMsg = blockHashOpt.fold {
                      s"Propose failed: ${result.proposeStatus}"
                    } { block =>
                      val blockInfo = PrettyPrinter.buildString(block, short = true)
                      s"Propose finished: ${result.proposeStatus} Block $blockInfo created and added."
                    }
                    _ <- Log[F].info(infoMsg)

                    // propose on more time if trigger is cocked
                    _ <- trigger
                          .tryOffer(1)
                          .ifM(
                            Deferred[F, ProposerResult] >>= { d =>
                              proposeRequestsQueue.send(false -> d).void
                            },
                            ().pure[F]
                          )
                  } yield r
                }
            }
            .parJoin(maxConcurrentAttempts)
      }

    out
  }
}
