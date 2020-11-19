package coop.rchain.node.instances

import cats.effect.Concurrent
import cats.effect.concurrent.{Deferred, MVar, Ref, Semaphore}
import coop.rchain.casper.Casper
import coop.rchain.casper.blocks.proposer.{BugError, ProposeResult, Proposer}
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.state.instances.ProposerState
import coop.rchain.shared.Log
import cats.syntax.all._
import fs2.Stream
import fs2.concurrent.Queue

object ProposerInstance {
  def create[F[_]: Concurrent: Log](
      proposeRequestsQueue: Queue[F, (Casper[F], Deferred[F, Option[Int]])],
      proposer: Proposer[F],
      state: Ref[F, ProposerState[F]]
  ): Stream[F, (ProposeResult, Option[BlockMessage])] = {
    // stream of requests to propose
    val in = proposeRequestsQueue.dequeue

    // max number of concurrent attempts to propose. Actual propose can happen only one at a time, but clients
    // are free to make propose attempt. In that case proposeID returned will be None.
    val maxConcurrentAttempts = 100

    val out = Stream
    // propose permit
      .eval(for {
        lock    <- Semaphore[F](1)
        trigger <- MVar[F].of()
      } yield (lock, trigger))
      .flatMap {
        case (lock, trigger) =>
          in.map { i =>
              val (c, proposeIDDef) = i

              Stream
                .eval(lock.tryAcquire)
                // if propose is in progress - resolve proposeID to None and stop here.
                // Cock the trigger, so propose is called again after the one that occupies the lock finishes.
                .evalFilter { v =>
                  (proposeIDDef.complete(None) >> trigger.tryPut()).unlessA(v).as(v)
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
                    r <- proposer.propose(c, proposeIDDef)
                    // complete deferred with propose result, update state
                    _ <- rDef.complete(r)
                    _ <- state
                          .update { s =>
                            s.copy(latestProposeResult = r.some, currProposeResult = None)
                          }
                    _ <- lock.release
                    _ <- Log[F].info(s"Propose finished: ${r._1.proposeStatus}")
                    // propose on more time if trigger is cocked
                    _ <- trigger.tryRead.map {
                          case Some(_) =>
                            Deferred[F, Option[Int]] >>= { d =>
                              proposeRequestsQueue.enqueue1(c, d)
                            }
                          case None => ().pure[F]
                        }
                  } yield r
                }
            }
            .parJoin(maxConcurrentAttempts)
      }

    out
  }
}
