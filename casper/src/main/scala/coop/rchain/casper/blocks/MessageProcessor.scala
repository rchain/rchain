package coop.rchain.casper.blocks

import cats.effect.concurrent.{Ref, Semaphore}
import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import coop.rchain.blockstorage.state.CasperST
import coop.rchain.casper.BlockStatus._
import coop.rchain.casper._
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.models.BlockHash._
import coop.rchain.shared.Log
import fs2.Stream
import fs2.concurrent.Queue

import scala.collection.immutable.Set

object MessageProcessor {

  /** Casper message processor inputs blocks and outputs the Casper states. */
  def create[F[_]: Concurrent: Log](
      initCasperState: CasperST[BlockHash],
      input: Stream[F, BlockMessage],
      // functions required
      checkFormatF: BlockMessage => F[Either[IgnoreReason, BlockMessage]],
      storeF: BlockMessage => F[Unit],
      validateF: (BlockHash, CasperST[BlockHash]) => F[Either[BlockStatus, Validated[F]]],
      // effects invoked
      effRequestDependencies: Set[BlockHash] => F[Unit],
      effValidated: (BlockHash, Option[Offence]) => F[Unit]
  ): Stream[F, CasperST[BlockHash]] = {

    def logIgnored(r: IgnoreReason): F[Unit] = Log[F].debug(s"Incoming block ignored ($r)")

    // Resources required to create stream
    val mkReqs = for {
      // Ref storing latest Casper state
      casperStRef <- Ref.of[F, CasperST[BlockHash]](initCasperState)
      // Queue of messages to be processed.
      // Note: this queue is used and not the input stream, as successful validation of a message can trigger
      // processing of another message, which depends on the first one.
      validationQueue <- Queue.unbounded[F, BlockHash]
      // it would be great to have method for updating pure state in Ref, but now we don't have it
      stateUpdateLock <- Semaphore(1)
    } yield (validationQueue, casperStRef, stateUpdateLock)

    Stream.eval(mkReqs).flatMap {
      case (vQueue, casperStRef, stateUpdateLock) =>
        def guardIgnorable(m: BlockMessage): F[Either[IgnoreReason, BlockMessage]] = {
          val checkNotOfInterestF =
            casperStRef.get
              .map(s => (s.known(m.blockHash), s.beforeFinalized(m.body.state.blockNumber)))
              .map {
                case (true, _)      => Left(Known)
                case (false, true)  => Left(Old)
                case (false, false) => Right(m)
              }
          checkNotOfInterestF.flatMap(_ => checkFormatF(m))
        }

        // Stream routing messages received from network to processing queue
        val pullIncoming = input
          .evalMap { guardIgnorable }
          .evalTap { case Left(reason) => logIgnored(reason) }
          .collect { case Right(m) => m }
          .evalTap { storeF }
          // record new message to Casper state
          .evalMap { m =>
            casperStRef.modify { st =>
              val dependencies = m.justifications.map(_.latestBlockHash)
              val (newSt, isReady, _, toRequest) =
                st.add(m.blockHash, dependencies.toSet)
              (newSt, (m, isReady, toRequest))
            }
          }
          // request missing dependencies if any
          .evalTap { case (_, _, toReq) if toReq.nonEmpty => effRequestDependencies(toReq) }
          // send to validation if dependency free
          .collect { case (m, true, _) => m.blockHash }
          .through { vQueue.enqueue }

        // This state update makes message transition from buffer state to Casper state, so both states
        // should be updated atomically
        val casperStUpdateWithEffects = (m: BlockHash, offence: Option[Offence]) =>
          Sync[F].bracket(stateUpdateLock.acquire) { _ =>
            for {
              _         <- effValidated(m, offence)
              unblocked <- casperStRef.modify { _.validated(m) }
              newSt     <- casperStRef.get
            } yield (newSt, unblocked)
          }(_ => stateUpdateLock.release)

        // Processing stream
        val validate = vQueue
          .dequeueChunk(maxSize = 1)
          // validate message using latest casper state
          .evalMap { case m => casperStRef.get.flatMap(s => validateF(m, s)) }
          .evalTap { case Left(errStatus) => println(s"Unexpected error $errStatus").pure }
          // collect only properly validated messages
          .collect { case Right(Validated(m, offenceOpt)) => (m, offenceOpt) }
          // update casper state and processor state inside single lock
          .evalMap { casperStUpdateWithEffects.tupled }
          .evalTap { case (_, ready) => ready.toList.traverse(vQueue.enqueue1) }
          .map { case (newSt, _) => newSt }

        validate concurrently pullIncoming
    }
  }
}
