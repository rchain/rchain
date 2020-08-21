package coop.rchain.casper.engine

import cats.effect.Concurrent
import cats.effect.concurrent.Ref
import cats.syntax.all._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.casper.ValidBlock.Valid
import coop.rchain.casper.{PrettyPrinter, Validate}
import coop.rchain.casper.protocol.{ApprovedBlock, BlockMessage}
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.shared.{Log, Time}
import fs2.Stream
import fs2.concurrent.{Queue, SignallingRef}

import scala.concurrent.duration._

object LastFinalizedStateBlockRequester {
  // Possible request statuses
  trait ReqStatus
  case object Init      extends ReqStatus
  case object Requested extends ReqStatus
  case object Received  extends ReqStatus
  case object Done      extends ReqStatus

  /**
    * State to control processing of requests
    */
  final case class ST[Key](private val d: Map[Key, ReqStatus]) {
    // Adds new keys to Init state, ready for processing. Existing keys are skipped.
    def add(keys: Set[Key]): ST[Key] = ST(d ++ keys.filterNot(d.contains).map((_, Init)))

    // Get next keys not already requested or
    //  in case of resend together with Requested.
    // Returns updated state with requested keys.
    def getNext(resend: Boolean): (ST[Key], Seq[Key]) = {
      val requested = d
        .filter { case (_, v) => v == Init || (resend && v == Requested) }
        .mapValues(_ => Requested)
      ST(d ++ requested) -> requested.keysIterator.toSeq
    }

    // Confirm key is Received if it was Requested.
    // Returns updated state with the flag if it was Requested.
    def received(k: Key): (ST[Key], Boolean) = {
      val isRequested = d.get(k).contains(Requested)
      val newSt       = if (isRequested) d + ((k, Received)) else d
      ST(newSt) -> isRequested
    }

    // Mark key as finished (Done).
    def done(k: Key): ST[Key] = ST(d + ((k, Done)))

    // Returns flag if all keys are marked as finished (Done).
    def isFinished: Boolean = !d.exists { case (_, v) => v != Done }
  }

  object ST {
    // Create requests state with initial keys.
    def apply[Key](initial: Seq[Key]): ST[Key] = ST[Key](d = initial.map((_, Init)).toMap)
  }

  /**
    * Create a stream to receive blocks needed for Last Finalized State.
    *
    * @param approvedBlock Last finalized block
    * @param responseQueue Handler of block messages
    * @return fs2.Stream processing all blocks
    */
  def stream[F[_]: Concurrent: Time: BlockDagStorage: BlockStore: CommUtil: Log](
      approvedBlock: ApprovedBlock,
      responseQueue: Queue[F, BlockMessage]
  ): F[Stream[F, Boolean]] = {

    def createStream(
        d: SignallingRef[F, ST[BlockHash]],
        requestQueue: Queue[F, Boolean],
        validatorsRequired: Ref[F, Set[Validator]]
    ): Stream[F, Boolean] = {

      def broadcastStreams(ids: Seq[BlockHash]) = {
        // Create broadcast requests to peers
        val broadcastRequests = ids.map(x => Stream.eval(CommUtil[F].broadcastRequestForBlock(x)))
        // Create stream if requests
        Stream(broadcastRequests: _*)
      }

      /**
        * Request stream is pulling new block hashes ready for broadcast requests.
        */
      val requestStream = for {
        // Request queue is a trigger when to check the state
        resend <- requestQueue.dequeue

        // Check if stream is finished (no more requests)
        isEnd <- Stream.eval(d.get.map(_.isFinished))

        // Take next set of items to request
        ids <- Stream.eval(d.modify(_.getNext(resend)))

        // Send all requests in parallel
        _ <- broadcastStreams(ids).parJoinUnbounded.whenA(!isEnd && ids.nonEmpty)
      } yield isEnd

      /**
        * Response stream is handling incoming block messages.
        */
      val responseStream = for {
        // Response queue is incoming message source / async callback handler
        block <- responseQueue.dequeue

        // Mark block as received
        isReceived <- Stream.eval(d.modify(_.received(block.blockHash)))

        allValidatorsSeen <- Stream.eval(validatorsRequired.modify[Boolean] { currSet =>
                              val newSet = currSet - block.sender
                              (newSet, newSet.isEmpty)
                            })

        inDeployLifeSpanRage = ProtoUtil.blockNumber(block) > ProtoUtil.blockNumber(
          approvedBlock.candidate.block
          // TODO extract this number from shard config
        ) - 50
        // Add block parents for requesting
        _ <- Stream
              .eval {
                for {
                  deps <- getBlockDependencies(block)
                  _    <- d.update(_.add(deps.toSet))
                } yield ()
              }
              .whenA(isReceived && (inDeployLifeSpanRage || !allValidatorsSeen))

        /**
          * Validate block hash. Checking justifications from last finalized block gives us proof
          *  for all ancestor blocks.
          */
        isBlockHashValid_ <- Stream.eval(Validate.blockHash(block).map(_ == Right(Valid)))
        // TODO: validate zero genesis correctly
        isBlockHashValid = block.body.state.blockNumber == 0 || isBlockHashValid_
        warnInvalidBlock = Log[F].warn(
          s"Received ${PrettyPrinter.buildString(block)} with invalid hash. Ignored block."
        )
        // Log invalid block
        _ <- Stream.eval(warnInvalidBlock).whenA(isReceived && !isBlockHashValid)

        // Save block to DAG store
        _ <- Stream
              .eval(
                for {
                  // Save block to the store
                  _ <- BlockStore[F].put(block.blockHash, block)
                  // Mark block as finished
                  _ <- d.update(_.done(block.blockHash))
                } yield ()
              )
              .whenA(isReceived && isBlockHashValid)

        // Trigger request queue (without resend of already requested)
        _ <- Stream.eval(requestQueue.enqueue1(false))
      } yield ()

      /**
        * Timeout to resend block requests if response is not received
        */
      val timeoutDuration = 30.seconds
      val timeoutMsg      = s"No block responses for $timeoutDuration. Requests resent."
      val resendStream = Stream.eval(
        // Trigger request queue (resend already requested)
        Time[F].sleep(timeoutDuration) *> requestQueue.enqueue1(true) <* Log[F].warn(timeoutMsg)
      )
      // Switch to resend if response is not triggered after timeout
      // - response message reset timeout by canceling previous stream
      val timeoutResendStream = responseStream.switchMap(_ => resendStream.repeat)

      /**
        * Final result! Concurrently pulling requests and handling responses
        *  with resend timeout if response is not received.
        */
      requestStream.takeWhile(!_) concurrently responseStream concurrently timeoutResendStream
    }

    def getBlockDependencies(block: BlockMessage) = {
      import cats.instances.list._
      ProtoUtil.dependenciesHashesOf(block).filterA(BlockStore[F].contains(_).not)
    }

    for {

      // Start block hashes
      dependenciesHashes <- getBlockDependencies(approvedBlock.candidate.block)
      // Requester state
      st <- SignallingRef[F, ST[BlockHash]](ST(dependenciesHashes))
      // Block requests queue
      requestQueue <- Queue.unbounded[F, Boolean]

      // active validators as per approved block state
      // for approved state to be complete it is required to have block from each of them
      abValidators = approvedBlock.candidate.block.body.state.bonds
        .filter(_.stake > 0)
        .map(_.validator)
      validatorsRequired <- Ref.of[F, Set[Validator]](abValidators.toSet)

      // Light the fire! / Starts the first request for block
      // - `true` if requested blocks should be re-requested
      _ <- requestQueue.enqueue1(false)
      // Create block receiver stream
    } yield createStream(st, requestQueue, validatorsRequired)
  }

}
