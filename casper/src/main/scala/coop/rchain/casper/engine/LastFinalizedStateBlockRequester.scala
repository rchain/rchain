package coop.rchain.casper.engine

import cats.effect.Concurrent
import cats.syntax.all._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.casper.ValidBlock.Valid
import coop.rchain.casper.protocol.{ApprovedBlock, BlockMessage}
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.casper.{MultiParentCasperImpl, PrettyPrinter, Validate}
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.shared.{Log, Time}
import fs2.concurrent.{Queue, SignallingRef}
import fs2.{Pipe, Stream}

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
  final case class ST[Key](private val d: Map[Key, ReqStatus], latest: Set[Key], lowerBound: Long) {
    // Adds new keys to Init state, ready for processing. Existing keys are skipped.
    def add(keys: Set[Key]): ST[Key] =
      this.copy(d ++ keys.filterNot(d.contains).map((_, Init)))

    // Get next keys not already requested or
    //  in case of resend together with Requested.
    // Returns updated state with requested keys.
    def getNext(resend: Boolean): (ST[Key], Seq[Key]) = {
      val requested = d
        .filter { case (_, v) => v == Init || (resend && v == Requested) }
        .mapValues(_ => Requested)
      this.copy(d ++ requested) -> requested.keysIterator.toSeq
    }

    // Confirm key is Received if it was Requested.
    // Returns updated state with the flag if it was Requested.
    def received(k: Key): (ST[Key], Boolean) = {
      val isRequested = d.get(k).contains(Requested)
      val newSt       = if (isRequested) d + ((k, Received)) else d
      this.copy(newSt) -> isRequested
    }

    // Mark key as finished (Done) with optionally set minimum lower bound.
    def done(k: Key, height: Option[Long]): (ST[Key], Long) = {
      val newLowest = height.fold(lowerBound)(Math.min(_, lowerBound))
      val newSt     = d + ((k, Done))
      this.copy(newSt, lowerBound = newLowest) -> newLowest
    }

    // Track when senders list will be empty
    def admitLatest(msg: Key): (ST[Key], (Boolean, Boolean)) = {
      // Remove message from the set of latest messages (if exists)
      val newLatest = latest - msg
      // Is message found in the set of latest messages
      val isFound = latest != newLatest
      // Is set of latest messages empty
      val isEmpty = newLatest.isEmpty
      // Returns new state and observed changes to the state
      (this.copy(latest = newLatest), (isFound, isEmpty))
    }

    // Returns flag if all keys are marked as finished (Done).
    def isFinished: Boolean = latest.isEmpty && !d.exists { case (_, v) => v != Done }
  }

  object ST {
    // Create requests state with initial keys.
    def apply[Key](initial: Seq[Key], latest: Set[Key] = Set[Key]()): ST[Key] =
      ST[Key](d = initial.map((_, Init)).toMap, latest, lowerBound = Long.MaxValue)
  }

  /**
    * Create a stream to receive blocks needed for Last Finalized State.
    *
    * @param approvedBlock Last finalized block
    * @param responseQueue Handler of block messages
    * @param requestTimeout Time after request will be resent if not received
    * @param requestForBlock Send request for block
    * @param containsBlock Check if block is in the store
    * @param putBlockToStore Add block to the store
    * @param validateBlock Check if received block is valid
    * @return fs2.Stream processing all blocks
    */
  def stream[F[_]: Concurrent: Time: Log](
      approvedBlock: ApprovedBlock,
      responseQueue: Queue[F, BlockMessage],
      requestTimeout: FiniteDuration,
      requestForBlock: BlockHash => F[Unit],
      containsBlock: BlockHash => F[Boolean],
      putBlockToStore: (BlockHash, BlockMessage) => F[Unit],
      validateBlock: BlockMessage => F[Boolean]
  ): F[Stream[F, Boolean]] = {

    val block                           = approvedBlock.candidate.block
    val approvedBlockNumber             = ProtoUtil.blockNumber(block)
    val minBlockNumberForDeployLifespan = approvedBlockNumber - MultiParentCasperImpl.deployLifespan

    // Active validators as per approved block state
    // - for approved state to be complete it is required to have block from each of them
    val latestMessages = block.justifications.map(_.latestBlockHash).toSet

    def createStream(
        st: SignallingRef[F, ST[BlockHash]],
        requestQueue: Queue[F, Boolean]
    ): Stream[F, Boolean] = {

      def broadcastStreams(ids: Seq[BlockHash]) = {
        // Create broadcast requests to peers
        val broadcastRequests = ids.map(x => Stream.eval(requestForBlock(x)))
        // Create stream of requests
        Stream(broadcastRequests: _*)
      }

      /**
        * Validate and save block. Checking justifications from last finalized block gives us proof
        *  for all ancestor blocks.
        */
      def validateAndSaveBlock(block: BlockMessage) =
        for {
          blockHashIsValid_ <- validateBlock(block)
          // TODO: validate zero genesis correctly
          blockHashIsValid = block.body.state.blockNumber == 0 || blockHashIsValid_

          // Log invalid block
          invalidBlockMsg = s"Received ${PrettyPrinter.buildString(block)} with invalid hash. Ignored block."
          _               <- Log[F].warn(invalidBlockMsg).whenA(!blockHashIsValid)

          _ <- saveBlock(block).whenA(blockHashIsValid)
        } yield ()

      def saveBlock(block: BlockMessage) =
        for {
          // Block is latest message from bonded validator
          // - we need all child blocks of this block
          // - it must be used to calculate minimum block height
          admitResult                     <- st.modify(_.admitLatest(block.blockHash))
          (useAsMinHeight, isLatestEmpty) = admitResult

          // Mark block as finished
          blockNumber           = ProtoUtil.blockNumber(block)
          blockNumberOpt        = if (useAsMinHeight) (blockNumber - 1).some else none
          minBlockNumberForDeps <- st.modify(_.done(block.blockHash, blockNumberOpt))

          // Minimum block number to request
          minBlockNumber = Math.min(minBlockNumberForDeps, minBlockNumberForDeployLifespan)

          // Is block number accepted
          // - true if all latest are not received
          // - or all latest received and minimum height is the from oldest latest block
          isBlockNumberAccepted = !isLatestEmpty || (isLatestEmpty && blockNumber >= minBlockNumber)

          // Save block to the store
          alreadySaved <- containsBlock(block.blockHash)
          _            <- putBlockToStore(block.blockHash, block).whenA(isBlockNumberAccepted && !alreadySaved)

          _ <- Log[F]
                .info(s"New minimum block height re-calculated $minBlockNumberForDeps.")
                .whenA(useAsMinHeight)

          // Update dependencies for requesting
          requestDependencies = getBlockDependencies(block) >>= (
              deps => st.update(_.add(deps.toSet))
          )
          _ <- requestDependencies.whenA(isBlockNumberAccepted)
        } yield ()

      import cats.instances.list._

      /**
        * Request stream is pulling new block hashes ready for broadcast requests.
        */
      val requestStream = for {
        // Request queue is a trigger when to check the state
        resend <- requestQueue.dequeue

        // Check if stream is finished (no more requests)
        isEnd <- Stream.eval(st.get.map(_.isFinished))

        // Take next set of items to request (w/o duplicates)
        hashes <- Stream.eval(st.modify(_.getNext(resend)))

        // Check existing blocks
        existingHashes <- Stream.eval(hashes.toList.filterA(containsBlock))

        // Missing blocks not already in the block store
        missingBlocks = hashes.diff(existingHashes)

        _ <- Stream.eval(st.update(_.add(missingBlocks.toSet)))

        // Send all requests in parallel
        _ <- broadcastStreams(missingBlocks).parJoinUnbounded
              .whenA(!isEnd && missingBlocks.nonEmpty)
      } yield isEnd

      /**
        * Response stream is handling incoming block messages.
        */
      val responseStream = for {
        // Response queue is incoming message source / async callback handler
        block <- responseQueue.dequeue

        // Mark block as received
        isReceived <- Stream.eval(st.modify(_.received(block.blockHash)))

        // Save block to DAG store
        _ <- Stream.eval(validateAndSaveBlock(block)).whenA(isReceived)

        // Trigger request queue (without resend of already requested)
        _ <- Stream.eval(requestQueue.enqueue1(false))
      } yield ()

      /**
        * Timeout to resend block requests if response is not received.
        */
      val timeoutMsg = s"No block responses for $requestTimeout. Resending requests."
      val resendStream = Stream.eval(
        // Trigger request queue (resend already requested)
        Time[F].sleep(requestTimeout) *> requestQueue.enqueue1(true) <* Log[F].warn(timeoutMsg)
      )
      // Switch to resend if response is not triggered after timeout
      // - response message reset timeout by canceling previous stream
      def withTimeout: Pipe[F, Boolean, Boolean] =
        in => in concurrently resendStream.interruptWhen(in.map(_ => true)).repeat

      /**
        * Final result! Concurrently pulling requests and handling responses
        *  with resend timeout if response is not received.
        */
      requestStream.takeWhile(!_).broadcastThrough(withTimeout) concurrently responseStream
    }

    def getBlockDependencies(block: BlockMessage) = {
      import cats.instances.list._
      ProtoUtil.dependenciesHashesOf(block).filterA(containsBlock(_).not)
    }

    for {
      // Requester state, fill with validators for required latest messages
      st <- SignallingRef[F, ST[BlockHash]](
             ST(Seq(block.blockHash), latest = latestMessages)
           )

      // Block requests queue
      requestQueue <- Queue.unbounded[F, Boolean]

      // Light the fire! / Starts the first request for block
      // - `true` if requested blocks should be re-requested
      _ <- requestQueue.enqueue1(false)

      // Create block receiver stream
    } yield createStream(st, requestQueue)
  }

}
