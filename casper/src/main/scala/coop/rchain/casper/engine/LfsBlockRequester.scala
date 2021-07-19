package coop.rchain.casper.engine

import cats.effect.concurrent.Ref
import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import coop.rchain.casper.PrettyPrinter
import coop.rchain.casper.protocol.{ApprovedBlock, BlockMessage}
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.shared.syntax._
import coop.rchain.shared.{Log, Time}
import fs2.{Pure, Stream}
import fs2.concurrent.Queue

import scala.collection.immutable.SortedMap
import scala.concurrent.duration._

/**
  * Last Finalized State processor for receiving blocks.
  */
object LfsBlockRequester {
  // Possible request statuses
  trait ReqStatus
  case object Init      extends ReqStatus
  case object Requested extends ReqStatus
  case object Received  extends ReqStatus

  final case class ReceiveInfo(requested: Boolean, latest: Boolean, lastlatest: Boolean)

  /**
    * State to control processing of requests
    */
  final case class ST[Key](
      d: Map[Key, ReqStatus],
      latest: Set[Key],
      lowerBound: Long,
      heightMap: SortedMap[Long, Set[Key]],
      finished: Set[Key]
  ) {
    // Adds new keys to Init state, ready for processing. Existing keys are skipped.
    def add(keys: Set[Key]): ST[Key] = {
      // Filter keys that are in Done status
      //  or in request Map.
      val newKeys = keys.diff(finished).filterNot(d.contains)
      // Set Init status for new keys.
      this.copy(d ++ newKeys.map((_, Init)))
    }

    // Get next keys not already requested or
    //  in case of resend together with Requested.
    // Returns updated state with requested keys.
    def getNext(resend: Boolean): (ST[Key], Seq[Key]) = {
      val requested = d
        .filter {
          case (key, status) =>
            // Select initialized or re-request if resending
            def checkForRequest = status == Init || (resend && status == Requested)
            if (latest.isEmpty) {
              // Latest are downloaded, no additional conditions
              checkForRequest
            } else {
              // Only latest are requested first
              checkForRequest && latest(key)
            }
        }
        .mapValues(_ => Requested)
      this.copy(d ++ requested) -> requested.keysIterator.toSeq
    }

    // Confirm key is Received if it was Requested.
    // Returns updated state with the flags if Requested and last latest received.
    def received(k: Key, height: Long): (ST[Key], ReceiveInfo) = {
      val isReq = d.get(k).contains(Requested)
      if (isReq) {
        // Remove message from the set of latest messages (if exists)
        val newLatest    = latest - k
        val isLatest     = latest != newLatest
        val isLastLatest = isLatest && newLatest.isEmpty
        // Save in height map
        val heightKeys   = heightMap.getOrElse(height, Set())
        val newHeightMap = heightMap + ((height, heightKeys + k))
        // Calculate new minimum height if latest message
        //  - we need parents of latest message so it's `-1`
        val newLowerBound = if (isLatest) Math.min(height - 1, lowerBound) else lowerBound
        val newSt         = d + ((k, Received))
        // Set new minimum height and update latest
        (
          this.copy(newSt, newLatest, newLowerBound, newHeightMap),
          ReceiveInfo(isReq, isLatest, isLastLatest)
        )
      } else {
        (this, ReceiveInfo(requested = false, latest = false, lastlatest = false))
      }
    }

    // Mark key as finished (Done) with optionally set minimum lower bound.
    def done(k: Key): ST[Key] = {
      val isReceived = d.get(k).contains(Received)
      if (isReceived) {
        // If Received key, remove from request Map and add to Done Set
        val newSt   = d - k
        val newDone = finished + k
        this.copy(newSt, finished = newDone)
      } else this
    }

    // Returns flag if all keys are marked as finished (Done).
    def isFinished: Boolean = latest.isEmpty && d.isEmpty
  }

  object ST {
    // Create requests state with initial keys.
    def apply[Key](
        initial: Set[Key],
        latest: Set[Key] = Set[Key](),
        lowerBound: Long = 0
    ): ST[Key] =
      ST[Key](
        d = initial.map((_, Init)).toMap,
        latest,
        lowerBound,
        heightMap = SortedMap[Long, Set[Key]](),
        finished = Set[Key]()
      )
  }

  /**
    * Create a stream to receive blocks needed for Last Finalized State.
    *
    * @param approvedBlock Last finalized block
    * @param responseQueue Handler of block messages
    * @param initialMinimumHeight Required minimum block height before latest messages are downloaded
    * @param requestForBlock Send request for block
    * @param requestTimeout Time after request will be resent if not received
    * @param containsBlock Check if block is in the store
    * @param putBlockToStore Add block to the store
    * @param validateBlock Check if received block is valid
    * @return fs2.Stream processing all blocks
    */
  def stream[F[_]: Concurrent: Time: Log](
      approvedBlock: ApprovedBlock,
      responseQueue: Queue[F, BlockMessage],
      initialMinimumHeight: Long,
      requestForBlock: BlockHash => F[Unit],
      requestTimeout: FiniteDuration,
      containsBlock: BlockHash => F[Boolean],
      getBlockFromStore: BlockHash => F[BlockMessage],
      putBlockToStore: (BlockHash, BlockMessage) => F[Unit],
      validateBlock: BlockMessage => F[Boolean]
  ): F[Stream[F, ST[BlockHash]]] = {

    val block = approvedBlock.candidate.block

    // Active validators as per approved block state
    // - for approved state to be complete it is required to have block from each of them
    val latestMessages = block.justifications.map(_.latestBlockHash).toSet

    val initialHashes = latestMessages + block.blockHash

    def createStream(
        st: Ref[F, ST[BlockHash]],
        requestQueue: Queue[F, Boolean],
        responseHashQueue: Queue[F, BlockHash]
    ): Stream[F, ST[BlockHash]] = {

      def broadcastStreams(ids: Seq[BlockHash]): Stream[Pure, Stream[F, Unit]] = {
        // Create broadcast requests to peers
        val broadcastRequests = ids.map(requestForBlock andThen Stream.eval)
        // Create stream of requests
        Stream.emits(broadcastRequests)
      }

      def processBlock(block: BlockMessage): F[Unit] =
        for {
          // Validate and mark received block
          isValid <- validateReceivedBlock(block)

          // Save block to store
          _ <- saveBlock(block).whenA(isValid)

          // Trigger request queue (without resend of already requested)
          _ <- requestQueue.enqueue1(false)
        } yield ()

      /**
        * Validate received block, check if it was requested and if block hash is correct.
        *  Following justifications from last finalized block gives us proof for all ancestor blocks.
        */
      def validateReceivedBlock(block: BlockMessage) = {
        def invalidBlockMsg =
          s"Received ${PrettyPrinter.buildString(block)} with invalid hash. Ignored block."
        val blockNumber = ProtoUtil.blockNumber(block)
        for {
          // Mark block as received and calculate minimum height (if latest)
          receivedResult <- st.modify(_.received(block.blockHash, blockNumber))
          // Result if block is received and if last latest is received
          ReceiveInfo(isReceived, isReceivedLatest, isLastLatest) = receivedResult

          blockHashIsValid <- if (isReceived) validateBlock(block) else false.pure[F]

          // Log invalid block if block is requested but hash is invalid
          _ <- Log[F].warn(invalidBlockMsg).whenA(isReceived && !blockHashIsValid)

          // Try accept received block if it has valid hash
          isReceived <- if (blockHashIsValid) {
                         for {
                           // Log minimum height when last latest block is received
                           minimumHeight <- st.get.map(_.lowerBound)
                           _ <- Log[F]
                                 .info(
                                   s"Latest blocks downloaded. Minimum block height is $minimumHeight."
                                 )
                                 .whenA(isLastLatest)

                           // Update dependencies for requesting
                           requestDependencies = Sync[F].delay(
                             ProtoUtil.dependenciesHashesOf(block)
                           ) >>= (deps => st.update(_.add(deps.toSet)))

                           // Accept block if it's requested and satisfy conditions
                           // - received one of latest messages
                           // - requested and block number is greater than minimum
                           blockIsAccepted = isReceivedLatest || isReceived && blockNumber >= minimumHeight
                           _               <- requestDependencies.whenA(blockIsAccepted)
                         } yield isReceived
                       } else false.pure[F]
        } yield isReceived
      }

      def saveBlock(block: BlockMessage) =
        for {
          // Save block to the store
          alreadySaved <- containsBlock(block.blockHash)
          _            <- putBlockToStore(block.blockHash, block).whenA(!alreadySaved)

          // Mark block download as done
          _ <- st.update(_.done(block.blockHash))
        } yield ()

      import cats.instances.list._

      /**
        * Request stream is pulling new block hashes ready for broadcast requests.
        */
      val requestStream = for {
        // Request queue is a trigger when to check the state
        resend <- requestQueue.dequeueChunk(maxSize = 1)

        // Check if stream is finished (no more requests)
        isEnd <- Stream.eval(st.get.map(_.isFinished))

        // Take next set of items to request (w/o duplicates)
        hashes <- Stream.eval(st.modify(_.getNext(resend)))

        // Check existing blocks
        existingHashes <- Stream.eval(hashes.toList.filterA(containsBlock))

        // Enqueue hashes of exiting blocks
        _ <- responseHashQueue.enqueue(Stream.emits(existingHashes)).whenA(existingHashes.nonEmpty)

        // Missing blocks not already in the block store
        missingBlocks = hashes.diff(existingHashes)

        // Send all requests in parallel for missing blocks (using `last` to drain the stream)
        _ <- broadcastStreams(missingBlocks).parJoinUnbounded.last
              .whenA(!isEnd && missingBlocks.nonEmpty)
      } yield resend

      /**
        * Response stream is handling incoming block messages. Responses can be processed in parallel.
        */
      val responseStream1 = responseQueue.dequeue.parEvalMapProcBounded(processBlock)

      val responseStream2 = responseHashQueue.dequeue
        .parEvalMapProcBounded { hash =>
          for {
            block <- getBlockFromStore(hash)
            _     <- Log[F].info(s"Process existing ${PrettyPrinter.buildString(block)}")
            _     <- processBlock(block)
          } yield ()
        }

      // Merge both response streams
      val responseStream = Stream(responseStream1, responseStream2).parJoinUnbounded

      /**
        * Timeout to resend block requests if response is not received.
        */
      val timeoutMsg = s"No block responses for $requestTimeout. Resending requests."
      // Triggers request queue (resend already requested)
      val resendRequests = requestQueue.enqueue1(true) <* Log[F].warn(timeoutMsg)

      /**
        * Final result! Concurrently pulling requests and handling responses
        *  with resend timeout if response is not received.
        */
      requestStream
        .evalMap(_ => st.get)
        .onIdle(requestTimeout, resendRequests)
        .terminateAfter(_.isFinished) concurrently responseStream
    }

    for {
      // Requester state, fill with validators for required latest messages
      st <- Ref.of[F, ST[BlockHash]](
             ST(
               initialHashes,
               latest = initialHashes,
               lowerBound = initialMinimumHeight
             )
           )

      // Queue to trigger processing of requests. `True` to resend requests.
      requestQueue <- Queue.bounded[F, Boolean](maxSize = 2)
      // Response queue for existing blocks in the store.
      responseHashQueue <- Queue.unbounded[F, BlockHash]

      // Light the fire! / Starts the first request for block
      // - `true` if requested blocks should be re-requested
      _ <- requestQueue.enqueue1(false)

      // Create block receiver stream
    } yield createStream(st, requestQueue, responseHashQueue)
  }

}
