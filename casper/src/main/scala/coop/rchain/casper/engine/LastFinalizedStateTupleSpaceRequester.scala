package coop.rchain.casper.engine

import cats.effect.{Concurrent, Sync}
import cats.effect.syntax.all._
import cats.syntax.all._
import coop.rchain.casper.protocol._
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.comm.rp.Connect.RPConfAsk
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.models.{BindPattern, ListParWithRandom, Par, TaggedContinuation}
import coop.rchain.rholang.interpreter.storage
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.state.{RSpaceImporter, RSpaceStateManager}
import coop.rchain.rspace.util.Lib
import coop.rchain.shared.ByteVectorOps._
import coop.rchain.shared.{Log, Time}
import fs2.concurrent.{Queue, SignallingRef}
import fs2.{Pipe, Stream}
import scodec.bits.ByteVector

import scala.concurrent.duration._

object LastFinalizedStateTupleSpaceRequester {
  // Possible request statuses
  trait ReqStatus
  case object Init      extends ReqStatus
  case object Requested extends ReqStatus
  case object Received  extends ReqStatus
  case object Done      extends ReqStatus

  type StatePartPath = Seq[(Blake2b256Hash, Option[Byte])]

  // TODO: extract this as a parameter
  // 20,000 records uses about 2G of direct buffer memory on importing side
  val pageSize = 20000

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
    * Create a stream to receive tuple space needed for Last Finalized State.
    *
    * @param approvedBlock          Last finalized block
    * @param tupleSpaceMessageQueue Handler of tuple space messages
    * @return fs2.Stream processing all tuple space state
    */
  def stream[F[_]: Concurrent: Time: RSpaceStateManager: CommUtil: Log: RPConfAsk: TransportLayer](
      approvedBlock: ApprovedBlock,
      tupleSpaceMessageQueue: Queue[F, StoreItemsMessage]
  ): F[Stream[F, Boolean]] = {

    def createStream(
        st: SignallingRef[F, ST[StatePartPath]],
        requestQueue: Queue[F, Boolean]
    ): Stream[F, Boolean] = {

      // Get importer
      val importer = RSpaceStateManager[F].importer

      def broadcastStreams(ids: Seq[StatePartPath]) = {
        // Create broadcast requests to peers
        val broadcastRequests = ids.map(
          x =>
            Stream.eval(
              Log[F].info(s"Sending StoreItemsRequest to bootstrap") *>
                TransportLayer[F].sendToBootstrap(StoreItemsMessageRequest(x, 0, pageSize).toProto)
            )
        )
        // Create stream if requests
        Stream(broadcastRequests: _*)
      }

      /**
        * Request stream is pulling state chunk paths ready for broadcast requests.
        */
      val requestStream = for {
        // Request queue is a trigger when to check the state
        resend <- requestQueue.dequeue

        // Check if stream is finished (no more requests)
        isEnd <- Stream.eval(st.get.map(_.isFinished))

        // Take next set of items to request
        ids <- Stream.eval(st.modify(_.getNext(resend)))

        // Send all requests in parallel
        _ <- broadcastStreams(ids).parJoinUnbounded.whenA(!isEnd && ids.nonEmpty)
      } yield isEnd

      /**
        * Response stream is handling incoming chunks of state.
        */
      val responseStream = for {
        // Response queue is incoming message source / async callback handler
        msg <- tupleSpaceMessageQueue.dequeue

        StoreItemsMessage(startPath, lastPath, historyItems, dataItems) = msg

        // Mark chunk as received
        isReceived <- Stream.eval(st.modify(_.received(startPath)))

        // Add chunk paths for requesting and trigger request queue (without resend of already requested)
        _ <- Stream
              .eval(st.update(_.add(Set(lastPath))) >> requestQueue.enqueue1(false))
              .whenA(isReceived)

        // Import chunk to RSpace
        _ <- Stream
              .eval {
                // Transform values from ByteString to ByteVector
                val historyItemsWithByteVector = historyItems.map {
                  case (k, v) => (k, ByteVector(v.toByteArray))
                }
                val dataItemsWithByteVector = dataItems.map {
                  case (k, v) => (k, ByteVector(v.toByteArray))
                }

                for {
                  // Validate received tuple space items. Run it in parallel with writing to store.
                  validateJob <- Lib
                                  .time("VALIDATE STATE CHUNK")(
                                    validateTupleSpaceItems(
                                      historyItemsWithByteVector,
                                      dataItemsWithByteVector,
                                      startPath,
                                      chunkSize = pageSize,
                                      skip = 0,
                                      importer.getHistoryItem
                                    )
                                  )
                                  .start

                  // Write incoming data
                  historyJob <- Lib
                                 .time("IMPORT HISTORY")(
                                   importer
                                     .setHistoryItems[ByteVector](
                                       historyItemsWithByteVector,
                                       _.toDirectByteBuffer
                                     )
                                 )
                                 .start
                  _ <- Lib.time("IMPORT DATA")(
                        importer
                          .setDataItems[ByteVector](
                            dataItemsWithByteVector,
                            _.toDirectByteBuffer
                          )
                      )

                  // Wait history write
                  _ <- historyJob.join

                  // Wait for validation to finish, throw if failed.
                  _ <- validateJob.join

                  // Mark chunk as finished
                  _ <- st.update(_.done(startPath))

                  // Trigger request queue again to process finished chunks
                  _ <- requestQueue.enqueue1(false)
                } yield ()
              }
              .whenA(isReceived)
      } yield ()

      /**
        * Timeout to resend block requests if response is not received
        */
      val timeoutDuration = 2.minutes
      val timeoutMsg      = s"No tuple space state responses for $timeoutDuration. Resending requests."
      val resendStream = Stream.eval(
        // Trigger request queue (resend already requested)
        Time[F].sleep(timeoutDuration) *> requestQueue.enqueue1(true) <* Log[F].warn(timeoutMsg)
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

    val stateHash = Blake2b256Hash.fromByteString(
      ProtoUtil.postStateHash(approvedBlock.candidate.block)
    )
    val startRequest: StatePartPath = Seq((stateHash, None))
    val importer                    = RSpaceStateManager[F].importer
    for {
      // Write last finalized state root
      _ <- importer.setRoot(stateHash)

      // Requester state
      st <- SignallingRef[F, ST[StatePartPath]](ST(Seq(startRequest)))

      // Tuple space state requests queue
      requestQueue <- Queue.unbounded[F, Boolean]

      // Light the fire! / Starts the first request for chunk of state
      // - `true` if requested chunks should be re-requested
      _ <- requestQueue.enqueue1(false)

      // Create tuple space state receiver stream
    } yield createStream(st, requestQueue)
  }

  implicit val codecPar  = storage.serializePar.toCodec
  implicit val codecBind = storage.serializeBindPattern.toCodec
  implicit val codecPars = storage.serializePars.toCodec
  implicit val codecCont = storage.serializeTaggedContinuation.toCodec

  def validateTupleSpaceItems[F[_]: Sync](
      historyItems: Seq[(Blake2b256Hash, ByteVector)],
      dataItems: Seq[(Blake2b256Hash, ByteVector)],
      startPath: Seq[(Blake2b256Hash, Option[Byte])],
      chunkSize: Int,
      skip: Int,
      getFromHistory: Blake2b256Hash => F[Option[ByteVector]]
  ) =
    RSpaceImporter.validateStateItems[F, Par, BindPattern, ListParWithRandom, TaggedContinuation](
      historyItems,
      dataItems,
      startPath,
      chunkSize,
      skip,
      getFromHistory
    )
}
