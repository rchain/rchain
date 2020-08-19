package coop.rchain.casper.engine

import cats.effect.Concurrent
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.{
  ApprovedBlock,
  StoreItemsMessage,
  StoreItemsMessageRequest,
  ToPacket
}
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.comm.rp.Connect.RPConfAsk
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.state.RSpaceStateManager
import coop.rchain.shared.ByteVectorOps._
import coop.rchain.shared.{Log, Time}
import fs2.Stream
import fs2.concurrent.{Queue, SignallingRef}
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

  // TODO: extract this as a parameter (3000 on local machine is showing best results)
  val pageSize = 3000

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
        d: SignallingRef[F, ST[StatePartPath]],
        requestQueue: Queue[F, Boolean]
    ): Stream[F, Boolean] = {

      def broadcastStreams(ids: Seq[StatePartPath]) = {
        // Create broadcast requests to peers
        val broadcastRequests = ids.map(
          x =>
            Stream.eval(
              TransportLayer[F].sendToBootstrap(
                ToPacket(StoreItemsMessageRequest(x, 0, pageSize).toProto)
              )
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
        isEnd <- Stream.eval(d.get.map(_.isFinished))

        // Take next set of items to request
        ids <- Stream.eval(d.modify(_.getNext(resend)))

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
        isReceived <- Stream.eval(d.modify(_.received(startPath)))

        // Add chunk paths for requesting
        _ <- Stream.eval(d.update(_.add(Set(lastPath)))).whenA(isReceived)

//        _ <- Stream.eval(Time[F].sleep(3.seconds))

        // Import chunk to RSpace
        _ <- Stream
              .eval(
                for {
                  _ <- Log[F].info(
                        s"Received store items - history: ${historyItems.size}, data: ${dataItems.size}, START: $startPath: LAST: $lastPath"
                      )

                  // Get importer
                  importer = RSpaceStateManager[F].importer

                  // Write incoming data
                  _ <- importer
                        .setHistoryItems[ByteString](
                          historyItems,
                          x => ByteVector(x.toByteArray).toDirectByteBuffer
                        )
                  _ <- importer
                        .setDataItems[ByteString](
                          dataItems,
                          x => ByteVector(x.toByteArray).toDirectByteBuffer
                        )

                  // Mark chunk as finished
                  _ <- d.update(_.done(startPath))
                } yield ()
              )
              .whenA(isReceived)

        // Trigger request queue (without resend of already requested)
        _ <- Stream.eval(requestQueue.enqueue1(false))
      } yield ()

      /**
        * Timeout to resend requests for tuple state chunks if response is not received.
        */
      val timeoutDuration = 30.seconds
      val timeoutMsg      = s"No tuple space state responses for $timeoutDuration. Requests resent."
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
}
