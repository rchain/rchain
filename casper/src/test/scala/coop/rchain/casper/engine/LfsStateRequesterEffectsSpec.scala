package coop.rchain.casper.engine

import cats.effect.Concurrent
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.engine.LfsTupleSpaceRequester.{ST, StatePartPath}
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.scalatest.Fs2StreamMatchers
import coop.rchain.models.blockImplicits
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.state.{RSpaceImporter, StateValidationError}
import coop.rchain.shared.{Log, Time}
import fs2.Stream
import fs2.concurrent.Queue
import monix.eval.Task
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scodec.bits.ByteVector

import java.nio.ByteBuffer
import scala.concurrent.duration.{DurationInt, FiniteDuration}

class LfsStateRequesterEffectsSpec extends AnyFlatSpec with Matchers with Fs2StreamMatchers {

  def createApprovedBlock(block: BlockMessage): ApprovedBlock = ApprovedBlock(block)

  // Create hash from hex string (padding to 32 bytes)
  def createHash(s: String) = Blake2b256Hash.fromHex(s.padTo(64, '0'))

  def mkRequest(path: StatePartPath) = (path, LfsTupleSpaceRequester.pageSize)

  // Approved block state (start of the state)
  val historyHash1 = createHash("1a")
  val historyPath1 = List((historyHash1, None))
  val history1     = Seq((createHash("1a1"), ByteString.EMPTY), (createHash("1a2"), ByteString.EMPTY))
  val data1        = Seq((createHash("1b1"), ByteString.EMPTY))
  // Chunk 2
  val historyHash2 = createHash("2a")
  val historyPath2 = List((historyHash2, None))
  val history2     = Seq((createHash("2a1"), ByteString.EMPTY))
  val data2        = Seq((createHash("2b1"), ByteString.EMPTY), (createHash("2b2"), ByteString.EMPTY))
  // Chunk 3
  val historyHash3 = createHash("3a")
  val historyPath3 = List((historyHash2, None))
  // Fake invalid history items
  val invalidHistory = Seq((createHash("666aaaaa"), ByteString.EMPTY))

  type SavedStoreItems = Seq[(Blake2b256Hash, ByteString)]

  def exceptionInvalidState = StateValidationError("Fake invalid state received.")

  trait Mock[F[_]] {
    // Fill response queue - simulate receiving StoreItems from external source
    def receive(item: StoreItemsMessage*): F[Unit]

    // Observed requests
    val sentRequests: Stream[F, (StatePartPath, Int)]

    // Observed saved state items
    val savedHistory: Stream[F, SavedStoreItems]
    val savedData: Stream[F, SavedStoreItems]

    // Processing stream
    val stream: Stream[F, ST[StatePartPath]]
  }

  /**
    * Creates test setup
    *
    * @param test test definition
    */
  def createMock[F[_]: Concurrent: Time: Log](requestTimeout: FiniteDuration)(
      test: Mock[F] => F[Unit]
  ): F[Unit] = {

    def mockValidateStateChunk(
        historyItems: Seq[(Blake2b256Hash, ByteVector)],
        dataItems: Seq[(Blake2b256Hash, ByteVector)],
        startPath: Seq[(Blake2b256Hash, Option[Byte])],
        chunkSize: Int,
        skip: Int,
        getFromHistory: Blake2b256Hash => F[Option[ByteVector]]
    ): F[Unit] = {
      val invalidItems = invalidHistory.map(_.map(bv => ByteVector(bv.toByteArray)))

      // Simulates invalid state if matches predefined constant
      if (historyItems != invalidItems) ().pure[F]
      else exceptionInvalidState.raiseError
    }

    // Approved block has initial root hash of the state
    val approvedBlock = createApprovedBlock(
      blockImplicits.getRandomBlock(setPostStateHash = historyHash1.toByteString.some)
    )

    for {
      // Queue for received store messages
      responseQueue <- Queue.unbounded[F, StoreItemsMessage]

      // Queue for requested state chunks
      requestQueue <- Queue.unbounded[F, (StatePartPath, Int)]

      // Queues for saved chunks
      savedHistoryQueue <- Queue.unbounded[F, SavedStoreItems]
      savedDataQueue    <- Queue.unbounded[F, SavedStoreItems]

      importer = new RSpaceImporter[F] {
        override type KeyHash = Blake2b256Hash

        override def setHistoryItems[Value](
            data: Seq[(KeyHash, Value)],
            toBuffer: Value => ByteBuffer
        ): F[Unit] = {
          val items = data.map(_.map(toBuffer andThen ByteString.copyFrom))
          savedHistoryQueue.enqueue1(items)
        }

        override def setDataItems[Value](
            data: Seq[(KeyHash, Value)],
            toBuffer: Value => ByteBuffer
        ): F[Unit] = {
          val items = data.map(_.map(toBuffer andThen ByteString.copyFrom))
          savedDataQueue.enqueue1(items)
        }

        override def setRoot(key: KeyHash): F[Unit] = ().pure[F]

        // Not used by LFS requester
        override def getHistoryItem(hash: Blake2b256Hash): F[Option[ByteVector]] = ???
      }

      // Queue for processing the internal state (ST)
      processingStream <- LfsTupleSpaceRequester.stream(
                           approvedBlock,
                           responseQueue,
                           requestQueue.enqueue1(_, _),
                           requestTimeout,
                           importer,
                           mockValidateStateChunk
                         )

      mock = new Mock[F] {
        override def receive(msgs: StoreItemsMessage*): F[Unit] =
          responseQueue.enqueue(Stream.emits(msgs)).compile.drain

        override val sentRequests: Stream[F, (StatePartPath, Int)] =
          Stream.eval(requestQueue.dequeue1).repeat
        override val savedHistory: Stream[F, SavedStoreItems] =
          Stream.eval(savedHistoryQueue.dequeue1).repeat
        override val savedData: Stream[F, SavedStoreItems] =
          Stream.eval(savedDataQueue.dequeue1).repeat

        override val stream: Stream[F, ST[StatePartPath]] = processingStream
      }

      // Execute test function together with processing stream
      _ <- test(mock)
    } yield ()
  }

  implicit val logEff: Log[Task]   = Log.log[Task]
  implicit val timeEff: Time[Task] = TestTime.instance

  import monix.execution.Scheduler.Implicits.global

  /**
    * Test runner
    *
    *  - Default request timeout is set to large value to disable re-request messages if CI is slow.
    *
    * @param runProcessingStream flag to automatically run processing stream
    * @param requestTimeout request resend timeout
    * @param test test specification
    */
  def createBootstrapTest(runProcessingStream: Boolean, requestTimeout: FiniteDuration = 10.days)(
      test: Mock[Task] => Task[Unit]
  ): Unit =
    createMock[Task](requestTimeout) { mock =>
      if (!runProcessingStream) test(mock)
      else (Stream.eval(test(mock)) concurrently mock.stream).compile.drain
    }.runSyncUnsafe(timeout = 10.seconds)

  val bootstrapTest = createBootstrapTest(runProcessingStream = true) _

  it should "send request for next state chunk" in bootstrapTest { mock =>
    import mock._
    for {
      // Process initial request
      reqs <- sentRequests.take(1).compile.toList

      // After start, first request should be for approved block post state
      _ = reqs shouldBe List(mkRequest(historyPath1))

      // Receives store items message
      _ <- receive(StoreItemsMessage(historyPath1, historyPath2, history1, data1))

      reqs <- sentRequests.take(1).compile.toList

      // After first chunk received, next chunk should be requested (end of previous chunk)
      _ = reqs shouldBe List(mkRequest(historyPath2))
    } yield ()
  }

  it should "save (import) received state chunk" in bootstrapTest { mock =>
    import mock._
    for {
      // Process initial request
      _ <- sentRequests.take(1).compile.drain

      // Receives store items message
      _ <- receive(StoreItemsMessage(historyPath1, historyPath2, history1, data1))

      // One history chunk should be saved
      history <- savedHistory.take(1).compile.toList

      // Saved history responses
      _ = history shouldBe List(history1)

      // One data chunk should be saved
      data <- savedData.take(1).compile.toList

      // Saved data responses
      _ = data shouldBe List(data1)
    } yield ()
  }

  it should "not request next chunk if message is not requested" in bootstrapTest { mock =>
    import mock._
    for {
      // Process initial request
      reqs <- sentRequests.take(1).compile.toList

      // After start, first request should be for approved block post state
      _ = reqs shouldBe List(mkRequest(historyPath1))

      // Receives store items message which is not requested
      _ <- receive(StoreItemsMessage(historyPath2, historyPath3, history2, data2))

      // Request should not be sent for the next chunk
      _ = sentRequests should notEmit
    } yield ()
  }

  it should "not save (import) state chunk if not requested" in bootstrapTest { mock =>
    import mock._
    for {
      // Process initial request
      _ <- sentRequests.take(1).compile.drain

      // Receives store items message which is not requested
      _ <- receive(StoreItemsMessage(historyPath2, historyPath3, history2, data2))

      // No history items should be saved
      _ = savedHistory should notEmit

      // No data items should be saved
      _ = savedData should notEmit
    } yield ()
  }

  it should "stop if invalid state chunk received" in createBootstrapTest(
    runProcessingStream = false
  ) { mock =>
    import mock._
    for {
      // Process initial request
      _ <- stream.take(1).compile.drain
      _ <- sentRequests.take(1).compile.drain

      // No other requests should be sent
      _ = sentRequests should notEmit

      // Receives store items message
      _ <- receive(StoreItemsMessage(historyPath1, historyPath2, invalidHistory, data1))

      result <- stream.compile.lastOrError.attempt

      // Stream should fail with an error
      _ = result shouldBe Left(exceptionInvalidState)
    } yield ()
  }

  it should "finish after last chunk received" in createBootstrapTest(runProcessingStream = false) {
    mock =>
      import mock._
      for {
        // Process initial request
        _ <- stream.take(1).compile.drain
        _ <- sentRequests.take(1).compile.drain

        // No other requests should be sent
        _ = sentRequests should notEmit

        // Receives store items message (start path is equal to end path)
        _ <- receive(StoreItemsMessage(historyPath1, historyPath1, history1, data1))

        state <- stream.compile.lastOrError

        // Stream should fail with an error
        _ = state.isFinished shouldBe true
      } yield ()
  }

  /**
    * Test for request timeout. This is timing test which in CI can be a problem if execution is paused.
    *
    * NOTE: We don't have any abstraction to test time in execution (with monix Task or cats IO).
    *  We have LogicalTime and DiscreteTime which are just wrappers to get different "milliseconds" but are totally
    *  disconnected from Task/IO execution notion of time (e.g. Task.sleep).
    *  Other testing instances of Time are the same as in normal node execution (using Task.timer).
    *  https://github.com/rchain/rchain/issues/3001
    */
  it should "re-send request after timeout" in createBootstrapTest(
    runProcessingStream = false,
    requestTimeout = 200.millis
  ) { mock =>
    import mock._
    for {
      // Wait for timeout to expire
      _ <- stream.compile.drain.timeout(300.millis).onErrorHandle(_ => ())

      // Wait for two requests
      reqs <- sentRequests.take(2).compile.toList

      // Both requests should be the same, second is resent
      _ = reqs shouldBe List(mkRequest(historyPath1), mkRequest(historyPath1))
    } yield ()
  }

}
