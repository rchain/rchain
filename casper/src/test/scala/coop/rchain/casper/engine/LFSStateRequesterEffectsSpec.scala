package coop.rchain.casper.engine

import cats.effect.Concurrent
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.engine.LfsTupleSpaceRequester.{ST, StatePartPath}
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.TestTime
import coop.rchain.models.blockImplicits
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.state.{RSpaceImporter, StateValidationError}
import coop.rchain.shared.{Log, Time}
import fs2.Stream
import fs2.concurrent.Queue
import monix.eval.Task
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import scodec.bits.ByteVector

import java.nio.ByteBuffer
import java.util.concurrent.TimeoutException
import scala.concurrent.duration.DurationInt

class LFSStateRequesterEffectsSpec
    extends FlatSpec
    with Matchers
    with GeneratorDrivenPropertyChecks {

  def createApprovedBlock(block: BlockMessage): ApprovedBlock = {
    val candidate = ApprovedBlockCandidate(block, requiredSigs = 0)
    ApprovedBlock(candidate, Nil)
  }

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

  trait SUT[F[_]] {
    // Fill response queue - simulate receiving StoreItems from external source
    def receive(item: StoreItemsMessage*): F[Unit]

    // Observed requests
    val requests: Stream[F, (StatePartPath, Int)]

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
  def createSut[F[_]: Concurrent: Time: Log](test: SUT[F] => F[Unit]): F[Unit] = {

    def alwaysGoodStateValidator(
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

      // Queue for saved chunks
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

        override def getHistoryItem(hash: Blake2b256Hash): F[Option[ByteVector]] = ???
      }

      // Queue for processing the internal state (ST)
      processingStream <- LfsTupleSpaceRequester.stream(
                           approvedBlock,
                           responseQueue,
                           requestQueue.enqueue1(_, _),
                           importer,
                           alwaysGoodStateValidator
                         )

      sut = new SUT[F] {
        override def receive(msgs: StoreItemsMessage*): F[Unit] =
          responseQueue.enqueue(Stream.emits(msgs)).compile.drain

        override val requests: Stream[F, (StatePartPath, Int)] = requestQueue.dequeue

        override val savedHistory: Stream[F, SavedStoreItems] =
          savedHistoryQueue.dequeue
        override val savedData: Stream[F, SavedStoreItems] =
          savedDataQueue.dequeue

        override val stream: Stream[F, ST[StatePartPath]] = processingStream
      }

      // Take one element from the stream, processed the first signal on start
      _ <- processingStream.take(1).compile.drain

      // Execute test function together with processing stream
      _ <- test(sut)
    } yield ()
  }

  implicit val logEff: Log[Task]   = Log.log[Task]
  implicit val timeEff: Time[Task] = TestTime.instance

  import monix.execution.Scheduler.Implicits.global

  /**
    * Test runner
    *
    * @param runProcessingStream flag to automatically run processing stream
    * @param test test specification
    */
  def createBootstrapTest(runProcessingStream: Boolean)(
      test: SUT[Task] => Task[Unit]
  ): Unit =
    createSut[Task] { sut =>
      if (!runProcessingStream) test(sut)
      else (Stream.eval(test(sut)) concurrently sut.stream).compile.drain
    }.runSyncUnsafe(timeout = 10.seconds)

  val bootstrapTest = createBootstrapTest(runProcessingStream = true) _

  it should "send requests for dependencies" in bootstrapTest { sut =>
    import sut._
    for {
      reqs <- requests.take(1).compile.toList

      // After start, first request should be for approved block post state
      _ = reqs shouldBe List(mkRequest(historyPath1))

      // Receives store items message
      _ <- receive(StoreItemsMessage(historyPath1, historyPath2, history1, data1))

      reqs <- requests.take(1).compile.toList

      // After first chunk received, next chunk should be requested (last past from previous chunk)
      _ = reqs shouldBe List(mkRequest(historyPath2))
    } yield ()
  }

  it should "save requested responses" in bootstrapTest { sut =>
    import sut._
    for {
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

  it should "not request next chunk if message is not requested" in bootstrapTest { sut =>
    import sut._
    for {
      reqs <- requests.take(1).compile.toList

      // After start, first request should be for approved block post state
      _ = reqs shouldBe List(mkRequest(historyPath1))

      // Receives store items message which is not requested
      _ <- receive(StoreItemsMessage(historyPath2, historyPath3, history2, data2))

      // Request should not be sent for the next chunk,
      //  getting element from the queue should fail with timeout
      reqs <- requests.take(1).timeout(250.millis).compile.toList.attempt

      // After first chunk received, next chunk should be requested (last past from previous chunk)
      _ = reqs shouldBe a[Left[TimeoutException, _]]
    } yield ()
  }

  it should "not save message which is not requested" in bootstrapTest { sut =>
    import sut._
    for {
      // Receives store items message which is not requested
      _ <- receive(StoreItemsMessage(historyPath2, historyPath3, history2, data2))

      // Get from saved queue should fail with timeout
      history <- savedHistory.take(1).timeout(250.millis).compile.toList.attempt

      // No history items should be saved
      _ = history shouldBe a[Left[TimeoutException, _]]

      // Get from saved queue should fail with timeout
      data <- savedData.take(1).timeout(250.millis).compile.toList.attempt

      // No data items should be saved
      _ = data shouldBe a[Left[TimeoutException, _]]
    } yield ()
  }

  it should "stop if invalid state received" in createBootstrapTest(runProcessingStream = false) {
    sut =>
      import sut._
      for {
        // Receives store items message
        _ <- receive(StoreItemsMessage(historyPath1, historyPath2, invalidHistory, data1))

        result <- stream.compile.lastOrError.attempt

        // Stream should fail with an error
        _ = result shouldBe Left(exceptionInvalidState)
      } yield ()
  }
}
