package coop.rchain.casper.engine

import cats.effect.unsafe.implicits.global
import cats.effect.{Async, IO}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.engine.LfsBlockRequester.ST
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.scalatest.Fs2StreamMatchers
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.blockImplicits
import coop.rchain.shared.Log
import fs2.Stream
import fs2.concurrent.Channel
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._
import cats.effect.{Ref, Temporal}

class LfsBlockRequesterEffectsSpec extends AnyFlatSpec with Matchers with Fs2StreamMatchers {

  def mkHash(s: String) = ByteString.copyFromUtf8(s)

  def getBlock(hash: BlockHash, number: Long, latestMessages: Seq[BlockHash]) = {
    val hashFun: BlockMessage => BlockHash = _ => hash
    blockImplicits.getRandomBlock(
      hashF = hashFun.some,
      setBlockNumber = number.some,
      setJustifications = latestMessages.some
    )
  }

  def createFinalizedFringe(block: BlockMessage): FinalizedFringe =
    FinalizedFringe(block.justifications, block.postStateHash)

  val hash9 = mkHash("9")
  val hash8 = mkHash("8")
  val hash7 = mkHash("7")
  val hash6 = mkHash("6")
  val hash5 = mkHash("5")
  val hash4 = mkHash("4")
  val hash3 = mkHash("3")
  val hash2 = mkHash("2")
  val hash1 = mkHash("1")

  val b9 = getBlock(hash9, number = 109, Seq(hash8))
  val b8 = getBlock(hash8, number = 108, Seq(hash7, hash5))
  val b7 = getBlock(hash7, number = 107, Seq(hash6, hash5))
  val b6 = getBlock(hash6, number = 106, Seq(hash4))
  val b5 = getBlock(hash5, number = 75, Seq(hash3))
  val b4 = getBlock(hash4, number = 34, Seq(hash2))
  val b3 = getBlock(hash3, number = 23, Seq(hash1))
  val b2 = getBlock(hash2, number = 2, Seq())
  val b1 = getBlock(hash1, number = 1, Seq())

  import scala.math.Ordering.Implicits.seqOrdering
  implicit val ordBytes = Ordering.by((_: ByteString).toByteArray.toSeq).reverse

  case class TestST(blocks: Map[BlockHash, BlockMessage], invalid: Set[BlockHash])

  trait Mock[F[_]] {
    // Fill response queue - simulate receiving block from external source
    def receiveBlock(blocks: BlockMessage*): F[Unit]

    // Observed requests
    val sentRequests: Stream[F, BlockHash]

    // Observed saved blocks
    val savedBlocks: Stream[F, (BlockHash, BlockMessage)]

    // Test state
    val setup: Ref[F, TestST]

    // Processing stream
    val stream: Stream[F, ST[BlockHash]]
  }

  /**
    * Creates test setup
    *
    * @param test test definition
    */
  def createMock[F[_]: Async: Log](
      startBlock: BlockMessage,
      requestTimeout: FiniteDuration
  )(test: Mock[F] => F[Unit]): F[Unit] = {

    // Finalized fringe is initial latest messages
    val finalizedFringe = createFinalizedFringe(startBlock)

    // Approved block is already saved in block storage
    val savedBlocks = Map(startBlock.blockHash -> startBlock)

    for {
      testState <- Ref.of[F, TestST](TestST(blocks = savedBlocks, invalid = Set()))

      // Queue for received blocks
      responseQueue <- Channel.unbounded[F, BlockMessage]

      // Queue for requested block hashes
      requestQueue <- Channel.unbounded[F, BlockHash]

      // Queue for saved blocks
      savedBlocksQueue <- Channel.unbounded[F, (BlockHash, BlockMessage)]

      // Queue for processing the internal state (ST)
      processingStream <- LfsBlockRequester.stream[F](
                           finalizedFringe,
                           responseQueue.stream,
                           blockHeightsBeforeFringe = 0,
                           requestQueue.send(_).void,
                           requestTimeout,
                           hash => testState.get.map(_.blocks.contains(hash)),
                           hash => testState.get.map(_.blocks(hash)),
                           (h, m) => savedBlocksQueue.send(h -> m).void,
                           block => testState.get.map(!_.invalid.contains(block.blockHash))
                         )

      mock = new Mock[F] {
        override def receiveBlock(blocks: BlockMessage*): F[Unit] =
          Stream.emits(blocks).evalMap(responseQueue.send).compile.drain

        override val sentRequests: Stream[F, BlockHash]                = requestQueue.stream
        override val savedBlocks: Stream[F, (BlockHash, BlockMessage)] = savedBlocksQueue.stream

        override val setup: Ref[F, TestST] = testState

        override val stream: Stream[F, ST[BlockHash]] = processingStream
      }

      // Execute test function together with processing stream
      _ <- test(mock)
    } yield ()
  }

  implicit val logEff: Log[IO] = Log.log[IO]

  /**
    * Test runner
    *
    *  - Default request timeout is set to large value to disable re-request messages if CI is slow.
    *
    * @param startBlock start of the block DAG
    * @param requestTimeout request resend timeout
    * @param test test specification
    */
  def dagFromBlock(
      startBlock: BlockMessage,
      runProcessingStream: Boolean = true,
      requestTimeout: FiniteDuration = 10.days
  )(test: Mock[IO] => IO[Unit]): Unit =
    createMock[IO](startBlock, requestTimeout) { mock =>
      if (!runProcessingStream) test(mock)
      else (Stream.eval(test(mock)) concurrently mock.stream).compile.drain
    }.unsafeRunTimed(10.seconds)

  def asMap(bs: BlockMessage*): Map[BlockHash, BlockMessage] = bs.map(b => (b.blockHash, b)).toMap

  it should "send requests for dependencies" in dagFromBlock(b8) { mock =>
    import mock._
    for {
      // Receive of parent should create requests for justifications (dependencies)
      reqs <- sentRequests.take(2).compile.to(List)
      _    = reqs.sorted shouldBe List(hash7, hash5)

      // No other requests should be sent
      _ = sentRequests should notEmit
    } yield ()
  }

  it should "not request saved blocks" in dagFromBlock(b8) { mock =>
    import mock._
    for {
      // Receive of parent should create requests for justifications (dependencies)
      reqs <- sentRequests.take(2).compile.to(List)
      _    = reqs.sorted shouldBe List(hash7, hash5)

      // No other requests should be sent
      _ = sentRequests should notEmit

      // Dependent block is already saved
      _ <- setup.update(x => x.copy(blocks = x.blocks ++ asMap(b6)))

      _ <- receiveBlock(b7, b5)

      reqs <- sentRequests.take(2).compile.to(List)
      _    = reqs.sorted shouldBe List(hash4, hash3)

      // No other requests should be sent
      _ = sentRequests should notEmit
    } yield ()
  }

  it should "first request dependencies only from starting block" in dagFromBlock(b8) { mock =>
    import mock._
    for {
      // Requested dependencies from starting blocks
      reqs <- sentRequests.take(2).compile.to(List)
      _    = reqs.sorted shouldBe List(hash7, hash5)

      // Receive only one dependency
      _ <- receiveBlock(b7)

      // No new requests until all dependencies received
      _ = sentRequests should notEmit

      // Receive the last dependency (the last of latest blocks)
      _ <- receiveBlock(b5)

      // All dependencies should be requested
      reqs <- sentRequests.take(2).compile.to(List)
      _    = reqs.sorted shouldBe List(hash6, hash3)
    } yield ()
  }

  it should "save received blocks if requested" in dagFromBlock(b9) { mock =>
    import mock._
    for {
      // Wait for initial request to be sent
      reqs <- sentRequests.take(1).compile.to(List)
      _    = reqs.sorted shouldBe List(hash8)

      // Receive first block dependencies
      _ <- receiveBlock(b8)

      // Received blocks should be saved
      puts <- savedBlocks.take(1).compile.to(Map)
      _    = puts shouldBe asMap(b8)

      // Wait for requests to be sent
      reqs <- sentRequests.take(2).compile.to(List)
      _    = reqs.sorted shouldBe List(hash7, hash5)

      // No other requests should be sent
      _ = sentRequests should notEmit

      // Receive one dependency
      _ <- receiveBlock(b7)

      // Both blocks should be saved
      puts <- savedBlocks.take(1).compile.to(Map)
      _    = puts shouldBe asMap(b7)

      // No other blocks should be saved
      _ = savedBlocks should notEmit
    } yield ()
  }

  it should "drop all blocks not requested" in dagFromBlock(b9) { mock =>
    import mock._
    for {
      // Wait for initial request to be sent
      reqs <- sentRequests.take(1).compile.to(List)
      _    = reqs.sorted shouldBe List(hash8)

      // Receive blocks not requested
      _ <- receiveBlock(b7, b6, b5, b4, b3, b2, b1)

      // No other requests should be sent
      _ = sentRequests should notEmit

      // Nothing else should be saved
      _ = savedBlocks should notEmit
    } yield ()
  }

  it should "skip received invalid blocks" in dagFromBlock(b9) { mock =>
    import mock._
    for {
      // Wait for initial request to be sent
      reqs <- sentRequests.take(1).compile.to(List)
      _    = reqs.sorted shouldBe List(hash8)

      // Receive first block dependencies
      _ <- receiveBlock(b8)

      // Only valid block should be saved
      puts <- savedBlocks.take(1).compile.to(Map)
      _    = puts shouldBe asMap(b8)

      // No other blocks should be saved
      _ = savedBlocks should notEmit

      // Set invalid blocks
      _ <- setup.update(_.copy(invalid = Set(hash5)))

      // Receive dependencies
      _ <- receiveBlock(b7, b5)

      // Only valid block should be saved
      puts <- savedBlocks.take(1).compile.to(Map)
      _    = puts shouldBe asMap(b7)

      // No other blocks should be saved
      _ = savedBlocks should notEmit

      // Only dependencies from valid block should requested
      reqs <- sentRequests.take(3).compile.to(List)
      _    = reqs.sorted shouldBe List(hash7, hash6, hash5)

      // No other requests should be sent
      _ = sentRequests should notEmit
    } yield ()
  }

  it should "request and save all blocks" in dagFromBlock(b9) { mock =>
    import mock._
    for {
      // Staring block dependencies should be requested
      reqs <- sentRequests.take(1).compile.to(List)
      _    = reqs.sorted shouldBe List(hash8)

      // Receive starting block dependencies (latest blocks)
      _ <- receiveBlock(b8)

      // Dependencies of b8 should be in requests also
      reqs <- sentRequests.take(2).compile.to(List)
      _    = reqs.sorted shouldBe List(hash7, hash5)
      // Starting block dependencies should be saved
      puts <- savedBlocks.take(1).compile.to(Map)
      _    = puts shouldBe asMap(b8)

      // Receive blocks b7 and b5
      _ <- receiveBlock(b7, b5)

      // All blocks should be requested
      reqs <- sentRequests.take(2).compile.to(List)
      _    = reqs.sorted shouldBe List(hash6, hash3)
      // Received blocks should be saved
      puts <- savedBlocks.take(2).compile.to(Map)
      _    = puts shouldBe asMap(b7, b5)

      // Receive blocks b6, b5 and b3
      _ <- receiveBlock(b6, b5, b3)

      // All blocks should be requested
      reqs <- sentRequests.take(2).compile.to(List)
      _    = reqs.sorted shouldBe List(hash4, hash1)
      // All blocks should be saved
      puts <- savedBlocks.take(2).compile.to(Map)
      _    = puts shouldBe asMap(b6, b3)

      // Receive blocks b4 and b1
      _ <- receiveBlock(b4, b1)

      // All blocks should be requested
      reqs <- sentRequests.take(1).compile.to(List)
      _    = reqs.sorted shouldBe List(hash2)
      // All blocks should be saved
      puts <- savedBlocks.take(2).compile.to(Map)
      _    = puts shouldBe asMap(b4, b1)

      // Receive block b2
      _ <- receiveBlock(b2)

      // All blocks should already be be requested, no new requests
      _ = sentRequests should notEmit
      // All blocks should be saved
      puts <- savedBlocks.take(1).compile.to(Map)
      _    = puts shouldBe asMap(b2)

      // Nothing else should be saved
      _ = savedBlocks should notEmit
    } yield ()
  }

  /**
    * Test for request timeout. This is timing test which in CI can be a problem if execution is paused.
    *
    * NOTE: We don't have any abstraction to test time in execution (with monix Task or cats IO).
    *  We have LogicalTime and DiscreteTime which are just wrappers to get different "milliseconds" but are totally
    *  disconnected from Task/IO execution notion of time (e.g. IO.sleep).
    *  Other testing instances of Time are the same as in normal node execution (using IO.timer).
    *  https://github.com/rchain/rchain/issues/3001
    */
  it should "re-send request after timeout" in dagFromBlock(
    b9,
    runProcessingStream = false,
    requestTimeout = 200.millis
  ) { mock =>
    import mock._
    for {
      // Wait for timeout to expire
      _ <- stream.compile.drain.timeout(300.millis).attempt

      // Wait for two requests
      reqs <- sentRequests.take(2).compile.toList

      // Both requests should be repeated by resend
      _ = reqs.sorted shouldBe List(hash8, hash8).sorted

      // No other requests should be sent
      _ = sentRequests should notEmit
    } yield ()
  }
}
