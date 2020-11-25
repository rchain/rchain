package coop.rchain.casper.engine

import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.TestTime
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.blockImplicits
import coop.rchain.shared.{Log, SyncVarOps, Time}
import fs2.Stream
import fs2.concurrent.Queue
import monix.eval.Task
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class LFSBlockRequesterEffectsSpec
    extends FlatSpec
    with Matchers
    with GeneratorDrivenPropertyChecks {

  def mkHash(s: String) = ByteString.copyFromUtf8(s)

  def getBlock(hash: BlockHash, number: Long, latestMessages: Seq[BlockHash]) = {
    val justifications                     = latestMessages.map(Justification(ByteString.EMPTY, _))
    val hashFun: BlockMessage => BlockHash = _ => hash
    blockImplicits.getRandomBlock(
      hashF = hashFun.some,
      setBlockNumber = number.some,
      setJustifications = justifications.some,
      // Parents must be set also to prevent auto generation
      setParentsHashList = justifications.map(_.latestBlockHash).some
    )
  }

  def createApprovedBlock(block: BlockMessage): ApprovedBlock = {
    val candidate = ApprovedBlockCandidate(block, requiredSigs = 0)
    ApprovedBlock(candidate, Nil)
  }

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

  trait Effects[F[_]] {
    def requestForBlock(hash: BlockHash): F[Unit]
    def containsBlockInStore(hash: BlockHash): F[Boolean]
    def getBlock(hash: BlockHash): F[BlockMessage]
    def putBlockToStore(hash: BlockHash, b: BlockMessage): F[Unit]
    def validateBlock(b: BlockMessage): F[Boolean]
  }

  case class EffectsImpl[F[_]: Sync](
      var requests: List[BlockHash],
      var puts: Map[BlockHash, BlockMessage],
      var invalids: Set[BlockHash]
  ) extends Effects[F] {
    val lock = SyncVarOps.create(this)

    // Helper for multi-thread safe update of the state
    def atomically[A](operation: => A): F[A] =
      Sync[F].delay {
        lock.take()
        val result = operation
        lock.put(this)
        result
      }

    implicit val ordBytes = Ordering.by((_: ByteString).toByteArray.toIterable)

    override def requestForBlock(hash: BlockHash): F[Unit] =
      atomically(requests = (requests :+ hash).sorted.reverse).void

    override def containsBlockInStore(hash: BlockHash): F[Boolean] =
      atomically(puts.contains(hash))

    override def getBlock(hash: BlockHash): F[BlockMessage] =
      atomically(puts(hash))

    override def putBlockToStore(hash: BlockHash, b: BlockMessage): F[Unit] =
      atomically(puts = puts + ((hash, b))).void

    override def validateBlock(b: BlockMessage): F[Boolean] =
      atomically(!invalids(b.blockHash))
  }

  trait SUT[F[_], Eff] {
    // Processing stream
    val stream: Stream[F, Boolean]
    // Fill response queue - simulate receiving blocks from external source
    def receive(bs: BlockMessage*): F[Unit]
    // Observed effects (sent requests, saved blocks, ...)
    val eff: Eff
  }

  def createSut[F[_]: Concurrent: Time: Log, Eff <: Effects[F]](
      startBlock: BlockMessage,
      effects: Eff,
      requestTimeout: FiniteDuration
  )(test: SUT[F, Eff] => F[Unit]): F[Unit] = {
    import cats.instances.list._

    val approvedBlock = createApprovedBlock(startBlock)
    for {
      // Queue for received blocks
      responseQueue <- Queue.unbounded[F, BlockMessage]
      // Queue for processing the internal state (ST)
      requestStream <- LastFinalizedStateBlockRequester.stream(
                        approvedBlock,
                        responseQueue,
                        initialMinimumHeight = 0,
                        requestTimeout,
                        effects.requestForBlock,
                        effects.containsBlockInStore,
                        effects.getBlock,
                        effects.putBlockToStore,
                        effects.validateBlock
                      )

      receiveBlocks = (bs: List[BlockMessage]) => {
        bs.traverse_(responseQueue.enqueue1(_) >> requestStream.take(1).compile.drain)
      }

      sut = new SUT[F, Eff] {
        override val stream: Stream[F, Boolean]          = requestStream
        override def receive(bs: BlockMessage*): F[Unit] = receiveBlocks(bs.toList)
        override val eff: Eff                            = effects
      }

      // Take one element from the stream, processed the first signal on start
      _ <- requestStream.take(1).compile.drain

      // Execute test function
      _ <- test(sut)
    } yield ()
  }

  implicit val logEff: Log[Task]   = Log.log[Task]
  implicit val timeEff: Time[Task] = TestTime.instance

  import monix.execution.Scheduler.Implicits.global

  // Default timeout is set to large value to disable re-request messages if CI is slow.
  def dagFromBlock(startBlock: BlockMessage, requestTimeout: FiniteDuration = 10.days)(
      f: SUT[Task, EffectsImpl[Task]] => Task[Unit]
  ): Unit =
    createSut[Task, EffectsImpl[Task]](
      startBlock,
      EffectsImpl[Task](Nil, Map(), Set()),
      requestTimeout
    )(f)
    // These tests should be executed in milliseconds or maximum seconds,
    //  but large timeout is because of CI which can pause execution.
      .runSyncUnsafe(timeout = 3.minutes)

  def asMap(bs: BlockMessage*): Map[BlockHash, BlockMessage] = bs.map(b => (b.blockHash, b)).toMap

  it should "send requests for dependencies" in dagFromBlock(b8) { sut =>
    import sut._
    for {
      // Receive of parent should create requests for justifications (dependencies)
      _ <- receive(b8)

      _ = eff.requests shouldBe List(hash8, hash7, hash5)
    } yield ()
  }

  it should "first request dependencies from starting block" in dagFromBlock(b8) { sut =>
    import sut._
    for {
      // Receive of starting block
      _ <- receive(b8)

      // Requested dependencies from starting blocks
      _ = eff.requests shouldBe List(hash8, hash7, hash5)
      _ = eff.requests = Nil

      // Receive only one dependency
      _ <- receive(b7)

      // No new requests until all dependencies received
      _ = eff.requests shouldBe Nil

      // Receive the last dependency (the last of latest blocks)
      _ <- receive(b5)

      // All dependencies should be requested
      _ = eff.requests shouldBe List(hash6, hash3)
    } yield ()
  }

  it should "save received blocks if requested" in dagFromBlock(b9) { sut =>
    import sut._
    for {
      // Receive first block (with dependencies)
      _ <- receive(b9, b8)

      // Receive one dependency
      _ <- receive(b7)

      // Both blocks should be saved
      _ = eff.puts shouldBe asMap(b9, b8, b7)
    } yield ()
  }

  it should "skip received invalid blocks" in dagFromBlock(b9) { sut =>
    import sut._
    for {
      // Receive first block (with dependencies)
      _ <- receive(b9, b8)
      _ = eff.requests = Nil
      _ = eff.puts = Map()

      // Set invalid blocks
      _ = eff.invalids = Set(hash5)

      // Receive dependencies
      _ <- receive(b7, b5)

      // Only valid block should be saved
      _ = eff.puts shouldBe asMap(b7)

      // Only dependencies from valid block should requested
      _ = eff.requests shouldBe List(hash6)
    } yield ()
  }

  it should "drop all blocks not requested" in dagFromBlock(b9) { sut =>
    import sut._
    for {
      // Receive blocks not requested
      _ <- receive(b7, b6, b5, b4, b3, b2, b1)

      // It should contain only request for the first block
      _ = eff.requests shouldBe List(hash9, hash8)
      // Nothing should be saved
      _ = eff.puts shouldBe Map()
    } yield ()
  }

  it should "request and save all blocks" in dagFromBlock(b9) { sut =>
    import sut._
    // Staring block with dependencies should be requested
    eff.requests shouldBe List(hash9, hash8)
    eff.requests = Nil
    for {
      // Receive starting block and its dependencies (latest blocks)
      _ <- receive(b9, b8)

      // Dependencies of b8 should be in requests also
      _ = eff.requests shouldBe List(hash7, hash5)
      _ = eff.requests = Nil
      // Starting block and its dependencies should be saved
      _ = eff.puts shouldBe asMap(b9, b8)

      // Receive blocks b7 and b5
      _ <- receive(b7, b5)

      // All blocks should be requested
      _ = eff.requests shouldBe List(hash6, hash3)
      _ = eff.requests = Nil
      // Received blocks should be saved
      _ = eff.puts shouldBe asMap(b9, b8, b7, b5)

      // Receive blocks b6, b5 and b3
      _ <- receive(b6, b5, b3)

      // All blocks should be requested
      _ = eff.requests shouldBe List(hash4, hash1)
      _ = eff.requests = Nil
      // All blocks should be saved
      _ = eff.puts shouldBe asMap(b9, b8, b7, b6, b5, b3)

      // Receive block b3
      _ <- receive(b4, b1)

      // All blocks should be requested
      _ = eff.requests shouldBe List(hash2)
      _ = eff.requests = Nil
      // All blocks should be saved
      _ = eff.puts shouldBe asMap(b9, b8, b7, b6, b5, b4, b3, b1)

      // Receive block b2
      _ <- receive(b2)

      // All blocks should be requested
      _ = eff.requests shouldBe Nil
      // All blocks should be saved
      _ = eff.puts shouldBe asMap(b9, b8, b7, b6, b5, b4, b3, b2, b1)
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
  it should "re-send request after timeout" in dagFromBlock(b9, requestTimeout = 300.millis) {
    sut =>
      import sut._
      for {
        // Wait for timeout to expire
        _ <- stream.compile.drain.timeout(350.millis).onErrorHandle(_ => ())

        // Request should be repeated
        _ = eff.requests shouldBe List(hash9, hash9, hash8, hash8)
      } yield ()
  }

}
