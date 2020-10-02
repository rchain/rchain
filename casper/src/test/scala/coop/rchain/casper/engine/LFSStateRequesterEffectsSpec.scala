package coop.rchain.casper.engine

import java.nio.ByteBuffer

import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.engine.LastFinalizedStateTupleSpaceRequester.StatePartPath
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.TestTime
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.blockImplicits
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.state.RSpaceImporter
import coop.rchain.shared.{Log, SyncVarOps, Time}
import coop.rchain.state.TrieImporter
import fs2.Stream
import fs2.concurrent.Queue
import monix.eval.Task
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import scodec.bits.ByteVector

import scala.concurrent.duration._

class LFSStateRequesterEffectsSpec
    extends FlatSpec
    with Matchers
    with GeneratorDrivenPropertyChecks {

  def createApprovedBlock(block: BlockMessage): ApprovedBlock = {
    val candidate = ApprovedBlockCandidate(block, requiredSigs = 0)
    ApprovedBlock(candidate, Nil)
  }

  trait Effects[F[_]] {
    def requestForStoreItem(s: StatePartPath, d: Int): F[Unit]

    def setHistoryItems[Value](
        item: Seq[(Blake2b256Hash, Value)],
        conv: Value => ByteBuffer
    ): F[Unit]

    def setDataItems[Value](
        item: Seq[(Blake2b256Hash, Value)],
        conv: Value => ByteBuffer
    ): F[Unit]
  }

  case class EffectsImpl[F[_]: Sync](
      var requests: List[(StatePartPath, Int)],
      var historyStore: Seq[Blake2b256Hash],
      var coldStore: Seq[Blake2b256Hash]
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

    override def requestForStoreItem(s: StatePartPath, d: Int): F[Unit] =
      atomically(requests = requests :+ (s, d)).void

    override def setHistoryItems[Value](
        item: Seq[(Blake2b256Hash, Value)],
        conv: Value => ByteBuffer
    ) =
      atomically(historyStore = historyStore ++ item.map(_._1)).void

    override def setDataItems[Value](
        item: Seq[(Blake2b256Hash, Value)],
        conv: Value => ByteBuffer
    ) =
      atomically(coldStore = coldStore ++ item.map(_._1)).void
  }

  trait SUT[F[_], Eff] {
    // Processing stream
    val stream: Stream[F, Boolean]

    // Fill response queue - simulate receiving StoreItems from external source
    def receive(item: StoreItemsMessage*): F[Unit]

    // Observed effects (data persisted, data requested, ...)
    val eff: Eff
  }

  def createSut[F[_]: Concurrent: Time: Log, Eff <: Effects[F]](
      effects: Eff
  )(test: SUT[F, Eff] => F[Unit]): F[Unit] = {
    import cats.instances.list._

    val dummyStateImporter = new RSpaceImporter[F] {
      override type KeyHash = Blake2b256Hash

      override def setHistoryItems[Value](
          data: Seq[(KeyHash, Value)],
          toBuffer: Value => ByteBuffer
      ): F[Unit] = effects.setHistoryItems(data, toBuffer)

      override def setDataItems[Value](
          data: Seq[(KeyHash, Value)],
          toBuffer: Value => ByteBuffer
      ): F[Unit] = effects.setDataItems(data, toBuffer)

      override def setRoot(key: KeyHash): F[Unit] = ().pure[F]

      override def getHistoryItem(hash: Blake2b256Hash): F[Option[ByteVector]] = ???
    }

    def alwaysGoodStateValidator(
        historyItems: Seq[(Blake2b256Hash, ByteVector)],
        dataItems: Seq[(Blake2b256Hash, ByteVector)],
        startPath: Seq[(Blake2b256Hash, Option[Byte])],
        chunkSize: Int,
        skip: Int,
        getFromHistory: Blake2b256Hash => F[Option[ByteVector]]
    ): F[Unit] = ().pure[F]

    val approvedBlock = createApprovedBlock(blockImplicits.getRandomBlock())
    for {
      // Queue for received blocks
      responseQueue <- Queue.unbounded[F, StoreItemsMessage]
      // Queue for processing the internal state (ST)
      requestStream <- LastFinalizedStateTupleSpaceRequester.stream(
                        approvedBlock,
                        responseQueue,
                        effects.requestForStoreItem,
                        dummyStateImporter,
                        alwaysGoodStateValidator
                      )

      receiveStoreItems = (sims: List[StoreItemsMessage]) => {
        sims.traverse_(responseQueue.enqueue1(_) >> requestStream.take(1).compile.drain)
      }

      sut = new SUT[F, Eff] {
        override val stream: Stream[F, Boolean] = requestStream

        override def receive(item: StoreItemsMessage*): F[Unit] = receiveStoreItems(item.toList)

        override val eff: Eff = effects
      }

      // Take one element from the stream, processed the first signal on start
      _ <- requestStream.take(1).compile.drain

      // Execute test function
      _ <- test(sut)
    } yield ()
  }

  implicit val logEff: Log[Task]   = Log.log[Task]
  implicit val timeEff: Time[Task] = TestTime.instance

  val sim1StartHash   = Blake2b256Hash.create(Array(1.toByte))
  val sim1EndHash     = Blake2b256Hash.create(Array(2.toByte))
  val sim1HistoryData = Blake2b256Hash.create(Array(3.toByte))
  val sim1ColdData    = Blake2b256Hash.create(Array(3.toByte))

  import monix.execution.Scheduler.Implicits.global

  def bootstrapTest(
      f: SUT[Task, EffectsImpl[Task]] => Task[Unit]
  ): Unit =
    createSut[Task, EffectsImpl[Task]](EffectsImpl[Task](List.empty, Seq.empty, Seq.empty))(f)
    // These tests should be executed in milliseconds or maximum seconds,
    //  but large timeout is because of CI which can pause execution.
      .runSyncUnsafe()

  it should "received storeItems should be stored and next item should be called" in bootstrapTest {
    sut =>
      {
        import sut._
        for {
          _ <- receive(
                StoreItemsMessage(
                  Seq((sim1StartHash, None)),
                  Seq((sim1EndHash, None)),
                  Seq((sim1HistoryData, ByteString.EMPTY)),
                  Seq((sim1ColdData, ByteString.EMPTY))
                )
              )
          _ = eff.historyStore.contains(sim1HistoryData) shouldBe true
          _ = eff.coldStore.contains(sim1ColdData) shouldBe true
          _ = eff.requests.contains(sim1EndHash) shouldBe true
        } yield ()
      }
  }
}
