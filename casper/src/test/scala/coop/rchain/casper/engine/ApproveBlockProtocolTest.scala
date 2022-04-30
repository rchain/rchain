package coop.rchain.casper.engine

import cats.effect.concurrent.Ref
import com.google.protobuf.ByteString
import coop.rchain.casper.LastApprovedBlock
import coop.rchain.casper.LastApprovedBlock.LastApprovedBlock
import coop.rchain.casper.engine.ApproveBlockProtocolTest.TestFixture
import coop.rchain.casper.protocol.{CommUtil, _}
import coop.rchain.casper.util.GenesisBuilder._
import coop.rchain.casper.util.TestTime
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.comm._
import coop.rchain.comm.rp.Connect.Connections
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.p2p.EffectsTestInstances._
import coop.rchain.shared.{Cell, _}
import monix.eval.Task
import monix.execution.schedulers.TestScheduler
import org.scalatest._

import scala.concurrent.duration._
import scala.util.Success

class ApproveBlockProtocolTest extends FlatSpec with Matchers {
  private val METRICS_APPROVAL_COUNTER_NAME = "rchain.casper.approve-block.genesis"

  val keyPair @ (validatorSk, validatorPk) = Secp256k1.newKeyPair

  "ApproveBlockProtocol" should "add valid signatures it receives to its state" in {
    implicit val ctx          = TestScheduler()
    implicit val logStub      = new LogStub[Task]()
    implicit val eventLogStub = new EventLogStub[Task]()
    implicit val metricsTest  = new MetricsTestImpl[Task]()

    val TestFixture(_, abp, candidate, _, sigsF) =
      ApproveBlockProtocolTest.createProtocol(10, 100.milliseconds, 1.millisecond, Set(keyPair))
    val a = ApproveBlockProtocolTest.approval(candidate, validatorSk, validatorPk)

    val cancelToken = abp.run().start.runToFuture
    sigsF.get.unsafeRunSync.size should be(0)
    metricsTest.counters.get(METRICS_APPROVAL_COUNTER_NAME) should be(None)
    abp.addApproval(a).unsafeRunSync
    ctx.tick(1.millisecond)
    sigsF.get.unsafeRunSync.size should be(1)
    metricsTest.counters(METRICS_APPROVAL_COUNTER_NAME) should be(1)
    cancelToken.cancel()
  }

  it should "not change the number of signatures in its state if the same one is given multiple times" in {
    implicit val ctx          = TestScheduler()
    implicit val logStub      = new LogStub[Task]()
    implicit val eventLogStub = new EventLogStub[Task]()
    implicit val metricsTest  = new MetricsTestImpl[Task]()

    val TestFixture(_, abp, candidate, _, sigsF) =
      ApproveBlockProtocolTest.createProtocol(10, 100.milliseconds, 1.millisecond, Set(keyPair))
    val a = ApproveBlockProtocolTest.approval(candidate, validatorSk, validatorPk)

    val cancelToken = abp.run().start.runToFuture
    sigsF.get.unsafeRunSync.size should be(0)
    metricsTest.counters.get(METRICS_APPROVAL_COUNTER_NAME) should be(None)
    abp.addApproval(a).unsafeRunSync
    ctx.tick(1.millisecond)
    sigsF.get.unsafeRunSync.size should be(1)
    metricsTest.counters(METRICS_APPROVAL_COUNTER_NAME) should be(1)
    abp.addApproval(a).unsafeRunSync
    ctx.tick(1.millisecond)
    sigsF.get.unsafeRunSync.size should be(1)
    metricsTest.counters(METRICS_APPROVAL_COUNTER_NAME) should be(1)
    cancelToken.cancel()
  }

  it should "not add invalid signatures it receives to its state" in {
    implicit val ctx          = TestScheduler()
    implicit val logStub      = new LogStub[Task]()
    implicit val eventLogStub = new EventLogStub[Task]()
    implicit val metricsTest  = new MetricsTestImpl[Task]()

    val TestFixture(_, abp, candidate, _, sigs) =
      ApproveBlockProtocolTest.createProtocol(10, 100.milliseconds, 1.millisecond, Set(keyPair))
    val a = ApproveBlockProtocolTest.invalidApproval(candidate)

    val cancelToken = abp.run().start.runToFuture
    ctx.tick(1.millisecond)
    sigs.get.unsafeRunSync.size should be(0)
    metricsTest.counters.get(METRICS_APPROVAL_COUNTER_NAME) should be(None)
    abp.addApproval(a).unsafeRunSync
    ctx.tick(1.millisecond)
    sigs.get.unsafeRunSync.size should be(0)
    metricsTest.counters.get(METRICS_APPROVAL_COUNTER_NAME) should be(None)
    cancelToken.cancel()
  }

  val n: Int = 10
  val sigs   = (1 to n).map(_ => Secp256k1.newKeyPair)

  it should "create an approved block if at least the correct number of signatures is collected after the duration has elapsed" in {
    val d: FiniteDuration     = 30.milliseconds
    implicit val ctx          = TestScheduler()
    implicit val logStub      = new LogStub[Task]()
    implicit val eventLogStub = new EventLogStub[Task]()
    implicit val metricsTest  = new MetricsTestImpl[Task]()

    val TestFixture(lab, abp, candidate, startTime, _) =
      ApproveBlockProtocolTest.createProtocol(
        n,
        duration = d,
        interval = 1.millisecond,
        sigs
      )
    ctx.tick(startTime.milliseconds) //align clocks
    val cancelToken = abp.run().start.runToFuture

    (1 to n).foreach { i =>
      val (validatorSk, validatorPk) = sigs(i - 1)
      val blockApproval              = ApproveBlockProtocolTest.approval(candidate, validatorSk, validatorPk)
      abp.addApproval(blockApproval).unsafeRunSync
      ctx.tick(1.millisecond)
    }

    ctx.tick(21.milliseconds)
    lab.get.runToFuture.value.nonEmpty should be(true)
    lab.get.runToFuture.value.get should be('success)
    ctx.clockMonotonic(MILLISECONDS) should be(startTime + d.toMillis + 1)
    metricsTest.counters(METRICS_APPROVAL_COUNTER_NAME) should be(n)

    cancelToken.cancel()
  }

  it should "continue collecting signatures if not enough are collected after the duration has elapsed" in {
    val d: FiniteDuration     = 30.milliseconds
    implicit val ctx          = TestScheduler()
    implicit val logStub      = new LogStub[Task]()
    implicit val eventLogStub = new EventLogStub[Task]()
    implicit val metricsTest  = new MetricsTestImpl[Task]()

    val TestFixture(lab, abp, candidate, startTime, _) =
      ApproveBlockProtocolTest.createProtocol(
        n,
        duration = d,
        interval = 1.millisecond,
        sigs
      )
    ctx.tick(startTime.milliseconds) // align clocks

    val cancelToken = abp.run().start.runToFuture

    (1 to (n / 2)).foreach { i =>
      val (validatorSk, validatorPk) = sigs(i - 1)
      abp
        .addApproval(ApproveBlockProtocolTest.approval(candidate, validatorSk, validatorPk))
        .unsafeRunSync
      ctx.tick(1.millisecond)
    }

    lab.get.runToFuture.value.get should be(Success(None))
    metricsTest.counters(METRICS_APPROVAL_COUNTER_NAME) should be(n / 2)

    ((n / 2) to n).foreach { i =>
      val (validatorSk, validatorPk) = sigs(i - 1)
      abp
        .addApproval(ApproveBlockProtocolTest.approval(candidate, validatorSk, validatorPk))
        .unsafeRunSync
      ctx.tick(1.millisecond)
    }

    val timeElapsed = ctx.clockMonotonic(MILLISECONDS) - startTime
    // since we started at `startTime` and we advanced internal clock `n` times x 1 millisecond
    // we still have to advance internal clock with missing milliseconds
    ctx.tick((d.toMillis - timeElapsed).milliseconds)
    lab.get.runToFuture.value.nonEmpty should be(true)
    lab.get.runToFuture.value.get should be('success)
    metricsTest.counters(METRICS_APPROVAL_COUNTER_NAME) should be(n)

    cancelToken.cancel()
  }

  it should "skip the duration and create and approved block immediately if the required signatures is zero" in {
    val d: FiniteDuration     = 30.milliseconds
    implicit val ctx          = TestScheduler()
    implicit val logStub      = new LogStub[Task]()
    implicit val eventLogStub = new EventLogStub[Task]()
    implicit val metricsTest  = new MetricsTestImpl[Task]()

    val TestFixture(lab, abp, _, startTime, _) =
      ApproveBlockProtocolTest.createProtocol(
        0,
        duration = d,
        interval = 1.millisecond,
        Set(keyPair)
      )

    ctx.tick(startTime.milliseconds) // align clocks

    val cancelToken = abp.run().start.runToFuture
    ctx.tick()

    lab.get.runToFuture.value.nonEmpty should be(true)
    lab.get.runToFuture.value.get should be('success)
    metricsTest.counters.get(METRICS_APPROVAL_COUNTER_NAME) should be(None)

    cancelToken.cancel()
  }

  it should "not accept BlockApproved messages signed by not trusted validators" in {
    implicit val ctx          = TestScheduler()
    implicit val logStub      = new LogStub[Task]()
    implicit val eventLogStub = new EventLogStub[Task]()
    implicit val metricsTest  = new MetricsTestImpl[Task]()

    val (invalidSk, invalidPk) = Secp256k1.newKeyPair
    val TestFixture(_, abp, candidate, _, sigsF) =
      ApproveBlockProtocolTest.createProtocol(10, 100.milliseconds, 1.millisecond, Set(keyPair))
    val a = ApproveBlockProtocolTest.approval(candidate, invalidSk, invalidPk)

    val cancelToken = abp.run().start.runToFuture
    sigsF.get.unsafeRunSync.size should be(0)
    metricsTest.counters.get(METRICS_APPROVAL_COUNTER_NAME) should be(None)
    abp.addApproval(a).unsafeRunSync
    ctx.tick(1.millisecond)
    sigsF.get.unsafeRunSync.size should be(0)
    metricsTest.counters.get(METRICS_APPROVAL_COUNTER_NAME) should be(None)
    cancelToken.cancel()
  }

  private def infosContain[F[_]](start: String, size: Int)(
      implicit logStub: LogStub[F]
  ): Assertion =
    logStub.infos.filter(_.startsWith(start)).size should be(size)

  it should "send UnapprovedBlock message to peers at every interval" in {
    implicit val ctx          = TestScheduler()
    implicit val logStub      = new LogStub[Task]()
    implicit val eventLogStub = new EventLogStub[Task]()
    implicit val metricsTest  = new MetricsTestImpl[Task]()
    val TestFixture(_, abp, candidate, _, sigsF) =
      ApproveBlockProtocolTest.createProtocol(10, 100.milliseconds, 5.millisecond, Set(keyPair))

    val cancelToken = abp.run().start.runToFuture

    // I know that testing the logs is not the best way but I comparing messages sent won't work
    // because we attach `System.currentMillis` to every message.
    ctx.tick(4.millisecond)
    infosContain("Broadcasting UnapprovedBlock", 1)
    infosContain("Received block approval from", 0)
    metricsTest.counters.get(METRICS_APPROVAL_COUNTER_NAME) should be(None)

    val a = ApproveBlockProtocolTest.approval(candidate, validatorSk, validatorPk)
    sigsF.get.unsafeRunSync.size should be(0)
    abp.addApproval(a).unsafeRunSync
    ctx.tick(1.millisecond)
    sigsF.get.unsafeRunSync.size should be(1)
    metricsTest.counters(METRICS_APPROVAL_COUNTER_NAME) should be(1)
    infosContain("Received block approval from", 1)
    infosContain("Broadcasting UnapprovedBlock", 2)
    cancelToken.cancel()
  }

  it should "send ApprovedBlock message to peers once an approved block is created" in {
    implicit val ctx          = TestScheduler()
    implicit val logStub      = new LogStub[Task]()
    implicit val eventLogStub = new EventLogStub[Task]()
    implicit val metricsTest  = new MetricsTestImpl[Task]()
    val TestFixture(_, abp, candidate, startTime, sigsF) =
      ApproveBlockProtocolTest.createProtocol(1, 2.milliseconds, 1.millisecond, Set(keyPair))

    ctx.tick(startTime.milliseconds) // align clocks

    val cancelToken = abp.run().start.runToFuture
    ctx.tick()

    // I know that testing the logs is not the best way but I comparing messages sent won't work
    // because we attach `System.currentMillis` to every message.
    logStub.infos.filter(_.startsWith("Received block approval from")).size should be(0)
    logStub.infos.filter(_.startsWith("Sending ApprovedBlock")).size should be(0)
    metricsTest.counters.get(METRICS_APPROVAL_COUNTER_NAME) should be(None)

    val a = ApproveBlockProtocolTest.approval(candidate, validatorSk, validatorPk)
    sigsF.get.unsafeRunSync.size should be(0)
    abp.addApproval(a).unsafeRunSync

    ctx.tick(1.millisecond)

    sigsF.get.unsafeRunSync.size should be(1)
    logStub.infos.filter(_.startsWith("Received block approval from")).size should be(1)
    metricsTest.counters(METRICS_APPROVAL_COUNTER_NAME) should be(1)
    ctx.tick(1.millisecond)
    logStub.infos.filter(_.startsWith("Sending ApprovedBlock")).size should be(1)
    cancelToken.cancel()
  }
}

object ApproveBlockProtocolTest {
  def approval(
      c: ApprovedBlockCandidate,
      validatorSk: PrivateKey,
      validatorPk: PublicKey
  ): BlockApproval = {
    val sigData = Blake2b256.hash(c.toProto.toByteArray)
    val sig     = Secp256k1.sign(sigData, validatorSk)
    BlockApproval(
      c,
      Signature(ByteString.copyFrom(validatorPk.bytes), "secp256k1", ByteString.copyFrom(sig))
    )
  }

  def invalidApproval(c: ApprovedBlockCandidate): BlockApproval = {
    val (sk, pk) = Secp256k1.newKeyPair
    val sigData  = Blake2b256.hash(c.toProto.toByteArray ++ "wrong data".toArray.map(_.toByte))
    val sig      = Secp256k1.sign(sigData, sk)
    BlockApproval(
      c,
      Signature(ByteString.copyFrom(pk.bytes), "secp256k1", ByteString.copyFrom(sig))
    )
  }

  final case class TestFixture(
      lab: LastApprovedBlock[Task],
      protocol: ApproveBlockProtocol[Task],
      candidate: ApprovedBlockCandidate,
      startTime: Long,
      sigsF: Ref[Task, Set[Signature]]
  )

  def createProtocol(
      requiredSigs: Int,
      duration: FiniteDuration,
      interval: FiniteDuration,
      validatorKeyPairs: Iterable[(PrivateKey, PublicKey)]
  )(
      implicit logStub: LogStub[Task],
      eventLogStub: EventLogStub[Task],
      metrics: MetricsTestImpl[Task]
  ): TestFixture = {
    implicit val time            = TestTime.instance
    implicit val transportLayer  = new TransportLayerStub[Task]
    val src: PeerNode            = peerNode("src", 40400)
    implicit val rpConfAsk       = createRPConfAsk[Task](src)
    implicit val ctx             = monix.execution.Scheduler.Implicits.global
    implicit val connectionsCell = Cell.mvarCell[Task, Connections](List(src)).unsafeRunSync
    implicit val lab             = LastApprovedBlock.unsafe[Task](None)
    implicit val commUtil        = CommUtil.of[Task]

    val genesis = buildGenesis(
      buildGenesisParameters(
        validatorKeyPairs,
        createBonds(validatorKeyPairs.map(_._2))
      )
    ).genesisBlock
    val candidate = ApprovedBlockCandidate(genesis, requiredSigs)
    val sigs      = Ref.unsafe[Task, Set[Signature]](Set.empty)
    val startTime = System.currentTimeMillis()

    val protocol = ApproveBlockProtocol
      .unsafe[Task](genesis, requiredSigs, duration, interval, sigs, startTime)

    TestFixture(lab, protocol, candidate, startTime, sigs)
  }

  private def endpoint(port: Int): Endpoint = Endpoint("host", port, port)

  private def peerNode(name: String, port: Int): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), endpoint(port))
}
