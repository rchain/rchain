package coop.rchain.casper.util.comm

import cats.effect.concurrent.Ref
import coop.rchain.comm.rp.Connect, Connect._
import coop.rchain.shared._
import com.google.protobuf.ByteString
import coop.rchain.casper.HashSetCasperTest
import coop.rchain.casper.helper.HashSetCasperTestNode
import coop.rchain.casper.protocol._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.catscontrib._
import coop.rchain.comm.rp.Connect
import coop.rchain.comm.rp.Connect.Connections
import coop.rchain.comm.{Endpoint, NodeIdentifier, PeerNode}
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.p2p.EffectsTestInstances._
import coop.rchain.shared.Log.NOPLog
import coop.rchain.shared.{Cell, Time}
import monix.eval.Task
import monix.execution.schedulers.TestScheduler

import scala.concurrent.duration._
import org.scalatest.{FlatSpec, Matchers}
import coop.rchain.casper.LastApprovedBlock.LastApprovedBlock
import coop.rchain.casper.util.comm.ApproveBlockProtocolTest.TestFixture
import coop.rchain.casper.{HashSetCasperTest, LastApprovedBlock}
import org.scalatest.{Assertion, FlatSpec, Matchers}
import coop.rchain.casper.util.TestTime

import scala.util.Success

class ApproveBlockProtocolTest extends FlatSpec with Matchers {
  "ApproveBlockProtocol" should "add valid signatures it receives to its state" in {
    implicit val ctx         = TestScheduler()
    implicit val logStub     = new LogStub[Task]()
    implicit val metricsTest = new MetricsTestImpl[Task]()

    val (validatorSk, validatorPk) = Ed25519.newKeyPair
    val TestFixture(_, abp, candidate, _, sigsF) =
      ApproveBlockProtocolTest.createProtocol(10, 100.milliseconds, 1.millisecond, Set(validatorPk))
    val a = ApproveBlockProtocolTest.approval(candidate, validatorSk, validatorPk)

    val cancelToken = abp.run().start.runToFuture
    sigsF.get.unsafeRunSync.size should be(0)
    metricsTest.counters.get(ApproveBlockProtocol.METRICS_APPROVAL_COUNTER_NAME) should be(None)
    abp.addApproval(a).unsafeRunSync
    ctx.tick(1.millisecond)
    sigsF.get.unsafeRunSync.size should be(1)
    metricsTest.counters(ApproveBlockProtocol.METRICS_APPROVAL_COUNTER_NAME) should be(1)
    cancelToken.cancel()
  }

  it should "not change the number of signatures in its state if the same one is given multiple times" in {
    implicit val ctx         = TestScheduler()
    implicit val logStub     = new LogStub[Task]()
    implicit val metricsTest = new MetricsTestImpl[Task]()

    val (validatorSk, validatorPk) = Ed25519.newKeyPair
    val TestFixture(_, abp, candidate, _, sigsF) =
      ApproveBlockProtocolTest.createProtocol(10, 100.milliseconds, 1.millisecond, Set(validatorPk))
    val a = ApproveBlockProtocolTest.approval(candidate, validatorSk, validatorPk)

    val cancelToken = abp.run().start.runToFuture
    sigsF.get.unsafeRunSync.size should be(0)
    metricsTest.counters.get(ApproveBlockProtocol.METRICS_APPROVAL_COUNTER_NAME) should be(None)
    abp.addApproval(a).unsafeRunSync
    ctx.tick(1.millisecond)
    sigsF.get.unsafeRunSync.size should be(1)
    metricsTest.counters(ApproveBlockProtocol.METRICS_APPROVAL_COUNTER_NAME) should be(1)
    abp.addApproval(a).unsafeRunSync
    ctx.tick(1.millisecond)
    sigsF.get.unsafeRunSync.size should be(1)
    metricsTest.counters(ApproveBlockProtocol.METRICS_APPROVAL_COUNTER_NAME) should be(1)
    cancelToken.cancel()
  }

  it should "not add invalid signatures it receives to its state" in {
    implicit val ctx         = TestScheduler()
    implicit val logStub     = new LogStub[Task]()
    implicit val metricsTest = new MetricsTestImpl[Task]()

    val (validatorSk, validatorPk) = Ed25519.newKeyPair
    val TestFixture(_, abp, candidate, _, sigs) =
      ApproveBlockProtocolTest.createProtocol(10, 100.milliseconds, 1.millisecond, Set(validatorSk))
    val a = ApproveBlockProtocolTest.invalidApproval(candidate)

    val cancelToken = abp.run().start.runToFuture
    ctx.tick(1.millisecond)
    sigs.get.unsafeRunSync.size should be(0)
    metricsTest.counters.get(ApproveBlockProtocol.METRICS_APPROVAL_COUNTER_NAME) should be(None)
    abp.addApproval(a).unsafeRunSync
    ctx.tick(1.millisecond)
    sigs.get.unsafeRunSync.size should be(0)
    metricsTest.counters.get(ApproveBlockProtocol.METRICS_APPROVAL_COUNTER_NAME) should be(None)
    cancelToken.cancel()
  }

  it should "create an approved block if at least the correct number of signatures is collected after the duration has elapsed" in {
    val n: Int               = 10
    val d: FiniteDuration    = 30.milliseconds
    implicit val ctx         = TestScheduler()
    implicit val logStub     = new LogStub[Task]()
    implicit val metricsTest = new MetricsTestImpl[Task]()

    val sigs = (1 to n).map(_ => Ed25519.newKeyPair)
    val TestFixture(lab, abp, candidate, startTime, sigsF) =
      ApproveBlockProtocolTest.createProtocol(
        n,
        duration = d,
        interval = 1.millisecond,
        sigs.map(_._2).toSet
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
    metricsTest.counters(ApproveBlockProtocol.METRICS_APPROVAL_COUNTER_NAME) should be(n)

    cancelToken.cancel()
  }

  it should "continue collecting signatures if not enough are collected after the duration has elapsed" in {
    val n: Int               = 10
    val d: FiniteDuration    = 30.milliseconds
    val sigs                 = (1 to n).map(_ => Ed25519.newKeyPair)
    implicit val ctx         = TestScheduler()
    implicit val logStub     = new LogStub[Task]()
    implicit val metricsTest = new MetricsTestImpl[Task]()

    val TestFixture(lab, abp, candidate, startTime, _) =
      ApproveBlockProtocolTest.createProtocol(
        n,
        duration = d,
        interval = 1.millisecond,
        sigs.map(_._2).toSet
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
    metricsTest.counters(ApproveBlockProtocol.METRICS_APPROVAL_COUNTER_NAME) should be(n / 2)

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
    metricsTest.counters(ApproveBlockProtocol.METRICS_APPROVAL_COUNTER_NAME) should be(n)

    cancelToken.cancel()
  }

  it should "skip the duration and create and approved block immediately if the required signatures is zero" in {
    val d: FiniteDuration          = 30.milliseconds
    val (validatorSk, validatorPk) = Ed25519.newKeyPair
    implicit val ctx               = TestScheduler()
    implicit val logStub           = new LogStub[Task]()
    implicit val metricsTest       = new MetricsTestImpl[Task]()

    val TestFixture(lab, abp, _, startTime, _) =
      ApproveBlockProtocolTest.createProtocol(
        0,
        duration = d,
        interval = 1.millisecond,
        Set(validatorPk)
      )

    ctx.tick(startTime.milliseconds) // align clocks

    val cancelToken = abp.run().start.runToFuture
    ctx.tick()

    lab.get.runToFuture.value.nonEmpty should be(true)
    lab.get.runToFuture.value.get should be('success)
    metricsTest.counters.get(ApproveBlockProtocol.METRICS_APPROVAL_COUNTER_NAME) should be(None)

    cancelToken.cancel()
  }

  it should "not accept BlockApproved messages signed by not trusted validators" in {
    implicit val ctx         = TestScheduler()
    implicit val logStub     = new LogStub[Task]()
    implicit val metricsTest = new MetricsTestImpl[Task]()

    val (validatorSk, validatorPk) = Ed25519.newKeyPair
    val (invalidSk, invalidPk)     = Ed25519.newKeyPair
    val TestFixture(_, abp, candidate, _, sigsF) =
      ApproveBlockProtocolTest.createProtocol(10, 100.milliseconds, 1.millisecond, Set(validatorPk))
    val a = ApproveBlockProtocolTest.approval(candidate, invalidSk, invalidPk)

    val cancelToken = abp.run().start.runToFuture
    sigsF.get.unsafeRunSync.size should be(0)
    metricsTest.counters.get(ApproveBlockProtocol.METRICS_APPROVAL_COUNTER_NAME) should be(None)
    abp.addApproval(a).unsafeRunSync
    ctx.tick(1.millisecond)
    sigsF.get.unsafeRunSync.size should be(0)
    metricsTest.counters.get(ApproveBlockProtocol.METRICS_APPROVAL_COUNTER_NAME) should be(None)
    cancelToken.cancel()
  }

  private def infosContain[F[_]](start: String, size: Int)(
      implicit logStub: LogStub[F]
  ): Assertion =
    logStub.infos.filter(_.startsWith(start)).size should be(size)

  it should "send UnapprovedBlock message to peers at every interval" in {
    implicit val ctx               = TestScheduler()
    implicit val logStub           = new LogStub[Task]()
    implicit val metricsTest       = new MetricsTestImpl[Task]()
    val (validatorSk, validatorPk) = Ed25519.newKeyPair
    val TestFixture(_, abp, candidate, _, sigsF) =
      ApproveBlockProtocolTest.createProtocol(10, 100.milliseconds, 5.millisecond, Set(validatorPk))

    val cancelToken = abp.run().start.runToFuture

    // I know that testing the logs is not the best way but I comparing messages sent won't work
    // because we attach `System.currentMillis` to every message.
    ctx.tick(4.millisecond)
    infosContain("APPROVAL: Sent UnapprovedBlock", 1)
    infosContain("APPROVAL: received block approval from", 0)
    metricsTest.counters.get(ApproveBlockProtocol.METRICS_APPROVAL_COUNTER_NAME) should be(None)

    val a = ApproveBlockProtocolTest.approval(candidate, validatorSk, validatorPk)
    sigsF.get.unsafeRunSync.size should be(0)
    abp.addApproval(a).unsafeRunSync
    ctx.tick(1.millisecond)
    sigsF.get.unsafeRunSync.size should be(1)
    metricsTest.counters(ApproveBlockProtocol.METRICS_APPROVAL_COUNTER_NAME) should be(1)
    infosContain("APPROVAL: received block approval from", 1)
    infosContain("APPROVAL: Sent UnapprovedBlock", 2)
    cancelToken.cancel()
  }

  it should "send ApprovedBlock message to peers once an approved block is created" in {
    implicit val ctx               = TestScheduler()
    implicit val logStub           = new LogStub[Task]()
    implicit val metricsTest       = new MetricsTestImpl[Task]()
    val (validatorSk, validatorPk) = Ed25519.newKeyPair
    val TestFixture(_, abp, candidate, start, sigsF) =
      ApproveBlockProtocolTest.createProtocol(1, 2.milliseconds, 1.millisecond, Set(validatorPk))

    val startTime = start
    ctx.tick(startTime.milliseconds) // align clocks

    val cancelToken = abp.run().start.runToFuture
    ctx.tick()

    // I know that testing the logs is not the best way but I comparing messages sent won't work
    // because we attach `System.currentMillis` to every message.
    logStub.infos.filter(_.startsWith("APPROVAL: received block approval from")).size should be(0)
    logStub.infos.filter(_.startsWith("APPROVAL: Sent ApprovedBlock")).size should be(0)
    metricsTest.counters.get(ApproveBlockProtocol.METRICS_APPROVAL_COUNTER_NAME) should be(None)

    val a = ApproveBlockProtocolTest.approval(candidate, validatorSk, validatorPk)
    sigsF.get.unsafeRunSync.size should be(0)
    abp.addApproval(a).unsafeRunSync

    ctx.tick(1.millisecond)

    sigsF.get.unsafeRunSync.size should be(1)
    logStub.infos.filter(_.startsWith("APPROVAL: received block approval from")).size should be(1)
    metricsTest.counters(ApproveBlockProtocol.METRICS_APPROVAL_COUNTER_NAME) should be(1)
    ctx.tick(1.millisecond)
    logStub.infos.filter(_.startsWith("APPROVAL: Sent ApprovedBlock")).size should be(1)
    cancelToken.cancel()
  }
}

object ApproveBlockProtocolTest {
  def approval(
      c: ApprovedBlockCandidate,
      validatorSk: Array[Byte],
      validatorPk: Array[Byte]
  ): BlockApproval = {
    val sigData = Blake2b256.hash(c.toByteArray)
    val sig     = Ed25519.sign(sigData, validatorSk)
    BlockApproval(
      Some(c),
      Some(Signature(ByteString.copyFrom(validatorPk), "ed25519", ByteString.copyFrom(sig)))
    )
  }

  def invalidApproval(c: ApprovedBlockCandidate): BlockApproval = {
    val (sk, pk) = Ed25519.newKeyPair
    val sigData  = Blake2b256.hash(c.toByteArray ++ "wrong data".toArray.map(_.toByte))
    val sig      = Ed25519.sign(sigData, sk)
    BlockApproval(
      Some(c),
      Some(Signature(ByteString.copyFrom(pk), "ed25519", ByteString.copyFrom(sig)))
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
      validatorsPk: Set[Array[Byte]]
  )(implicit logStub: LogStub[Task], metrics: MetricsTestImpl[Task]): TestFixture = {
    implicit val time            = TestTime.instance
    implicit val transportLayer  = new TransportLayerStub[Task]
    val src: PeerNode            = peerNode("src", 40400)
    implicit val rpConfAsk       = createRPConfAsk[Task](src)
    implicit val ctx             = monix.execution.Scheduler.Implicits.global
    implicit val connectionsCell = Cell.mvarCell[Task, Connections](List(src)).unsafeRunSync
    implicit val lab             = LastApprovedBlock.unsafe[Task](None)

    val (sk, pk)   = Ed25519.newKeyPair
    val bonds      = HashSetCasperTest.createBonds(Seq(pk))
    val genesis    = HashSetCasperTest.createGenesis(bonds)
    val validators = validatorsPk.map(ByteString.copyFrom(_))
    val candidate  = ApprovedBlockCandidate(Some(genesis), requiredSigs)
    val sigs       = Ref.unsafe[Task, Set[Signature]](Set.empty)
    val startTime  = System.currentTimeMillis()

    val node = HashSetCasperTestNode.standalone(genesis, sk)
    val protocol = ApproveBlockProtocol
      .unsafe[Task](genesis, validators, requiredSigs, duration, interval, sigs, startTime)

    TestFixture(lab, protocol, candidate, startTime, sigs)
  }

  private def endpoint(port: Int): Endpoint = Endpoint("host", port, port)

  private def peerNode(name: String, port: Int): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), endpoint(port))
}
