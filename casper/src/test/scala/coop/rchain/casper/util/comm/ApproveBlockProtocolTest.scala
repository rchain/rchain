package coop.rchain.casper.util.comm

import coop.rchain.comm.rp.Connect, Connect._
import coop.rchain.shared._
import com.google.protobuf.ByteString
import coop.rchain.casper.HashSetCasperTest
import coop.rchain.casper.helper.HashSetCasperTestNode
import coop.rchain.casper.protocol._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.catscontrib._
import coop.rchain.comm.{Endpoint, NodeIdentifier, PeerNode}
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.p2p.EffectsTestInstances._
import coop.rchain.shared.Log.NOPLog
import coop.rchain.shared.Time
import monix.eval.Task
import monix.execution.schedulers.TestScheduler
import scala.concurrent.duration._
import org.scalatest.{FlatSpec, Matchers}

class ApproveBlockProtocolTest extends FlatSpec with Matchers {
  "ApproveBlockProtocol" should "add valid signatures it receives to its state" in {
    implicit val time = new LogicalTime[Task]()
    implicit val ctx  = TestScheduler()
    val abp           = ApproveBlockProtocolTest.createProtocol(10, 100.milliseconds, 1.millisecond)
    val a             = ApproveBlockProtocolTest.approval(abp.candidate)

    val cancelToken = abp.run().fork.runAsync
    abp.currentSigs.unsafeRunSync.size should be(0)
    abp.addApproval(a).unsafeRunSync
    ctx.tick(1.millisecond)
    abp.currentSigs.unsafeRunSync.size should be(1)
    cancelToken.cancel()
  }

  it should "not change the number of signatures in its state if the same one is given multiple times" in {
    implicit val time = new LogicalTime[Task]()
    implicit val ctx  = TestScheduler()
    val abp           = ApproveBlockProtocolTest.createProtocol(10, 100.milliseconds, 1.millisecond)
    val a             = ApproveBlockProtocolTest.approval(abp.candidate)

    val cancelToken = abp.run().fork.runAsync
    abp.currentSigs.unsafeRunSync.size should be(0)
    abp.addApproval(a).unsafeRunSync
    ctx.tick(1.millisecond)
    abp.currentSigs.unsafeRunSync.size should be(1)
    abp.addApproval(a).unsafeRunSync
    ctx.tick(1.millisecond)
    abp.currentSigs.unsafeRunSync.size should be(1)
    cancelToken.cancel()
  }

  it should "not add invalid signatures it receives to its state" in {
    implicit val time = new LogicalTime[Task]()
    implicit val ctx  = TestScheduler()
    val abp           = ApproveBlockProtocolTest.createProtocol(10, 100.milliseconds, 1.millisecond)
    val a             = ApproveBlockProtocolTest.invalidApproval(abp.candidate)

    val cancelToken = abp.run().fork.runAsync
    ctx.tick(1.millisecond)
    abp.currentSigs.unsafeRunSync.size should be(0)
    abp.addApproval(a).unsafeRunSync
    ctx.tick(1.millisecond)
    abp.currentSigs.unsafeRunSync.size should be(0)
    cancelToken.cancel()
  }

  it should "create an approved block if at least the correct number of signatures is collected after the duration has elapsed" in {
    val n: Int            = 10
    val d: FiniteDuration = 30.milliseconds
    implicit val time     = new LogicalTime[Task]()
    implicit val ctx      = TestScheduler()
    implicit val abp =
      ApproveBlockProtocolTest.createProtocol(n, duration = d, interval = 1.millisecond)
    val c = abp.candidate

    val startTime = abp.start.milliseconds
    ctx.tick(abp.start.milliseconds) //align clocks
    val cancelToken = abp.run().fork.runAsync

    val sigs = (1 to n).map { _ =>
      val blockApproval = ApproveBlockProtocolTest.approval(c)
      abp.addApproval(blockApproval).unsafeRunSync
      ctx.tick(1.millisecond)
      blockApproval.sig.get
    }

    val expectedApproved = coop.rchain.casper.protocol.ApprovedBlock(Some(c), sigs)
    ctx.tick(21.milliseconds)
    abp.approvedBlock.runAsync.value.nonEmpty should be(true)
    abp.approvedBlock.runAsync.value.get should be('success)
    ctx.clockMonotonic(MILLISECONDS) should be(abp.start + d.toMillis + 1)
    cancelToken.cancel()
  }

  it should "continue collecting signatures if not enough are collected after the duration has elapsed" in {
    val n: Int            = 10
    val d: FiniteDuration = 30.milliseconds
    implicit val time     = new LogicalTime[Task]()
    implicit val ctx      = TestScheduler()
    implicit val abp =
      ApproveBlockProtocolTest.createProtocol(n, duration = d, interval = 1.millisecond)
    val c = abp.candidate

    val startTime = abp.start
    ctx.tick(startTime.milliseconds) // align clocks

    val cancelToken = abp.run().fork.runAsync

    (1 to (n / 2)).foreach { _ =>
      abp.addApproval(ApproveBlockProtocolTest.approval(c)).unsafeRunSync
      ctx.tick(1.millisecond)
    }

    abp.approvedBlock.runAsync.value.nonEmpty should be(false)

    (1 to (n / 2 + 1)).foreach { _ =>
      abp.addApproval(ApproveBlockProtocolTest.approval(c)).unsafeRunSync
      ctx.tick(1.millisecond)
    }

    val timeElapsed = ctx.clockMonotonic(MILLISECONDS) - startTime
    // since we started at `startTime` and we advanced internal clock `n` times x 1 millisecond
    // we still have to advance internal clock with missing milliseconds
    ctx.tick((d.toMillis - timeElapsed).milliseconds)
    abp.approvedBlock.runAsync.value.nonEmpty should be(true)
    abp.approvedBlock.runAsync.value.get shouldBe 'success

    cancelToken.cancel()
  }

  it should "skip the duration and create and approved block immediately if the required signatures is zero" in {
    val d: FiniteDuration = 30.milliseconds
    implicit val time     = new LogicalTime[Task]()
    implicit val ctx      = TestScheduler()
    implicit val abp =
      ApproveBlockProtocolTest.createProtocol(0, duration = d, interval = 1.millisecond)

    val startTime = abp.start
    ctx.tick(startTime.milliseconds) // align clocks

    val cancelToken = abp.run().fork.runAsync
    ctx.tick()

    abp.approvedBlock.runAsync.value.nonEmpty should be(true)
    abp.approvedBlock.runAsync.value.get shouldBe 'success

    cancelToken.cancel()
  }
}

object ApproveBlockProtocolTest {
  def approval(c: ApprovedBlockCandidate): BlockApproval = {
    val (sk, pk) = Ed25519.newKeyPair
    val sigData  = Blake2b256.hash(c.toByteArray)
    val sig      = Ed25519.sign(sigData, sk)
    BlockApproval(Some(c),
                  Some(Signature(ByteString.copyFrom(pk), "ed25519", ByteString.copyFrom(sig))))
  }

  def invalidApproval(c: ApprovedBlockCandidate): BlockApproval = {
    val (sk, pk) = Ed25519.newKeyPair
    val sigData  = Blake2b256.hash(c.toByteArray ++ "wrong data".toArray.map(_.toByte))
    val sig      = Ed25519.sign(sigData, sk)
    BlockApproval(Some(c),
                  Some(Signature(ByteString.copyFrom(pk), "ed25519", ByteString.copyFrom(sig))))
  }

  def createProtocol(requiredSigs: Int, duration: FiniteDuration, interval: FiniteDuration)(
      implicit time: Time[Task]): ApproveBlockProtocol[Task] = {
    implicit val connectionsCell = Cell.const[Task, Connections](Connect.Connections.empty)
    implicit val nopLog          = new NOPLog[Task]()
    val src: PeerNode            = peerNode("src", 40400)
    implicit val transportLayer  = new TransportLayerStub[Task]
    implicit val rpConfAsk       = createRPConfAsk[Task](src)
    implicit val ctx             = monix.execution.Scheduler.Implicits.global

    val (sk, pk) = Ed25519.newKeyPair
    val genesis  = HashSetCasperTest.createGenesis(Seq(pk))

    val node = HashSetCasperTestNode.standalone(genesis, sk)
    ApproveBlockProtocol.create[Task](genesis, requiredSigs, duration, interval).unsafeRunSync
  }

  private def endpoint(port: Int): Endpoint = Endpoint("host", port, port)

  private def peerNode(name: String, port: Int): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), endpoint(port))
}
