package coop.rchain.casper.util.comm

import cats.effect.concurrent.Ref
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.blockstorage.InMemBlockStore
import coop.rchain.casper._
import coop.rchain.casper.HashSetCasperTest.createGenesis
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.comm.CasperPacketHandler.{
  ApprovedBlockReceivedHandler,
  BootstrapCasperHandler,
  CasperPacketHandlerImpl,
  CasperPacketHandlerInternal,
  GenesisValidatorHandler,
  StandaloneCasperHandler
}
import coop.rchain.casper.protocol.NoApprovedBlockAvailable
import coop.rchain.casper.util.comm.CasperPacketHandlerSpec._
import coop.rchain.catscontrib.{ApplicativeError_, Capture, TaskContrib}
import coop.rchain.comm.transport.{CommMessages}
import coop.rchain.comm._
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.metrics.Metrics.MetricsNOP
import coop.rchain.p2p.EffectsTestInstances._
import monix.eval.Task
import TaskContrib._
import com.google.protobuf.ByteString
import coop.rchain.casper.helper.BlockStoreTestFixture
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.comm.protocol.rchain.Packet
import monix.execution.schedulers.TestScheduler
import org.scalatest.{Assertion, WordSpec}
import coop.rchain.comm.transport
import coop.rchain.rholang.interpreter.Runtime
import monix.execution.CancelableFuture

import scala.concurrent.duration._
import scala.util.Success

class CasperPacketHandlerSpec extends WordSpec {
  private def setup() = new {
    implicit val captureTask       = Capture.taskCapture
    val (genesisSk, genesisPk)     = Ed25519.newKeyPair
    val genesis                    = createGenesis(Seq(genesisSk))
    val requiredSigs               = 1
    val (validatorSk, validatorPk) = Ed25519.newKeyPair
    val validatorId                = ValidatorIdentity(validatorPk, validatorSk, "ed25519")
    val bap                        = new BlockApproverProtocol(validatorId, genesis, requiredSigs)
    val local: PeerNode            = peerNode("src", 40400)

    implicit val nodeDiscovery  = new NodeDiscoveryStub[Task]
    implicit val transportLayer = new TransportLayerStub[Task]
    implicit val rpConf         = createRPConfAsk[Task](local)
    implicit val time           = new LogicalTime[Task]
    implicit val log            = new LogStub[Task]
    implicit val errHandler = new ApplicativeError_[Task, CommError] {
      override def raiseError[A](e: CommError): Task[A] =
        Task.raiseError(new Exception(s"CommError: $e"))
      override def handleErrorWith[A](fa: Task[A])(f: CommError => Task[A]): Task[A] =
        fa.onErrorHandleWith(th => f(UnknownCommError(th.getMessage)))
    }
    implicit val metrics = new MetricsNOP[Task]
    implicit val lab = new TaskOps(LastApprovedBlock.of[Task])(
      monix.execution.Scheduler.Implicits.global).unsafeRunSync
    implicit val blockMap   = Ref.unsafe[Task, Map[BlockHash, BlockMessage]](Map.empty)
    implicit val blockStore = InMemBlockStore.create[Task]
    implicit val casperRef  = MultiParentCasperRef.unsafe[Task]
  }

  "CasperPacketHandler" when {
    "in GenesisValidator state" should {

      "respond on UnapprovedBlock messages with BlockApproval" in {
        implicit val ctx = TestScheduler()
        val fixture      = setup()
        import fixture._

        val ref =
          Ref.unsafe[Task, CasperPacketHandlerInternal[Task]](new GenesisValidatorHandler(bap))
        val packetHandler     = new CasperPacketHandlerImpl[Task](ref)
        val expectedCandidate = ApprovedBlockCandidate(Some(genesis), requiredSigs)

        val unapprovedBlock  = BlockApproverProtocolTest.createUnapproved(requiredSigs, genesis)
        val unapprovedPacket = BlockApproverProtocolTest.unapprovedToPacket(unapprovedBlock)
        val test = for {
          packetResponse <- packetHandler.handle(local).apply(unapprovedPacket)
          _              = assert(packetResponse.isEmpty)
          blockApproval  = BlockApproverProtocol.getBlockApproval(expectedCandidate, validatorId)
          expectedPacket = CommMessages.packet(local,
                                               transport.BlockApproval,
                                               blockApproval.toByteString,
                                               time.clock)
          _ = {
            val lastMessage = transportLayer.requests.last
            assert(lastMessage.peer == local && lastMessage.msg == expectedPacket)
          }
        } yield ()
        test.unsafeRunSync
        ctx.tick()
      }

      "should not respond to any other message" in {
        implicit val ctx = TestScheduler()
        val fixture      = setup()
        import fixture._

        val requiredSigns = 1
        val bap           = new BlockApproverProtocol(validatorId, genesis, requiredSigns)

        val ref =
          Ref.unsafe[Task, CasperPacketHandlerInternal[Task]](new GenesisValidatorHandler(bap))
        val packetHandler = new CasperPacketHandlerImpl[Task](ref)

        val approvedBlockRequest = ApprovedBlockRequest("test")
        val packet               = Packet(transport.ApprovedBlockRequest.id, approvedBlockRequest.toByteString)
        val test = for {
          packetResponse <- packetHandler.handle(local)(packet)
          _ = assert(
            packetResponse ==
              Some(NoApprovedBlockAvailable(transport.NoApprovedBlockAvailable.id, local.toString)))
          _               = assert(transportLayer.requests.isEmpty)
          blockRequest    = BlockRequest("base16Hash", ByteString.copyFromUtf8("base16Hash"))
          packet2         = Packet(transport.BlockRequest.id, blockRequest.toByteString)
          packetResponse2 <- packetHandler.handle(local)(packet2)
          _               = assert(packetResponse2.isEmpty)
          _               = assert(transportLayer.requests.isEmpty)
        } yield ()
        test.unsafeRunSync
        ctx.tick()
      }
    }

    "in StandaloneCasperHandler state" should {
      "make a transition to ApprovedBlockReceivedHandler state after block has been approved" in {
        implicit val ctx = TestScheduler()
        val fixture      = setup()
        import fixture._

        val runtimeDir     = BlockStoreTestFixture.dbDir
        val activeRuntime  = Runtime.create(runtimeDir, 1024L * 1024)
        val runtimeManager = RuntimeManager.fromRuntime(activeRuntime)

        val requiredSigns = 1
        val interval      = 100.millis
        val duration      = 1.second
        val startTime     = System.currentTimeMillis()
        ctx.tick(startTime.milliseconds) //align clocks

        val sigs = Ref.unsafe[Task, Set[Signature]](Set.empty)

        implicit val abp = ApproveBlockProtocol.unsafe[Task](genesis,
                                                             Set(ByteString.copyFrom(validatorPk)),
                                                             requiredSigns,
                                                             duration,
                                                             interval,
                                                             sigs,
                                                             startTime)
        implicit val safetyOracle = new SafetyOracle[Task] {
          override def normalizedFaultTolerance(blockDag: BlockDag,
                                                estimate: BlockMessage): Task[Float] =
            Task.pure(1.0f)
        }

        val standaloneCasper =
          new StandaloneCasperHandler[Task](abp)

        val refCasper =
          Ref.unsafe[Task, CasperPacketHandlerInternal[Task]](standaloneCasper)

        val casperPacketHandler = new CasperPacketHandlerImpl[Task](refCasper)

        val c1 = abp.run().forkAndForget.runAsync
        val c2 = StandaloneCasperHandler
          .approveBlockInterval(interval, runtimeManager, Some(validatorId), refCasper)
          .forkAndForget
          .runAsync
        val blockApproval =
          ApproveBlockProtocolTest.approval(ApprovedBlockCandidate(Some(genesis), requiredSigns),
                                            validatorSk,
                                            validatorPk)
        val blockApprovalPacket = Packet(transport.BlockApproval.id, blockApproval.toByteString)
        val packetRes           = casperPacketHandler.handle(local)(blockApprovalPacket).runAsync
        ctx.tick()
        assert(packetRes.value == Some(Success(None)))
        ctx.tick(duration + interval)
        val Some(casper) = assertValue(MultiParentCasperRef[Task].get.runAsync)

        val block = assertValue(blockStore.get(genesis.blockHash).runAsync)
        assert(block == Some(genesis))
        val handlerInternal = assertValue(refCasper.get.runAsync)
        assert(handlerInternal.isInstanceOf[ApprovedBlockReceivedHandler[Task]])

        val Some(approvedBlock) =
          assertValue[Option[ApprovedBlock]](LastApprovedBlock[Task].get.runAsync)

        val approvedBlockReq = ApprovedBlockRequest("test")
        val approvedBlockPacket =
          Packet(transport.ApprovedBlockRequest.id, approvedBlockReq.toByteString)
        val approvedBlockReqRes = casperPacketHandler.handle(local)(approvedBlockPacket).runAsync
        ctx.tick()
        val res = assertValue(approvedBlockReqRes)
        assert(res.isDefined)
        assert(ApprovedBlock.parseFrom(res.get.content.toByteArray) == approvedBlock)
      }
    }

    "in  BootstrapCasperHandler state" should {
      "query peers sequentially with ApprovedBlockRequest" in {
        implicit val ctx = TestScheduler()
        val fixture      = setup()
        import fixture._

        val runtimeDir     = BlockStoreTestFixture.dbDir
        val activeRuntime  = Runtime.create(runtimeDir, 1024L * 1024)
        val runtimeManager = RuntimeManager.fromRuntime(activeRuntime)
        val validators     = Set(ByteString.copyFrom(validatorPk))

        implicit val safetyOracle = new SafetyOracle[Task] {
          override def normalizedFaultTolerance(blockDag: BlockDag,
                                                estimate: BlockMessage): Task[Float] =
            Task.pure(1.0f)
        }

        val bootstrap = Ref.unsafe[Task, CasperPacketHandlerInternal[Task]](
          new BootstrapCasperHandler[Task](runtimeManager, Some(validatorId), validators)
        )
        pending
      }

      "make a transition to ApprovedBlockReceivedHandler once ApprovedBlock has beeen received" in {
        pending
      }

      "stop querying peers once ApprovedBlock has been received" in {
        pending
      }
    }

    "in ApprovedBlockReceivedHandler state" should {
      "respond to BlockMessage messages " in {
        pending
      }

      "respond to BlockRequest messages" in {
        pending
      }

      "respond to ApprovedBlockRequest messages" in {
        pending
      }
    }
  }

}

object CasperPacketHandlerSpec {
  def assertValue[A](a: CancelableFuture[A]): A = {
    assert(a.value.isDefined && a.value.get.isSuccess)
    a.value.get.get
  }

  private def endpoint(port: Int): Endpoint = Endpoint("host", port, port)

  private def peerNode(name: String, port: Int): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), endpoint(port))
}
