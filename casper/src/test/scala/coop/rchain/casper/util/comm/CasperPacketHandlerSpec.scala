package coop.rchain.casper.util.comm

import cats.effect.Timer
import cats.effect.concurrent.Ref
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.blockstorage.InMemBlockStore
import coop.rchain.casper.HashSetCasperTest.{buildGenesis, createBonds}
import coop.rchain.casper._
import coop.rchain.casper.helper.{BlockStoreTestFixture, HashSetCasperTestNode, NoOpsCasperEffect}
import coop.rchain.casper.protocol.{NoApprovedBlockAvailable, _}
import coop.rchain.casper.util.comm.CasperPacketHandler.{
  ApprovedBlockReceivedHandler,
  BootstrapCasperHandler,
  CasperPacketHandlerImpl,
  CasperPacketHandlerInternal,
  GenesisValidatorHandler,
  StandaloneCasperHandler
}
import coop.rchain.casper.util.comm.CasperPacketHandlerSpec._
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.catscontrib.{ApplicativeError_, Capture}
import coop.rchain.comm.protocol.rchain.Packet
import coop.rchain.comm.protocol.routing.Protocol
import coop.rchain.comm.rp.Connect.{Connections, ConnectionsCell}
import coop.rchain.comm.transport.CommMessages
import coop.rchain.comm.{transport, _}
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.metrics.Metrics.MetricsNOP
import coop.rchain.p2p.EffectsTestInstances._
import coop.rchain.p2p.effects.PacketHandler
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.shared.Cell
import monix.eval.Task
import monix.execution.schedulers.TestScheduler
import monix.execution.{CancelableFuture, Scheduler}
import org.scalatest.WordSpec
import CommMessages._

import scala.concurrent.duration._

class CasperPacketHandlerSpec extends WordSpec {
  private def setup() = new {
    val runtimeDir     = BlockStoreTestFixture.dbDir
    val activeRuntime  = Runtime.create(runtimeDir, 1024L * 1024)
    val runtimeManager = RuntimeManager.fromRuntime(activeRuntime)

    implicit val captureTask       = Capture.taskCapture
    val (genesisSk, genesisPk)     = Ed25519.newKeyPair
    val (validatorSk, validatorPk) = Ed25519.newKeyPair
    val bonds                      = createBonds(Seq(validatorPk))
    val requiredSigs               = 1
    val deployTimestamp            = 1L
    val genesis                    = buildGenesis(Seq.empty, bonds, 1L)
    val validatorId                = ValidatorIdentity(validatorPk, validatorSk, "ed25519")
    val scheduler                  = Scheduler.io("test")
    val bap = new BlockApproverProtocol(validatorId,
                                        deployTimestamp,
                                        runtimeManager,
                                        bonds,
                                        Seq.empty,
                                        requiredSigs)(scheduler)
    val local: PeerNode = peerNode("src", 40400)
    val shardId         = "test-shardId"

    implicit val nodeDiscovery = new NodeDiscoveryStub[Task]
    implicit val connectionsCell: ConnectionsCell[Task] =
      Cell.unsafe[Task, Connections](List(local))
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
    implicit val lab =
      LastApprovedBlock.of[Task].unsafeRunSync(monix.execution.Scheduler.Implicits.global)
    implicit val blockMap   = Ref.unsafe[Task, Map[BlockHash, BlockMessage]](Map.empty)
    implicit val blockStore = InMemBlockStore.create[Task]
    implicit val casperRef  = MultiParentCasperRef.unsafe[Task](None)
    implicit val safetyOracle = new SafetyOracle[Task] {
      override def normalizedFaultTolerance(blockDag: BlockDag,
                                            estimate: BlockMessage): Task[Float] =
        Task.pure(1.0f)
    }
  }

  "CasperPacketHandler" when {
    "in GenesisValidator state" should {

      "respond on UnapprovedBlock messages with BlockApproval" in {
        implicit val ctx = TestScheduler()
        val fixture      = setup()
        import fixture._

        val ref =
          Ref.unsafe[Task, CasperPacketHandlerInternal[Task]](
            new GenesisValidatorHandler(runtimeManager, validatorId, shardId, bap))
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
                                               blockApproval.toByteString)
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

        val runtimeDir     = BlockStoreTestFixture.dbDir
        val activeRuntime  = Runtime.create(runtimeDir, 1024L * 1024)
        val runtimeManager = RuntimeManager.fromRuntime(activeRuntime)

        val ref =
          Ref.unsafe[Task, CasperPacketHandlerInternal[Task]](
            new GenesisValidatorHandler(runtimeManager, validatorId, shardId, bap))
        val packetHandler = new CasperPacketHandlerImpl[Task](ref)

        val approvedBlockRequest = ApprovedBlockRequest("test")
        val packet               = Packet(transport.ApprovedBlockRequest.id, approvedBlockRequest.toByteString)
        val test = for {
          packetResponse <- packetHandler.handle(local)(packet)
          _ = assert(
            packetResponse ==
              Some(Packet(
                transport.NoApprovedBlockAvailable.id,
                NoApprovedBlockAvailable("NoApprovedBlockAvailable", local.toString).toByteString)))
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
        import monix.execution.Scheduler.Implicits.global
        val fixture = setup()
        import fixture._

        val runtimeDir     = BlockStoreTestFixture.dbDir
        val activeRuntime  = Runtime.create(runtimeDir, 1024L * 1024)
        val runtimeManager = RuntimeManager.fromRuntime(activeRuntime)

        val requiredSigns = 0
        // interval and duration don't really matter since we don't require and signs from validators
        val interval  = 1.millis
        val duration  = 1.second
        val startTime = System.currentTimeMillis()

        def waitUtilCasperIsDefined: Task[MultiParentCasper[Task]] =
          for {
            casperO <- MultiParentCasperRef[Task].get
            casper <- casperO match {
                       case None         => Task.sleep(3.seconds).flatMap(_ => waitUtilCasperIsDefined)
                       case Some(casper) => Task.pure(casper)
                     }
          } yield casper

        val test = for {
          sigs <- Ref.of[Task, Set[Signature]](Set.empty)
          abp = ApproveBlockProtocol.unsafe[Task](genesis,
                                                  Set(ByteString.copyFrom(validatorPk)),
                                                  requiredSigns,
                                                  duration,
                                                  interval,
                                                  sigs,
                                                  startTime)
          standaloneCasper    = new StandaloneCasperHandler[Task](abp)
          refCasper           <- Ref.of[Task, CasperPacketHandlerInternal[Task]](standaloneCasper)
          casperPacketHandler = new CasperPacketHandlerImpl[Task](refCasper)
          c1                  = abp.run().forkAndForget.runAsync
          c2 = StandaloneCasperHandler
            .approveBlockInterval(interval, shardId, runtimeManager, Some(validatorId), refCasper)
            .forkAndForget
            .runAsync
          blockApproval = ApproveBlockProtocolTest.approval(ApprovedBlockCandidate(Some(genesis),
                                                                                   requiredSigns),
                                                            validatorSk,
                                                            validatorPk)
          blockApprovalPacket = Packet(transport.BlockApproval.id, blockApproval.toByteString)
          _                   <- casperPacketHandler.handle(local)(blockApprovalPacket)
          //wait until casper is defined, with 1 minute timeout (indicating failure)
          possiblyCasper  <- Task.racePair(Task.sleep(1.minute), waitUtilCasperIsDefined)
          _               = assert(possiblyCasper.isRight)
          blockO          <- blockStore.get(genesis.blockHash)
          _               = assert(blockO.isDefined)
          _               = assert(blockO.contains(genesis))
          handlerInternal <- refCasper.get
          _               = assert(handlerInternal.isInstanceOf[ApprovedBlockReceivedHandler[Task]])
          // assert that we really serve last approved block
          lastApprovedBlockO <- LastApprovedBlock[Task].get
          _                  = assert(lastApprovedBlockO.isDefined)
          approvedPacket = approvedBlockPacket
          approvedBlockRes <- casperPacketHandler.handle(local)(approvedBlockPacket)
          _ = assert(
            approvedBlockRes.map(p => ApprovedBlock.parseFrom(p.content.toByteArray)) == Some(
              lastApprovedBlockO.get))
        } yield ()

        test.unsafeRunSync
      }
    }

    "in  BootstrapCasperHandler state" should {
      "query peers sequentially with ApprovedBlockRequest" in {
        implicit val ctx = TestScheduler()
        val fixture      = setup()
        import fixture._

        val request       = ApprovedBlockRequest("PleaseSendMeAnApprovedBlock").toByteString
        val requestPacket = CommMessages.packet(local, transport.ApprovedBlockRequest, request)

        val peer1 = peerNode("peerNode1", 1)
        val peer2 = peerNode("peerNode2", 2)

        transportLayer.setResponses(_ => p => Right(p))
        connectionsCell.modify(_ => Task.now(List(peer1, peer2))).unsafeRunSync
        ctx.tick()
        val peers = connectionsCell.read.unsafeRunSync
        assert(peers.size == 2)

        implicit val lab = LastApprovedBlock.unsafe[Task]()
        implicit val ph = new PacketHandler[Task] {
          override def handlePacket(peer: PeerNode, packet: Packet): Task[Option[Packet]] =
            Task.now(None: Option[Packet])
        }

        def assertSent(sentOnPeers: List[PeerNode]): Unit =
          transportLayer.requests.zipWithIndex.foreach {
            case (request, idx) =>
              assert(request.msg == requestPacket)
              assert(request.peer == sentOnPeers(idx))
          }

        val interval = 1.second
        val test = for {
          _ <- CommUtil.requestApprovedBlock[Task](interval).forkAndForget
          _ = ctx.tick()
          _ = assertSent(List(peer1, peer2))
          _ = ctx.tick(interval)
          _ = assertSent(List(peer1, peer2, peer1, peer2))
        } yield ()

        test.unsafeRunSync
      }

      "make a transition to ApprovedBlockReceivedHandler once ApprovedBlock has been received" in {
        import monix.execution.Scheduler.Implicits.global
        val fixture = setup()
        import fixture._

        val runtimeDir     = BlockStoreTestFixture.dbDir
        val activeRuntime  = Runtime.create(runtimeDir, 1024L * 1024)
        val runtimeManager = RuntimeManager.fromRuntime(activeRuntime)
        val validators     = Set(ByteString.copyFrom(validatorPk))

        // interval and duration don't really matter since we don't require and signs from validators
        val bootstrapCasper =
          new BootstrapCasperHandler[Task](runtimeManager, shardId, Some(validatorId), validators)
        val genesis = HashSetCasperTest.createGenesis(Map.empty)

        val approvedBlockCandidate = ApprovedBlockCandidate(block = Some(genesis))

        val approvedBlock: ApprovedBlock = ApprovedBlock(
          candidate = Some(approvedBlockCandidate),
          sigs = Seq(
            Signature(
              ByteString.copyFrom(validatorPk),
              "ed25519",
              ByteString.copyFrom(
                Ed25519.sign(Blake2b256.hash(approvedBlockCandidate.toByteArray), validatorSk))))
        )
        val approvedPacket = approvedBlockPacket

        val test = for {
          refCasper           <- Ref.of[Task, CasperPacketHandlerInternal[Task]](bootstrapCasper)
          casperPacketHandler = new CasperPacketHandlerImpl[Task](refCasper)
          _                   <- casperPacketHandler.handle(local).apply(approvedPacket)
          casperO             <- MultiParentCasperRef[Task].get
          _                   = assert(casperO.isDefined)
          blockO              <- blockStore.get(genesis.blockHash)
          _                   = assert(blockO.isDefined)
          _                   = assert(blockO.contains(genesis))
          handlerInternal     <- refCasper.get
          _                   = assert(handlerInternal.isInstanceOf[ApprovedBlockReceivedHandler[Task]])
          // assert that we really serve last approved block
          lastApprovedBlockO <- LastApprovedBlock[Task].get
          _                  = assert(lastApprovedBlockO.isDefined)
          approvedBlockRes <- casperPacketHandler.handle(local)(approvedPacket)
          _ = assert(
            approvedBlockRes.map(p => ApprovedBlock.parseFrom(p.content.toByteArray)) == Some(
              lastApprovedBlockO.get))
        } yield ()

        test.unsafeRunSync
      }

      "stop querying peers once ApprovedBlock has been received" in {
        implicit val ctx = TestScheduler()
        val fixture      = setup()
        import fixture._

        transportLayer.setResponses(_ => p => Right(p))

        implicit val ph = new PacketHandler[Task] {
          override def handlePacket(peer: PeerNode, packet: Packet): Task[Option[Packet]] =
            Task.now(None: Option[Packet])
        }

        implicit val lab = LastApprovedBlock.unsafe[Task]()
        val interval     = 1.second

        assert(transportLayer.requests.size == 0)
        val test = for {
          _ <- CommUtil.requestApprovedBlock[Task](interval).forkAndForget
          _ = ctx.tick()
          _ = assert(transportLayer.requests.size == 1)
          _ = ctx.tick(interval)
          _ = assert(transportLayer.requests.size == 2)
          _ <- lab.set(ApprovedBlock())
          _ = ctx.tick(interval)
          _ = assert(transportLayer.requests.size == 2)
          _ = ctx.tick(interval)
          _ = assert(transportLayer.requests.size == 2)
        } yield ()

        test.unsafeRunSync
      }
    }

    "in ApprovedBlockReceivedHandler state" should {
      import monix.execution.Scheduler.Implicits.global
      val fixture = setup()
      import fixture._

      val runtimeDir     = BlockStoreTestFixture.dbDir
      val activeRuntime  = Runtime.create(runtimeDir, 1024L * 1024)
      val runtimeManager = RuntimeManager.fromRuntime(activeRuntime)

      val genesis                = HashSetCasperTest.createGenesis(Map.empty)
      val approvedBlockCandidate = ApprovedBlockCandidate(block = Some(genesis))
      val approvedBlock: ApprovedBlock = ApprovedBlock(
        candidate = Some(approvedBlockCandidate),
        sigs = Seq(
          Signature(
            ByteString.copyFrom(validatorPk),
            "ed25519",
            ByteString.copyFrom(
              Ed25519.sign(Blake2b256.hash(approvedBlockCandidate.toByteArray), validatorSk))))
      )

      val casper = new NoOpsCasperEffect[Task]()

      val refCasper = Ref.unsafe[Task, CasperPacketHandlerInternal[Task]](
        new ApprovedBlockReceivedHandler[Task](casper, approvedBlock))
      val casperPacketHandler = new CasperPacketHandlerImpl[Task](refCasper)

      transportLayer.setResponses(_ => p => Right(p))

      "respond to BlockMessage messages " in {
        val blockMessage = BlockMessage(ByteString.copyFrom("Test BlockMessage", "UTF-8"))
        val packet       = Packet(transport.BlockMessage.id, blockMessage.toByteString)
        val test: Task[Unit] = for {
          _ <- casperPacketHandler.handle(local)(packet)
          _ = assert(casper.store.contains(blockMessage.blockHash))
        } yield ()

        test.unsafeRunSync
      }

      "respond to BlockRequest messages" in {
        val blockRequest =
          BlockRequest(Base16.encode(genesis.blockHash.toByteArray), genesis.blockHash)
        val requestPacket = Packet(transport.BlockRequest.id, blockRequest.toByteString)
        val test = for {
          _     <- blockStore.put(genesis.blockHash, genesis)
          _     <- casperPacketHandler.handle(local)(requestPacket)
          head  = transportLayer.requests.head
          block = packet(local, transport.BlockMessage, genesis.toByteString)
          _     = assert(head.peer == local && head.msg == block)
        } yield ()

        test.unsafeRunSync
      }

      "respond to ApprovedBlockRequest messages" in {
        val approvedBlockRequest = ApprovedBlockRequest("test")
        val requestPacket =
          Packet(transport.ApprovedBlockRequest.id, approvedBlockRequest.toByteString)

        val test: Task[Unit] = for {
          response <- casperPacketHandler.handle(local)(requestPacket)
          block    = Packet(transport.ApprovedBlock.id, approvedBlock.toByteString)
          _        = assert(response == Some(block))
        } yield ()

        test.unsafeRunSync
      }
    }
  }

}

object CasperPacketHandlerSpec {
  def assertValue[A](a: CancelableFuture[A]): A = {
    assert(a.value.isDefined && a.value.get.isSuccess)
    a.value.get.get
  }

  def approvedBlockPacket: Packet = {
    val approvedBlockReq   = ApprovedBlockRequest("test")
    Packet(transport.ApprovedBlockRequest.id, approvedBlockReq.toByteString)
  }


  private def endpoint(port: Int): Endpoint = Endpoint("host", port, port)

  private def peerNode(name: String, port: Int): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), endpoint(port))
}
