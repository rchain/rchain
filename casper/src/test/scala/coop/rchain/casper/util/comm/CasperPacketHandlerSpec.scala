package coop.rchain.casper.util.comm

import cats.effect.concurrent.Ref
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.blockstorage.InMemBlockStore
import coop.rchain.casper.HashSetCasperTest.{buildGenesis, createBonds}
import coop.rchain.casper._
import coop.rchain.casper.genesis.contracts.Faucet
import coop.rchain.casper.helper.{BlockStoreTestFixture, NoOpsCasperEffect}
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
import coop.rchain.comm.protocol.routing.Packet
import coop.rchain.comm.rp.Connect.{Connections, ConnectionsCell}
import coop.rchain.comm.rp.ProtocolHelper, ProtocolHelper._
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
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.global
import monix.execution.schedulers.TestScheduler
import org.scalatest.WordSpec
import coop.rchain.casper.util.TestTime

import scala.concurrent.duration._

class CasperPacketHandlerSpec extends WordSpec {
  private def setup() = new {
    val scheduler      = Scheduler.io("test")
    val runtimeDir     = BlockStoreTestFixture.dbDir
    val activeRuntime  = Runtime.create(runtimeDir, 1024L * 1024)
    val runtimeManager = RuntimeManager.fromRuntime(activeRuntime)(scheduler)

    implicit val captureTask       = Capture.taskCapture
    val (genesisSk, genesisPk)     = Ed25519.newKeyPair
    val (validatorSk, validatorPk) = Ed25519.newKeyPair
    val bonds                      = createBonds(Seq(validatorPk))
    val requiredSigs               = 1
    val deployTimestamp            = 1L
    val genesis                    = buildGenesis(Seq.empty, bonds, 1L, Long.MaxValue, Faucet.noopFaucet, 1L)
    val validatorId                = ValidatorIdentity(validatorPk, validatorSk, "ed25519")
    val bap = new BlockApproverProtocol(
      validatorId,
      deployTimestamp,
      runtimeManager,
      bonds,
      Seq.empty,
      1L,
      Long.MaxValue,
      false,
      requiredSigs
    )(scheduler)
    val local: PeerNode = peerNode("src", 40400)
    val shardId         = "test-shardId"

    implicit val nodeDiscovery = new NodeDiscoveryStub[Task]
    implicit val connectionsCell: ConnectionsCell[Task] =
      Cell.unsafe[Task, Connections](List(local))
    implicit val transportLayer = new TransportLayerStub[Task]
    implicit val rpConf         = createRPConfAsk[Task](local)
    implicit val time           = TestTime.instance
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
      override def normalizedFaultTolerance(
          blockDag: BlockDag,
          estimateBlockHash: BlockHash
      ): Float = 1.0f
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
            new GenesisValidatorHandler(runtimeManager, validatorId, shardId, bap)
          )
        val packetHandler     = new CasperPacketHandlerImpl[Task](ref)
        val expectedCandidate = ApprovedBlockCandidate(Some(genesis), requiredSigs)

        val unapprovedBlock  = BlockApproverProtocolTest.createUnapproved(requiredSigs, genesis)
        val unapprovedPacket = BlockApproverProtocolTest.unapprovedToPacket(unapprovedBlock)
        val test = for {
          packetResponse <- packetHandler.handle(local).apply(unapprovedPacket)
          _              = assert(packetResponse.isEmpty)
          blockApproval  = BlockApproverProtocol.getBlockApproval(expectedCandidate, validatorId)
          expectedPacket = ProtocolHelper.packet(
            local,
            transport.BlockApproval,
            blockApproval.toByteString
          )
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

        val ref =
          Ref.unsafe[Task, CasperPacketHandlerInternal[Task]](
            new GenesisValidatorHandler(runtimeManager, validatorId, shardId, bap)
          )
        val packetHandler = new CasperPacketHandlerImpl[Task](ref)

        val approvedBlockRequest = ApprovedBlockRequest("test")
        val packet               = Packet(transport.ApprovedBlockRequest.id, approvedBlockRequest.toByteString)
        val test = for {
          packetResponse <- packetHandler.handle(local)(packet)
          _ = assert(
            packetResponse ==
              Some(
                Packet(
                  transport.NoApprovedBlockAvailable.id,
                  NoApprovedBlockAvailable("NoApprovedBlockAvailable", local.toString).toByteString
                )
              )
          )
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
          abp = ApproveBlockProtocol.unsafe[Task](
            genesis,
            Set(ByteString.copyFrom(validatorPk)),
            requiredSigns,
            duration,
            interval,
            sigs,
            startTime
          )
          standaloneCasper    = new StandaloneCasperHandler[Task](abp)
          refCasper           <- Ref.of[Task, CasperPacketHandlerInternal[Task]](standaloneCasper)
          casperPacketHandler = new CasperPacketHandlerImpl[Task](refCasper)
          c1                  = abp.run().forkAndForget.runToFuture
          c2 = StandaloneCasperHandler
            .approveBlockInterval(interval, shardId, runtimeManager, Some(validatorId), refCasper)
            .forkAndForget
            .runToFuture
          blockApproval = ApproveBlockProtocolTest.approval(
            ApprovedBlockCandidate(Some(genesis), requiredSigns),
            validatorSk,
            validatorPk
          )
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
          lastApprovedBlock <- LastApprovedBlock[Task].get
          _                 = assert(lastApprovedBlock.isDefined)
          _                 <- casperPacketHandler.handle(local)(approvedBlockRequestPacket)
          head              = transportLayer.requests.head
          _ = assert(
            ApprovedBlock
              .parseFrom(head.msg.message.packet.get.content.toByteArray) == lastApprovedBlock.get
          )
        } yield ()

        test.unsafeRunSync
      }
    }

    "in  BootstrapCasperHandler state" should {
      "make a transition to ApprovedBlockReceivedHandler once ApprovedBlock has been received" in {
        import monix.execution.Scheduler.Implicits.global
        val fixture = setup()
        import fixture._

        val validators = Set(ByteString.copyFrom(validatorPk))

        // interval and duration don't really matter since we don't require and signs from validators
        val bootstrapCasper =
          new BootstrapCasperHandler[Task](runtimeManager, shardId, Some(validatorId), validators)

        val approvedBlockCandidate = ApprovedBlockCandidate(block = Some(genesis))

        val approvedBlock: ApprovedBlock = ApprovedBlock(
          candidate = Some(approvedBlockCandidate),
          sigs = Seq(
            Signature(
              ByteString.copyFrom(validatorPk),
              "ed25519",
              ByteString.copyFrom(
                Ed25519.sign(Blake2b256.hash(approvedBlockCandidate.toByteArray), validatorSk)
              )
            )
          )
        )

        val approvedPacket = Packet(transport.ApprovedBlock.id, approvedBlock.toByteString)

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
          _                  <- casperPacketHandler.handle(local)(approvedBlockRequestPacket)
          head               = transportLayer.requests.head
          _                  = assert(head.msg.message.packet.get.content == approvedBlock.toByteString)
        } yield ()

        test.unsafeRunSync
      }
    }

    "in ApprovedBlockReceivedHandler state" should {
      import monix.execution.Scheduler.Implicits.global
      val fixture = setup()
      import fixture._

      val (_, validators)        = (1 to 4).map(_ => Ed25519.newKeyPair).unzip
      val bonds                  = HashSetCasperTest.createBonds(validators)
      val genesis                = HashSetCasperTest.createGenesis(bonds)
      val approvedBlockCandidate = ApprovedBlockCandidate(block = Some(genesis))
      val approvedBlock: ApprovedBlock = ApprovedBlock(
        candidate = Some(approvedBlockCandidate),
        sigs = Seq(
          Signature(
            ByteString.copyFrom(validatorPk),
            "ed25519",
            ByteString.copyFrom(
              Ed25519.sign(Blake2b256.hash(approvedBlockCandidate.toByteArray), validatorSk)
            )
          )
        )
      )

      val casper = NoOpsCasperEffect[Task]().unsafeRunSync

      val refCasper = Ref.unsafe[Task, CasperPacketHandlerInternal[Task]](
        new ApprovedBlockReceivedHandler[Task](casper, approvedBlock)
      )
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
        transportLayer.reset()
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
        transportLayer.reset()
      }

      "respond to ApprovedBlockRequest messages" in {
        val approvedBlockRequest = ApprovedBlockRequest("test")
        val requestPacket =
          Packet(transport.ApprovedBlockRequest.id, approvedBlockRequest.toByteString)

        val test: Task[Unit] = for {
          _    <- casperPacketHandler.handle(local)(requestPacket)
          head = transportLayer.requests.head
          _    = assert(head.peer == local)
          _ = assert(
            ApprovedBlock
              .parseFrom(head.msg.message.packet.get.content.toByteArray) == approvedBlock
          )
        } yield ()

        test.unsafeRunSync
        transportLayer.reset()
      }
    }
  }

}

object CasperPacketHandlerSpec {
  def approvedBlockRequestPacket: Packet = {
    val approvedBlockReq = ApprovedBlockRequest("test")
    Packet(transport.ApprovedBlockRequest.id, approvedBlockReq.toByteString)
  }

  private def endpoint(port: Int): Endpoint = Endpoint("host", port, port)

  private def peerNode(name: String, port: Int): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), endpoint(port))
}
