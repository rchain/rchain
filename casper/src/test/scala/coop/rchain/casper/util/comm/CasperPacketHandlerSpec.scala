package coop.rchain.casper.util.comm

import cats.effect.concurrent.Ref
import cats.effect.{Concurrent, ContextShift}
import cats._, cats.data._, cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.blockstorage.{BlockDagRepresentation, InMemBlockDagStorage, InMemBlockStore}
import coop.rchain.casper.MultiParentCasperTestUtil.createBonds
import coop.rchain.casper._
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.genesis.contracts._
import coop.rchain.casper.engine._, EngineCell._
import coop.rchain.casper.helper.{BlockDagStorageTestFixture, NoOpsCasperEffect}
import coop.rchain.casper.protocol.{NoApprovedBlockAvailable, _}
import coop.rchain.casper.util.TestTime
import coop.rchain.casper.util.comm.CasperPacketHandlerSpec._
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.catscontrib.ApplicativeError_
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.comm.protocol.routing.Packet
import coop.rchain.comm.rp.Connect.{Connections, ConnectionsCell}
import coop.rchain.comm.rp.ProtocolHelper
import coop.rchain.comm.rp.ProtocolHelper._
import coop.rchain.comm.{transport, _}
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.metrics.Metrics.MetricsNOP
import coop.rchain.p2p.EffectsTestInstances._
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.rholang.interpreter.util.RevAddress
import coop.rchain.shared.{Cell, StoreType}
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.WordSpec

import scala.concurrent.duration._

class CasperPacketHandlerSpec extends WordSpec {
  private def setup() = new {
    implicit val log     = new LogStub[Task]
    implicit val metrics = new MetricsNOP[Task]
    val networkId        = "test"
    val scheduler        = Scheduler.io("test")
    val runtimeDir       = BlockDagStorageTestFixture.blockStorageDir
    val activeRuntime =
      Runtime
        .createWithEmptyCost[Task, Task.Par](runtimeDir, 3024L * 1024, StoreType.LMDB)(
          ContextShift[Task],
          Concurrent[Task],
          log,
          metrics,
          Parallel[Task, Task.Par],
          scheduler
        )
        .unsafeRunSync(scheduler)

    val runtimeManager = RuntimeManager.fromRuntime(activeRuntime).unsafeRunSync(scheduler)

    val (genesisSk, genesisPk)     = Ed25519.newKeyPair
    val (validatorSk, validatorPk) = Ed25519.newKeyPair
    val bonds                      = createBonds(Seq(validatorPk))
    val requiredSigs               = 1
    val shardId                    = "test-shardId"
    val deployTimestamp            = 1L
    val genesis: BlockMessage =
      MultiParentCasperTestUtil.buildGenesis(
        Genesis(
          shardId = shardId,
          timestamp = deployTimestamp,
          wallets = Seq.empty[PreWallet],
          proofOfStake = ProofOfStake(
            minimumBond = 0L,
            maximumBond = Long.MaxValue,
            validators = bonds.map(Validator.tupled).toSeq
          ),
          faucet = false,
          genesisPk = genesisPk,
          vaults = bonds.toList.map {
            case (pk, stake) =>
              RevAddress.fromPublicKey(pk).map(Vault(_, stake))
          }.flattenOption,
          supply = Long.MaxValue
        )
      )
    val validatorId = ValidatorIdentity(validatorPk, validatorSk, "ed25519")
    val bap = new BlockApproverProtocol(
      validatorId,
      deployTimestamp,
      bonds,
      Seq.empty,
      1L,
      Long.MaxValue,
      false,
      requiredSigs
    )
    val local: PeerNode = peerNode("src", 40400)

    implicit val nodeDiscovery = new NodeDiscoveryStub[Task]
    implicit val connectionsCell: ConnectionsCell[Task] =
      Cell.unsafe[Task, Connections](List(local))
    implicit val transportLayer = new TransportLayerStub[Task]
    implicit val rpConf         = createRPConfAsk[Task](local)
    implicit val time           = TestTime.instance
    implicit val errHandler =
      ApplicativeError_.applicativeError(new ApplicativeError[Task, CommError] {
        override def raiseError[A](e: CommError): Task[A] =
          Task.raiseError(new Exception(s"CommError: $e"))
        override def handleErrorWith[A](fa: Task[A])(f: CommError => Task[A]): Task[A] =
          fa.onErrorHandleWith(th => f(UnknownCommError(th.getMessage)))
        override def pure[A](x: A): Task[A]                           = Task.pure(x)
        override def ap[A, B](ff: Task[A => B])(fa: Task[A]): Task[B] = Applicative[Task].ap(ff)(fa)
      })
    implicit val lab =
      LastApprovedBlock.of[Task].unsafeRunSync(monix.execution.Scheduler.Implicits.global)
    implicit val blockMap         = Ref.unsafe[Task, Map[BlockHash, BlockMessage]](Map.empty)
    implicit val approvedBlockRef = Ref.unsafe[Task, Option[ApprovedBlock]](None)
    implicit val blockStore       = InMemBlockStore.create[Task]
    implicit val blockDagStorage = InMemBlockDagStorage
      .create[Task]
      .unsafeRunSync(monix.execution.Scheduler.Implicits.global)
    implicit val casperRef = MultiParentCasperRef.unsafe[Task](None)
    implicit val safetyOracle = new SafetyOracle[Task] {
      override def normalizedFaultTolerance(
          blockDag: BlockDagRepresentation[Task],
          estimateBlockHash: BlockHash
      ): Task[Float] = Task.pure(1.0f)
    }
  }

  "CasperPacketHandler" when {

    "in  BootstrapCasperHandler state" should {
      "make a transition to ApprovedBlockReceivedHandler once ApprovedBlock has been received" in {
        import monix.execution.Scheduler.Implicits.global
        val fixture = setup()
        import fixture._

        val validators = Set(ByteString.copyFrom(validatorPk.bytes))

        val theInit = Task.unit

        implicit val engineCell = Cell.unsafe[Task, Engine[Task]](Engine.noop)

        // interval and duration don't really matter since we don't require and signs from validators
        val bootstrapCasper =
          new BootstrapCasperHandler[Task](
            runtimeManager,
            shardId,
            Some(validatorId),
            validators,
            theInit
          )

        val approvedBlockCandidate = ApprovedBlockCandidate(block = Some(genesis))

        val approvedBlock: ApprovedBlock = ApprovedBlock(
          candidate = Some(approvedBlockCandidate),
          sigs = Seq(
            Signature(
              ByteString.copyFrom(validatorPk.bytes),
              "ed25519",
              ByteString.copyFrom(
                Ed25519.sign(Blake2b256.hash(approvedBlockCandidate.toByteArray), validatorSk)
              )
            )
          )
        )

        val approvedPacket = Packet(transport.ApprovedBlock.id, approvedBlock.toByteString)

        val test = for {
          _                   <- EngineCell[Task].set(bootstrapCasper)
          casperPacketHandler = new CasperPacketHandler[Task]
          _                   <- casperPacketHandler.handle(local).apply(approvedPacket)
          casperO             <- MultiParentCasperRef[Task].get
          _                   = assert(casperO.isDefined)
          blockO              <- blockStore.get(genesis.blockHash)
          _                   = assert(blockO.isDefined)
          _                   = assert(blockO.contains(genesis))
          handlerInternal     <- EngineCell[Task].read
          _                   = assert(handlerInternal.isInstanceOf[ApprovedBlockReceivedHandler[Task]])
          _ = assert(
            transportLayer.requests.head.msg == packet(
              local,
              networkId,
              transport.ForkChoiceTipRequest,
              ByteString.EMPTY
            )
          )
          _ = transportLayer.reset()
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
      val bonds                  = MultiParentCasperTestUtil.createBonds(validators)
      val genesis                = MultiParentCasperTestUtil.createGenesis(bonds)
      val approvedBlockCandidate = ApprovedBlockCandidate(block = Some(genesis))
      val approvedBlock: ApprovedBlock = ApprovedBlock(
        candidate = Some(approvedBlockCandidate),
        sigs = Seq(
          Signature(
            ByteString.copyFrom(validatorPk.bytes),
            "ed25519",
            ByteString.copyFrom(
              Ed25519.sign(Blake2b256.hash(approvedBlockCandidate.toByteArray), validatorSk)
            )
          )
        )
      )

      implicit val casper = NoOpsCasperEffect[Task]().unsafeRunSync

      val engine =
        new ApprovedBlockReceivedHandler[Task](casper, approvedBlock)

      implicit val engineCell = Cell.unsafe[Task, Engine[Task]](engine)
      val casperPacketHandler = new CasperPacketHandler[Task]

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
          block = packet(local, networkId, transport.BlockMessage, genesis.toByteString)
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

      "respond to ForkChoiceTipRequest messages" in {
        val request = ForkChoiceTipRequest()
        val requestPacket =
          Packet(transport.ForkChoiceTipRequest.id, request.toByteString)

        val test: Task[Unit] = for {
          tip  <- MultiParentCasper.forkChoiceTip[Task]
          _    <- casperPacketHandler.handle(local)(requestPacket)
          head = transportLayer.requests.head
          _    = assert(head.peer == local)
          _ = assert(
            head.msg.message.packet.get == Packet(transport.BlockMessage.id, tip.toByteString)
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
