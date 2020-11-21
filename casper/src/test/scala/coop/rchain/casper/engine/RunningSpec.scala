package coop.rchain.casper.engine

import com.google.protobuf.ByteString
import coop.rchain.casper._
import coop.rchain.casper.helper.NoOpsCasperEffect
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.{GenesisBuilder, ProtoUtil}
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.comm.rp.ProtocolHelper._
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.models.blockImplicits.getRandomBlock
import coop.rchain.casper.helper.RSpaceStateManagerTestImpl
import monix.eval.Task
import org.scalatest.{BeforeAndAfterEach, Matchers, WordSpec}

import scala.concurrent.duration.FiniteDuration

class RunningSpec extends WordSpec with BeforeAndAfterEach with Matchers {

  val fixture = Setup()
  import fixture._

  override def beforeEach(): Unit =
    transportLayer.setResponses(_ => p => Right(()))

  override def afterEach(): Unit =
    transportLayer.reset()

  "Running state" should {
    import monix.execution.Scheduler.Implicits.global

    val genesis                = GenesisBuilder.createGenesis()
    val approvedBlockCandidate = ApprovedBlockCandidate(block = genesis, requiredSigs = 0)
    val approvedBlock: ApprovedBlock = ApprovedBlock(
      candidate = approvedBlockCandidate,
      sigs = List(
        Signature(
          ByteString.copyFrom(validatorPk.bytes),
          "secp256k1",
          ByteString.copyFrom(
            Secp256k1.sign(Blake2b256.hash(approvedBlockCandidate.toProto.toByteArray), validatorSk)
          )
        )
      )
    )

    implicit val casper    = NoOpsCasperEffect[Task]().unsafeRunSync
    implicit val rspaceMan = RSpaceStateManagerTestImpl[Task]()

    val engine =
      new Running[Task](
        fixture.blockProcessingQueue,
        fixture.blockProcessingState,
        casper,
        approvedBlock,
        None,
        Task.unit,
        true
      )

    // Need to have well-formed block here. Do we have that API in tests?
    "respond to BlockMessage messages " in {
      val blockMessage = getRandomBlock()

      val signedBlockMessage = validatorId.signBlock(blockMessage)
      val test: Task[Unit] = for {
        _ <- engine.handle(local, signedBlockMessage)
        blockIsInqueue <- blockProcessingQueue.dequeue1.map(
                           _._2.blockHash == signedBlockMessage.blockHash
                         )
        _ = assert(blockIsInqueue)
      } yield ()

      test.unsafeRunSync
    }

    "respond to BlockRequest messages" in {
      val blockRequest = BlockRequest(genesis.blockHash)
      val test = for {
        _     <- blockStore.put(genesis.blockHash, genesis)
        _     <- engine.handle(local, blockRequest)
        head  = transportLayer.requests.head
        block = packet(local, networkId, genesis.toProto)
        _     = assert(head.peer == local && head.msg == block)
      } yield ()

      test.unsafeRunSync
    }

    "respond to ApprovedBlockRequest messages" in {
      val approvedBlockRequest = ApprovedBlockRequest("test")

      val test: Task[Unit] = for {
        _    <- engine.handle(local, approvedBlockRequest)
        head = transportLayer.requests.head
        _    = assert(head.peer == local)
        _ = assert(
          ApprovedBlock
            .from(
              ApprovedBlockProto
                .parseFrom(head.msg.message.packet.get.content.toByteArray)
            )
            .right
            .get == approvedBlock
        )
      } yield ()

      test.unsafeRunSync
    }

    "respond to ForkChoiceTipRequest messages" in {
      val request = ForkChoiceTipRequest
      val test: Task[Unit] = for {
        tip  <- MultiParentCasper.forkChoiceTip[Task](casper)
        _    <- engine.handle(local, request)
        head = transportLayer.requests.head
        _    = assert(head.peer == local)
        _ = assert(
          head.msg.message.packet.get == ToPacket(HasBlockProto(tip))
        )
      } yield ()

      test.unsafeRunSync
    }
  }
}
