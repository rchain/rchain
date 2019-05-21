package coop.rchain.casper.engine

import coop.rchain.casper._
import coop.rchain.casper.helper.NoOpsCasperEffect
import coop.rchain.casper.protocol._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.comm.protocol.routing.Packet
import coop.rchain.comm.rp.ProtocolHelper._
import coop.rchain.comm.transport
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.crypto.signatures.Ed25519

import com.google.protobuf.ByteString
import monix.eval.Task
import org.scalatest.WordSpec

class RunningSpec extends WordSpec {

  "Running state" should {
    import monix.execution.Scheduler.Implicits.global
    val fixture = Setup()
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

    val engine = new Running[Task](casper, approvedBlock)

    transportLayer.setResponses(_ => p => Right(p))

    "respond to BlockMessage messages " in {
      val blockMessage = BlockMessage(ByteString.copyFrom("Test BlockMessage", "UTF-8"))
      val test: Task[Unit] = for {
        _ <- engine.handle(local, blockMessage)
        _ = assert(casper.store.contains(blockMessage.blockHash))
      } yield ()

      test.unsafeRunSync
      transportLayer.reset()
    }

    "respond to BlockRequest messages" in {
      val blockRequest =
        BlockRequest(Base16.encode(genesis.blockHash.toByteArray), genesis.blockHash)
      val test = for {
        _     <- blockStore.put(genesis.blockHash, genesis)
        _     <- engine.handle(local, blockRequest)
        head  = transportLayer.requests.head
        block = packet(local, networkId, transport.BlockMessage, genesis.toByteString)
        _     = assert(head.peer == local && head.msg == block)
      } yield ()

      test.unsafeRunSync
      transportLayer.reset()
    }

    "respond to ApprovedBlockRequest messages" in {
      val approvedBlockRequest = ApprovedBlockRequest("test")

      val test: Task[Unit] = for {
        _    <- engine.handle(local, approvedBlockRequest)
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
      val test: Task[Unit] = for {
        tip  <- MultiParentCasper.forkChoiceTip[Task]
        _    <- engine.handle(local, request)
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
