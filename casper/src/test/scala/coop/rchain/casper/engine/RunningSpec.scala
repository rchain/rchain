//package coop.rchain.casper.engine
//
//import com.google.protobuf.ByteString
//import coop.rchain.casper._
//import coop.rchain.casper.helper.{NoOpsCasperEffect, RSpaceStateManagerTestImpl}
//import coop.rchain.casper.protocol._
//import coop.rchain.casper.util.GenesisBuilder
//import coop.rchain.catscontrib.TaskContrib._
//import coop.rchain.comm.rp.ProtocolHelper._
//import coop.rchain.crypto.hash.Blake2b256
//import coop.rchain.crypto.signatures.Secp256k1
//import coop.rchain.models.blockImplicits.getRandomBlock
//import monix.eval.Task
//import org.scalatest.{BeforeAndAfterEach, Matchers, WordSpec}
//
//import scala.concurrent.duration.FiniteDuration
//
//class RunningSpec extends WordSpec with BeforeAndAfterEach with Matchers {
//
//  val fixture = Setup()
//  import fixture._
//
//  override def beforeEach(): Unit =
//    transportLayer.setResponses(_ => p => Right(()))
//
//  override def afterEach(): Unit =
//    transportLayer.reset()
//
//  "Running state" should {
//    val genesis = GenesisBuilder.createGenesis()
//    blockDagStorage.insert(genesis, false, approved = true).runSyncUnsafe()
//    val approvedBlockCandidate = ApprovedBlockCandidate(block = genesis, requiredSigs = 0)
//    val approvedBlock: ApprovedBlock = ApprovedBlock(
//      candidate = approvedBlockCandidate,
//      sigs = List(
//        Signature(
//          ByteString.copyFrom(validatorPk.bytes),
//          "secp256k1",
//          ByteString.copyFrom(
//            Secp256k1.sign(Blake2b256.hash(approvedBlockCandidate.toProto.toByteArray), validatorSk)
//          )
//        )
//      )
//    )
//
//    implicit val casper    = NoOpsCasperEffect[Task]().unsafeRunSync
//    implicit val rspaceMan = RSpaceStateManagerTestImpl[Task]()
//
//    val engine =
//      new Running[Task](
//        fixture.blockProcessingQueue,
//        fixture.blockProcessingState,
//        casper,
//        approvedBlock,
//        None,
//        Task.unit,
//        true
//      )
//
//    // Need to have well-formed block here. Do we have that API in tests?
//    "respond to BlockMessage messages " in {
//      val blockMessage = getRandomBlock()
//
//      val signedBlockMessage = validatorId.signBlock(blockMessage)
//      val test: Task[Unit] = for {
//        _ <- engine.handle(local, signedBlockMessage)
//        blockIsInqueue <- blockProcessingQueue.dequeue1.map(
//                           _._2.blockHash == signedBlockMessage.blockHash
//                         )
//        _ = assert(blockIsInqueue)
//      } yield ()
//
//      test.unsafeRunSync
//    }
//
//    "respond to BlockRequest messages" in {
//      val blockRequest = BlockRequest(genesis.blockHash)
//      val test = for {
//        _     <- blockStore.put(genesis.blockHash, genesis)
//        _     <- engine.handle(local, blockRequest)
//        head  = transportLayer.requests.head
//        block = packet(local, networkId, genesis.toProto)
//        _     = assert(head.peer == local && head.msg == block)
//      } yield ()
//
//      test.unsafeRunSync
//    }
//
//    "respond to ApprovedBlockRequest messages" in {
//      val approvedBlockRequest = ApprovedBlockRequest("test")
//
//      val test: Task[Unit] = for {
//        _    <- engine.handle(local, approvedBlockRequest)
//        head = transportLayer.requests.head
//        _    = assert(head.peer == local)
//        _ = assert(
//          ApprovedBlock
//            .from(
//              ApprovedBlockProto
//                .parseFrom(head.msg.message.packet.get.content.toByteArray)
//            )
//            .right
//            .get == approvedBlock
//        )
//      } yield ()
//
//      test.unsafeRunSync
//    }
//
//    "respond to ForkChoiceTipRequest messages" in {
//      val request = ForkChoiceTipRequest
//      val block1  = getRandomBlock().copy(sender = ByteString.EMPTY)
//      val block2  = getRandomBlock().copy(sender = ByteString.EMPTY)
//      val test: Task[Unit] = for {
//        _        <- blockDagStorage.insert(block1, false)
//        _        <- blockDagStorage.insert(block2, false)
//        tips     <- casper.blockDag.flatMap(_.latestMessageHashes.map(_.values))
//        _        <- engine.handle(local, request)
//        requests = transportLayer.requests.map(_.msg.message.packet.get).toSet
//        expected = tips.map(tip => ToPacket(HasBlockProto(tip))).toSet
//        _        = assert(transportLayer.requests.head.peer == local)
//        _        = assert(requests == expected)
//      } yield ()
//
//      test.unsafeRunSync
//    }
//  }
//}
