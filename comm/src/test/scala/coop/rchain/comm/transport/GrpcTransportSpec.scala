package coop.rchain.comm.transport

import com.google.protobuf.ByteString
import coop.rchain.comm.CommError._
import coop.rchain.comm._
import coop.rchain.comm.protocol.routing._
import coop.rchain.comm.rp.ProtocolHelper
import coop.rchain.metrics.Metrics
import io.grpc.{Status, StatusRuntimeException}
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import org.scalatest._

import scala.collection.mutable
import scala.util.Random

class GrpcTransportSpec extends WordSpecLike with Matchers with Inside {

  implicit val metrics: Metrics[Task] = new Metrics.MetricsNOP
  implicit val scheduler: Scheduler   = Scheduler.Implicits.global
  private val networkId               = "test"
  private val peerLocal               = createPeerNode
  private val peerRemote              = createPeerNode
  private val msg                     = ProtocolHelper.heartbeat(peerLocal, networkId)

  private def createPeerNode: PeerNode = {
    val b = Array.ofDim[Byte](4)
    Random.nextBytes(b)
    val id = NodeIdentifier(b)
    PeerNode.from(id, "host", 0, 0)
  }

  private val ack: TLResponse =
    TLResponse(TLResponse.Payload.Ack(Ack()))

  private def internalServerError(msg: String): TLResponse =
    TLResponse(
      TLResponse.Payload
        .InternalServerError(InternalServerError(ProtocolHelper.toProtocolBytes(msg)))
    )

  private val unavailableThrowable: Throwable =
    new StatusRuntimeException(Status.UNAVAILABLE)

  private val timeoutThrowable: Throwable =
    new StatusRuntimeException(Status.DEADLINE_EXCEEDED)

  private val testThrowable: Throwable =
    new RuntimeException("Test exception")

  private class TestTransportLayer(response: Task[TLResponse])
      extends RoutingGrpcMonix.TransportLayer {
    def send(request: TLRequest): Task[TLResponse] = {
      sendMessages += request
      response
    }
    def stream(input: Observable[Chunk]): Task[TLResponse] =
      input.toListL.map { l =>
        streamMessages += l
        ack
      }

    val sendMessages: mutable.MutableList[TLRequest]     = mutable.MutableList.empty[TLRequest]
    val streamMessages: mutable.MutableList[List[Chunk]] = mutable.MutableList.empty[List[Chunk]]
  }

  "sending a message to a remote peer" when {
    "everything is fine" should {
      "send and receive Unit" in {
        val response   = ack
        val stub       = new TestTransportLayer(Task.now(response))
        val result     = GrpcTransport.send[Task](stub, peerRemote, msg).attempt.runSyncUnsafe()
        val unit: Unit = ()

        inside(result) {
          case Right(Right(p)) => p shouldEqual unit
        }
        stub.sendMessages.length shouldBe 1
        stub.sendMessages.head shouldBe TLRequest(Some(msg))
        stub.streamMessages.length shouldBe 0
      }
    }

    "server replies with InternalCommunicationError" should {
      "fail with an InternalCommunicationError" in {
        val response = internalServerError("Test error")
        val stub     = new TestTransportLayer(Task.now(response))
        val result   = GrpcTransport.send[Task](stub, peerRemote, msg).attempt.runSyncUnsafe()

        inside(result) {
          case Right(Left(p)) =>
            p shouldEqual internalCommunicationError("Got response: Test error")
        }
        stub.sendMessages.length shouldBe 1
        stub.sendMessages.head shouldBe TLRequest(Some(msg))
        stub.streamMessages.length shouldBe 0
      }
    }

    "server is unavailable" should {
      "fail with a PeerUnavailable" in {
        val stub   = new TestTransportLayer(Task.raiseError(unavailableThrowable))
        val result = GrpcTransport.send[Task](stub, peerRemote, msg).attempt.runSyncUnsafe()

        inside(result) {
          case Right(Left(p)) =>
            p shouldEqual peerUnavailable(peerRemote)
        }
        stub.sendMessages.length shouldBe 1
        stub.sendMessages.head shouldBe TLRequest(Some(msg))
        stub.streamMessages.length shouldBe 0
      }
    }

    "timeout" should {
      "fail with a TimeOut" in {
        val stub   = new TestTransportLayer(Task.raiseError(timeoutThrowable))
        val result = GrpcTransport.send[Task](stub, peerRemote, msg).attempt.runSyncUnsafe()

        inside(result) {
          case Right(Left(p)) =>
            p shouldEqual timeout
        }
        stub.sendMessages.length shouldBe 1
        stub.sendMessages.head shouldBe TLRequest(Some(msg))
        stub.streamMessages.length shouldBe 0
      }
    }

    "any other exception" should {
      "fail with a ProtocolException" in {
        val stub   = new TestTransportLayer(Task.raiseError(testThrowable))
        val result = GrpcTransport.send[Task](stub, peerRemote, msg).attempt.runSyncUnsafe()

        inside(result) {
          case Right(Left(p)) =>
            p shouldEqual protocolException(testThrowable)
        }
        stub.sendMessages.length shouldBe 1
        stub.sendMessages.head shouldBe TLRequest(Some(msg))
        stub.streamMessages.length shouldBe 0
      }
    }
  }

  "streaming a Blob to a remote peer" when {
    def messageSize: Int = 4 * 1024 * 1024
    val bigContent: ByteString = {
      val b = 128.toByte
      ProtocolHelper.toProtocolBytes(
        Array.fill((4 * messageSize) + (messageSize / 2))(b)
      )
    }

    "streaming successful" should {
      "deliver a list of Chuncks" in {
        val stub   = new TestTransportLayer(Task.raiseError(testThrowable))
        val blob   = Blob(peerLocal, Packet("N/A", bigContent))
        val chunks = Chunker.chunkIt[Task](networkId, blob, messageSize).runSyncUnsafe().toList
        val result =
          GrpcTransport
            .stream[Task](stub, peerRemote, networkId, blob, messageSize)
            .attempt
            .runSyncUnsafe()

        result shouldBe Right(Right(()))
        stub.streamMessages.length shouldBe 1
        stub.streamMessages.head shouldBe chunks
        stub.sendMessages.length shouldBe 0
      }
    }
  }
}
