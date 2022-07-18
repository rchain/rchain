package coop.rchain.comm.transport

import com.google.protobuf.ByteString
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.catscontrib.ski._
import coop.rchain.comm._
import coop.rchain.comm.protocol.routing._
import coop.rchain.comm.transport.StreamHandler.CircuitBreaker
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalatest.Inside
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.concurrent.TrieMap
import scala.util.Random

class StreamHandlerSpec extends AnyFunSpec with Matchers with Inside {

  implicit val log: Log.NOPLog[Task] = new Log.NOPLog[Task]()

  val networkId = "test"

  describe("StreamHandler.handleStream result") {
    it("should contain sender and message type information") {
      // given
      val stream = createStream(typeId = "BlockMessageTest")
      // when
      val msg: StreamMessage = handleStream(stream)
      // then
      msg.sender shouldBe peerNode("sender")
      msg.typeId shouldBe "BlockMessageTest"
    }

    it("should contain content length of the stored file") {
      // given
      val messageSize   = 10 * 1024 // 10 kb
      val contentLength = messageSize * 3 + (messageSize / 2)
      val stream = createStream(
        messageSize = messageSize,
        contentLength = contentLength
      )
      // when
      val msg: StreamMessage = handleStream(stream)
      // then
      msg.contentLength shouldBe contentLength
    }

    it("should stop receiving a stream if circuit broken") {
      // given
      val cache = TrieMap[String, Array[Byte]]()
      val breakOnSndChunk: CircuitBreaker =
        streamed => Opened(StreamHandler.StreamError.circuitOpened)
      val stream = createStream()
      // when
      val err: StreamHandler.StreamError = handleStreamErr(stream, circuitBreaker = breakOnSndChunk)
      // then
      inside(err) {
        case StreamHandler.StreamError.MaxSizeReached => ()
      }
      cache shouldBe empty
    }

    it("should stop processing a stream if stream is missing header") {
      // given
      val cache = TrieMap[String, Array[Byte]]()
      val streamWithoutHeader: Observable[Chunk] =
        Observable.fromIterator(createStreamIterator().map(_.toList).map {
          case _ :: data => data.toIterator
          case _         => throw new RuntimeException("")
        })
      // when
      val err: StreamHandler.StreamError = handleStreamErr(streamWithoutHeader)
      // then
      inside(err) {
        case StreamHandler.StreamError.NotFullMessage(_) =>
      }
      cache shouldBe empty
    }

    it("should stop processing a stream if stream brought incomplete data") {
      // given
      val cache = TrieMap[String, Array[Byte]]()
      val incompleteStream: Observable[Chunk] =
        Observable.fromIterator(createStreamIterator().map(_.toList).map {
          case header :: _ :: data2 => (header :: data2).toIterator
          case _                    => throw new RuntimeException("")
        })
      // when
      val err: StreamHandler.StreamError = handleStreamErr(incompleteStream, cache = cache)
      // then
      inside(err) {
        case StreamHandler.StreamError.NotFullMessage(_) =>
      }
      cache shouldBe empty
    }
  }

  private def handleStream(
      stream: Observable[Chunk],
      cache: TrieMap[String, Array[Byte]] = TrieMap[String, Array[Byte]]()
  ): StreamMessage =
    StreamHandler
      .handleStream(stream, circuitBreaker = neverBreak, cache)
      .runSyncUnsafe()
      .right
      .get

  private def handleStreamErr(
      stream: Observable[Chunk],
      circuitBreaker: StreamHandler.CircuitBreaker = neverBreak,
      cache: TrieMap[String, Array[Byte]] = TrieMap[String, Array[Byte]]()
  ): StreamHandler.StreamError =
    StreamHandler
      .handleStream(stream, circuitBreaker = circuitBreaker, cache)
      .runSyncUnsafe()
      .left
      .get

  private def createStream(
      messageSize: Int = 10 * 1024,
      contentLength: Int = 30 * 1024,
      sender: String = "sender",
      typeId: String = "BlockMessageTest"
  ): Observable[Chunk] =
    Observable.fromIterator(createStreamIterator(messageSize, contentLength, sender, typeId))

  private def createStreamIterator(
      messageSize: Int = 10 * 1024,
      contentLength: Int = 30 * 1024,
      sender: String = "sender",
      typeId: String = "BlockMessageTest"
  ): Task[Iterator[Chunk]] = {

    val content = Array.fill(contentLength)((Random.nextInt(256) - 128).toByte)
    val packet  = Packet(typeId, ByteString.copyFrom(content))
    val peer    = peerNode(sender)
    val blob    = Blob(peer, packet)
    Chunker.chunkIt[Task](networkId, blob, messageSize)
  }

  private def peerNode(name: String): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), Endpoint("", 80, 80))

  private val neverBreak: CircuitBreaker = kp(Closed)
}
