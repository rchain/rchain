package coop.rchain.comm.transport

import cats.effect.IO
import com.google.protobuf.ByteString
import coop.rchain.catscontrib.ski._
import coop.rchain.comm._
import coop.rchain.comm.protocol.routing._
import coop.rchain.comm.transport.StreamHandler.CircuitBreaker
import coop.rchain.shared.Log
import monix.reactive.Observable
import org.scalatest.Inside
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import fs2.Stream

import scala.collection.concurrent.TrieMap
import scala.util.Random

class StreamHandlerSpec extends AnyFunSpec with Matchers with Inside {

  implicit val log: Log.NOPLog[IO] = new Log.NOPLog[IO]()

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
      val streamWithoutHeader: Stream[IO, Chunk] = {
        val it: IO[Iterator[Chunk]] = createStreamIterator().map(_.toList).map {
          case _ :: data => data.toIterator
          case _         => throw new RuntimeException("")
        }
        Stream.eval(it).flatMap(Stream.fromIterator[IO](_, 1))
      }
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
      val incompleteStream: Stream[IO, Chunk] = {
        val it: IO[Iterator[Chunk]] = createStreamIterator().map(_.toList).map {
          case header :: _ :: data2 => (header :: data2).toIterator
          case _                    => throw new RuntimeException("")
        }
        Stream.eval(it).flatMap(Stream.fromIterator[IO](_, 1))
      }
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
      stream: fs2.Stream[IO, Chunk],
      cache: TrieMap[String, Array[Byte]] = TrieMap[String, Array[Byte]]()
  ): StreamMessage =
    StreamHandler
      .handleStream(stream, circuitBreaker = neverBreak, cache)
      .unsafeRunSync
      .right
      .get

  private def handleStreamErr(
      stream: fs2.Stream[IO, Chunk],
      circuitBreaker: StreamHandler.CircuitBreaker = neverBreak,
      cache: TrieMap[String, Array[Byte]] = TrieMap[String, Array[Byte]]()
  ): StreamHandler.StreamError =
    StreamHandler
      .handleStream(stream, circuitBreaker = circuitBreaker, cache)
      .unsafeRunSync
      .left
      .get

  private def createStream(
      messageSize: Int = 10 * 1024,
      contentLength: Int = 30 * 1024,
      sender: String = "sender",
      typeId: String = "BlockMessageTest"
  ): Stream[IO, Chunk] =
    Stream
      .eval(createStreamIterator(messageSize, contentLength, sender, typeId))
      .flatMap(Stream.fromIterator[IO](_, 1))

  private def createStreamIterator(
      messageSize: Int = 10 * 1024,
      contentLength: Int = 30 * 1024,
      sender: String = "sender",
      typeId: String = "BlockMessageTest"
  ): IO[Iterator[Chunk]] = {

    val content = Array.fill(contentLength)((Random.nextInt(256) - 128).toByte)
    val packet  = Packet(typeId, ByteString.copyFrom(content))
    val peer    = peerNode(sender)
    val blob    = Blob(peer, packet)
    Chunker.chunkIt[IO](networkId, blob, messageSize)
  }

  private def peerNode(name: String): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), Endpoint("", 80, 80))

  private val neverBreak: CircuitBreaker = kp(Closed)
}
