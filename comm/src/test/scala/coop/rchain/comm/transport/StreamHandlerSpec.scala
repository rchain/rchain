package coop.rchain.comm.transport

import java.nio.file._
import java.util.UUID

import scala.util.Random

import StreamHandler.CircuitBreaker
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.catscontrib.ski._
import coop.rchain.comm._
import coop.rchain.comm.protocol.routing._
import coop.rchain.shared.Log

import com.google.protobuf.ByteString
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalatest._

class StreamHandlerSpec extends FunSpec with Matchers with Inside with BeforeAndAfterEach {

  implicit val log: Log.NOPLog[Task] = new Log.NOPLog[Task]()

  val networkId = "test"

  var tempFolder: Path = _

  override def beforeEach(): Unit =
    tempFolder = Files.createTempDirectory("rchain-")

  override def afterEach(): Unit =
    tempFolder.toFile.delete()

  describe("StreamHandler.handleStream result") {
    it("should contain file path which parent is temp folder") {
      // given
      val stream = createStream()
      // when
      val msg: StreamMessage = handleStream(stream)
      // then
      msg.path.getParent shouldBe tempFolder
      msg.path.toFile.exists shouldBe true
    }

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

    it(
      "should create a file in non-existing folder if there are permissions to create that folder or files in it"
    ) {
      // given
      val stream = createStream()
      val nonExistingWithPersmission =
        FileSystems.getDefault.getPath("~/.rchaintest/" + UUID.randomUUID.toString + "/")
      // when
      val msg: StreamMessage = handleStream(stream, folder = nonExistingWithPersmission)
      // then
      msg.path.getParent shouldBe nonExistingWithPersmission
    }

    it("should stop receiving a stream if circuit broken") {
      // given
      val breakOnSndChunk: CircuitBreaker =
        streamed => Opened(StreamHandler.StreamError.circuitOpened)
      val stream = createStream()
      // when
      val err: StreamHandler.StreamError = handleStreamErr(stream, circuitBreaker = breakOnSndChunk)
      // then
      inside(err) {
        case StreamHandler.StreamError.MaxSizeReached => ()
      }
      tempFolder.toFile.list() should be(empty)
    }

    it("should stop processing a stream if stream is missing part of header") {
      // given
      val streamWithIncompleteHeader: Observable[Chunk] =
        Observable.fromIterator(createStreamIterator().map(_.toList).map {
          case header :: data =>
            val newHeaderContent: Chunk.Content =
              Chunk.Content.Header(header.content.header.get.copy(sender = None))
            val incompleteHeader = header.copy(content = newHeaderContent)
            (incompleteHeader :: data).toIterator
          case Nil => throw new RuntimeException("")
        })
      // when
      val err: StreamHandler.StreamError = handleStreamErr(streamWithIncompleteHeader)
      // then
      inside(err) {
        case StreamHandler.StreamError.NotFullMessage(_) =>
      }
      tempFolder.toFile.list() should be(empty)
    }

    it("should stop processing a stream if stream is missing header") {
      // given
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
      tempFolder.toFile.list() should be(empty)
    }

    it("should stop processing a stream if stream brought incomplete data") {
      // given
      val incompleteStream: Observable[Chunk] =
        Observable.fromIterator(createStreamIterator().map(_.toList).map {
          case header :: _ :: data2 => (header :: data2).toIterator
          case _                    => throw new RuntimeException("")
        })
      // when
      val err: StreamHandler.StreamError = handleStreamErr(incompleteStream)
      // then
      inside(err) {
        case StreamHandler.StreamError.NotFullMessage(_) =>
      }
      tempFolder.toFile.list() should be(empty)
    }
  }

  private def handleStream(stream: Observable[Chunk], folder: Path = tempFolder): StreamMessage =
    StreamHandler
      .handleStream(folder, stream, circuitBreaker = neverBreak)
      .unsafeRunSync
      .right
      .get

  private def handleStreamErr(
      stream: Observable[Chunk],
      folder: Path = tempFolder,
      circuitBreaker: StreamHandler.CircuitBreaker = neverBreak
  ): StreamHandler.StreamError =
    StreamHandler
      .handleStream(folder, stream, circuitBreaker = circuitBreaker)
      .unsafeRunSync
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
    val sender  = peerNode("sender")
    val blob    = Blob(sender, packet)
    Chunker.chunkIt[Task](networkId, blob, messageSize)
  }

  private def peerNode(name: String): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), Endpoint("", 80, 80))

  private val neverBreak: CircuitBreaker = kp(Closed)
}
