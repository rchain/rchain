package coop.rchain.comm.transport

import java.io.{File, FileNotFoundException}
import java.nio.file._
import java.util.UUID
import org.scalatest._
import coop.rchain.comm._
import coop.rchain.shared.Log
import coop.rchain.comm.protocol.routing._
import scala.util.Random
import monix.eval.Task
import monix.reactive.Observable
import coop.rchain.catscontrib.TaskContrib._
import cats._, cats.data._, cats.implicits._
import monix.execution.Scheduler.Implicits.global
import com.google.protobuf.ByteString

class StreamHandlerSpec extends FunSpec with Matchers with BeforeAndAfterEach {

  implicit val log = new Log.NOPLog[Task]()

  var tempFolder: Path = null

  override def beforeEach(): Unit =
    tempFolder = Files.createTempDirectory("rchain")

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
    }

    it("should contain sender and message type information") {
      // given
      val stream = createStream(
        sender = "sender",
        typeId = BlockMessage.id
      )
      // when
      val msg: StreamMessage = handleStream(stream)
      // then
      msg.sender shouldBe peerNode("sender")
      msg.typeId shouldBe BlockMessage.id
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
        FileSystems.getDefault().getPath("~/.rchaintest/" + UUID.randomUUID.toString + "/")
      // when
      val msg: StreamMessage = handleStream(stream, folder = nonExistingWithPersmission)
      // then
      msg.path.getParent shouldBe nonExistingWithPersmission
    }
  }

  private def handleStream(stream: Observable[Chunk], folder: Path = tempFolder): StreamMessage =
    StreamHandler.handleStream(folder, stream).unsafeRunSync.right.get

  private def handleStreamErr(stream: Observable[Chunk], folder: Path): Throwable =
    StreamHandler.handleStream(folder, stream).unsafeRunSync.left.get

  private def createStream(
      messageSize: Int = 10 * 1024,
      contentLength: Int = 30 * 1024,
      sender: String = "sender",
      typeId: String = BlockMessage.id
  ): Observable[Chunk] = {

    val content = Array.fill(contentLength)((Random.nextInt(256) - 128).toByte)
    val packet  = Packet(BlockMessage.id, ByteString.copyFrom(content))
    val sender  = peerNode("sender")
    val blob    = Blob(sender, packet)
    Observable.fromIterator(Chunker.chunkIt(blob, messageSize))
  }

  private def peerNode(name: String): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), Endpoint("", 80, 80))

}
