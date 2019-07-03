package coop.rchain.comm.transport

import java.nio.file._

import scala.util.Random

import coop.rchain.comm.protocol.routing._

import com.google.protobuf.ByteString
import monix.eval.Task
import monix.execution.Scheduler
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class PacketStoreRestoreSpec
    extends FunSpec
    with Matchers
    with BeforeAndAfterEach
    with GeneratorDrivenPropertyChecks {

  import PacketOps._

  implicit val scheduler: Scheduler = Scheduler.Implicits.global
  var tempFolder: Path              = _

  override def beforeEach(): Unit =
    tempFolder = Files.createTempDirectory("rchain-")

  override def afterEach(): Unit =
    tempFolder.toFile.delete()

  describe("Packet store & restore") {
    it("should store and restore to the original Packet") {
      forAll(contentGen) { content: Array[Byte] =>
        // given
        val parent = tempFolder.resolve("inner").resolve("folder")
        val packet = Packet(BlockMessage.id, ByteString.copyFrom(content))
        // when
        val storedIn = packet.store[Task](parent).runSyncUnsafe().right.get
        val restored = PacketOps.restore[Task](storedIn).runSyncUnsafe().right.get
        // then
        storedIn.toFile.exists() shouldBe true
        storedIn.getParent shouldBe parent
        packet shouldBe restored
      }
    }
  }

  val contentGen: Gen[Array[Byte]] =
    for (n <- Gen.choose(10, 50000))
      yield Array.fill(n)((Random.nextInt(256) - 128).toByte)
}
