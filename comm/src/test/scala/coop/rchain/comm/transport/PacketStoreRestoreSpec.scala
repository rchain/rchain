package coop.rchain.comm.transport

import cats.effect.IO
import com.google.protobuf.ByteString
import coop.rchain.comm.protocol.routing._
import org.scalacheck.Gen
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.collection.concurrent.TrieMap
import scala.util.Random

class PacketStoreRestoreSpec extends AnyFunSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  import PacketOps._

  import scala.concurrent.ExecutionContext.Implicits.global
  implicit val cs: ContextShift[IO] = IO.contextShift(global)

  describe("Packet store & restore") {
    it("should store and restore to the original Packet") {
      forAll(contentGen) { content: Array[Byte] =>
        // given
        val cache  = TrieMap[String, Array[Byte]]()
        val packet = Packet("Test", ByteString.copyFrom(content))
        // when
        val storedIn = packet.store[IO](cache).unsafeRunSync.right.get
        val restored = PacketOps.restore[IO](storedIn, cache).unsafeRunSync.right.get
        // then
        packet shouldBe restored
      }
    }
  }

  val contentGen: Gen[Array[Byte]] =
    for (n <- Gen.choose(10, 50000))
      yield Array.fill(n)((Random.nextInt(256) - 128).toByte)
}
