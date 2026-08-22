package coop.rchain.comm.transport

import cats.effect.Timer
import cats.effect.Sync
import com.google.protobuf.ByteString
import coop.rchain.comm.CommError.CommErr
import coop.rchain.comm._
import coop.rchain.comm.protocol.routing.Packet
import coop.rchain.comm.rp.ProtocolHelper
import coop.rchain.comm.syntax._
import org.scalatest._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

abstract class TransportLayerSpec[F[_]: Sync: Timer, E <: Environment]
    extends TransportLayerRuntime[F, E]
    with AnyWordSpecLike
    with Matchers
    with Inside {

  def maxMessageSize: Int

  val transportLayerName: String = this.getClass.getSimpleName.replace("Spec", "")

  transportLayerName when {
    "sending a message" should {
      "deliver the message" in
        new TwoNodesRuntime[CommErr[Unit]]() {
          def execute(
              transportLayer: TransportLayer[F],
              local: PeerNode,
              remote: PeerNode
          ): F[CommErr[Unit]] = sendHeartbeat(transportLayer, local, remote)

          val result: TwoNodesResult = run()

          protocolDispatcher.received should have length 1
          val (_, protocol2)   = protocolDispatcher.received.head
          val sender: PeerNode = ProtocolHelper.sender(protocol2)
          sender shouldEqual result.localNode
          protocol2.message shouldBe 'heartbeat
        }
    }

    "broadcasting a message" should {
      "send the message to all peers" in
        new ThreeNodesRuntime[Seq[CommErr[Unit]]]() {
          def execute(
              transportLayer: TransportLayer[F],
              local: PeerNode,
              remote1: PeerNode,
              remote2: PeerNode
          ): F[Seq[CommErr[Unit]]] = broadcastHeartbeat(transportLayer, local, remote1, remote2)

          val result: ThreeNodesResult = run()

          protocolDispatcher.received should have length 2
          val Seq((r1, p1), (r2, p2)) = protocolDispatcher.received
          val sender1: PeerNode       = ProtocolHelper.sender(p1)
          val sender2: PeerNode       = ProtocolHelper.sender(p2)
          sender1 shouldEqual result.localNode
          sender2 shouldEqual result.localNode
          p1.message shouldBe 'heartbeat
          p2.message shouldBe 'heartbeat
          r1 should (equal(result.remoteNode1) or equal(result.remoteNode2))
          r2 should (equal(result.remoteNode1) or equal(result.remoteNode2))
        }
    }

    lazy val bigContent: ByteString = {
      val b = 128.toByte
      ProtocolHelper.toProtocolBytes(
        Array.fill((4 * maxMessageSize) + (maxMessageSize / 2))(b)
      )
    }

    "streamBlob" should {
      "send a blob and receive by (single) remote side" in {
        new TwoNodesRuntime[Unit]() {
          def execute(
              transportLayer: TransportLayer[F],
              local: PeerNode,
              remote: PeerNode
          ): F[Unit] =
            transportLayer.stream1(
              remote,
              Blob(local, Packet("Test", bigContent))
            )

          run()

          streamDispatcher.received should have length 1
          val (_, blob) = streamDispatcher.received.head
          blob.packet.typeId shouldBe ("Test")
          blob.packet.content shouldBe (bigContent)
        }
      }

      "send a blob and receive by (multiple) remote side" in {
        new ThreeNodesRuntime[Unit]() {
          def execute(
              transportLayer: TransportLayer[F],
              local: PeerNode,
              remote1: PeerNode,
              remote2: PeerNode
          ): F[Unit] =
            transportLayer.stream(
              List(remote1, remote2),
              Blob(local, Packet("N/A", bigContent))
            )

          run()

          streamDispatcher.received should have length 2
          val (_, blob1) = streamDispatcher.received(0)
          val (_, blob2) = streamDispatcher.received(1)
          blob1.packet.typeId shouldBe "N/A"
          blob1.packet.content shouldBe bigContent
          blob2.packet.typeId shouldBe "N/A"
          blob2.packet.content shouldBe bigContent
        }
      }

    }

  }
}
