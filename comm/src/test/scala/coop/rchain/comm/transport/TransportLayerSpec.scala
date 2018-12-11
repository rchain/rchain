package coop.rchain.comm.transport

import scala.concurrent.duration._

import cats._
import cats.implicits._
import coop.rchain.comm._, rp.ProtocolHelper
import coop.rchain.comm.protocol.routing.{Packet, Protocol}
import coop.rchain.comm.CommError.CommErr
import com.google.protobuf.ByteString
import org.scalatest._

abstract class TransportLayerSpec[F[_]: Monad: cats.effect.Timer, E <: Environment]
    extends TransportLayerRuntime[F, E]
    with WordSpecLike
    with Matchers
    with Inside {

  def maxMessageSize: Int

  val transportLayerName: String = this.getClass.getSimpleName.replace("Spec", "")

  transportLayerName when {

    "doing a round trip to remote peer" when {

      "everything is fine" should {
        "send and receive the message" in
          new TwoNodesRuntime[CommErr[Protocol]](Dispatcher.heartbeatResponseDispatcher[F]) {
            def execute(
                transportLayer: TransportLayer[F],
                local: PeerNode,
                remote: PeerNode
            ): F[CommErr[Protocol]] =
              roundTripWithHeartbeat(transportLayer, local, remote)

            val result: TwoNodesResult = run()

            inside(result()) {
              case Right(protocol1) =>
                val sender = ProtocolHelper.sender(protocol1)
                sender shouldBe 'defined
                sender.get shouldEqual result.remoteNode
                protocol1.message shouldBe 'heartbeatResponse
            }

            protocolDispatcher.received should have length 1
            val (_, protocol2)           = protocolDispatcher.received.head
            val sender: Option[PeerNode] = ProtocolHelper.sender(protocol2)
            sender shouldBe 'defined
            sender.get shouldEqual result.localNode
            protocol2.message shouldBe 'heartbeat
          }
      }

      "response takes to long" should {
        "fail with a timeout" in
          new TwoNodesRuntime[CommErr[Protocol]](
            Dispatcher.heartbeatResponseDispatcherWithDelay(1.second)
          ) {
            def execute(
                transportLayer: TransportLayer[F],
                local: PeerNode,
                remote: PeerNode
            ): F[CommErr[Protocol]] =
              roundTripWithHeartbeat(transportLayer, local, remote, 200.millis)

            val result: TwoNodesResult = run()

            result() shouldEqual Left(TimeOut)
          }
      }

      "there is no response body" should {
        "fail with a communication error" in
          new TwoNodesRuntime[CommErr[Protocol]]() {
            def execute(
                transportLayer: TransportLayer[F],
                local: PeerNode,
                remote: PeerNode
            ): F[CommErr[Protocol]] =
              roundTripWithHeartbeat(transportLayer, local, remote)

            val result: TwoNodesResult = run()

            result() shouldEqual Left(
              InternalCommunicationError("Was expecting message, nothing arrived")
            )
          }
      }

      "peer is not listening" should {
        "fail with peer unavailable error" in
          new TwoNodesRemoteDeadRuntime[CommErr[Protocol]](
            Dispatcher.heartbeatResponseDispatcher[F]
          ) {
            def execute(
                transportLayer: TransportLayer[F],
                local: PeerNode,
                remote: PeerNode
            ): F[CommErr[Protocol]] =
              roundTripWithHeartbeat(transportLayer, local, remote)

            val result: TwoNodesResult = run()

            result() shouldEqual Left(PeerUnavailable(result.remoteNode))
          }
      }

      "there was a peer-side error" should {
        "fail with an internal communication error" in
          new TwoNodesRuntime[CommErr[Protocol]](Dispatcher.internalCommunicationErrorDispatcher[F]) {
            def execute(
                transportLayer: TransportLayer[F],
                local: PeerNode,
                remote: PeerNode
            ): F[CommErr[Protocol]] =
              roundTripWithHeartbeat(transportLayer, local, remote)

            val result: TwoNodesResult = run()

            result() shouldEqual Left(
              InternalCommunicationError("Got response: Internal communication error. Test")
            )
          }
      }
    }

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
          val (_, protocol2)           = protocolDispatcher.received.head
          val sender: Option[PeerNode] = ProtocolHelper.sender(protocol2)
          sender shouldBe 'defined
          sender.get shouldEqual result.localNode
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
          val Seq((r1, p1), (r2, p2))   = protocolDispatcher.received
          val sender1: Option[PeerNode] = ProtocolHelper.sender(p1)
          val sender2: Option[PeerNode] = ProtocolHelper.sender(p2)
          sender1 shouldBe 'defined
          sender2 shouldBe 'defined
          sender1.get shouldEqual result.localNode
          sender2.get shouldEqual result.localNode
          p1.message shouldBe 'heartbeat
          p2.message shouldBe 'heartbeat
          r1 should (equal(result.remoteNode1) or equal(result.remoteNode2))
          r2 should (equal(result.remoteNode1) or equal(result.remoteNode2))
        }
    }

    "shutting down" when {
      "doing a round trip" should {
        "not send the message" in
          new TwoNodesRuntime[CommErr[Protocol]]() {
            def execute(
                transportLayer: TransportLayer[F],
                local: PeerNode,
                remote: PeerNode
            ): F[CommErr[Protocol]] =
              for {
                _ <- transportLayer.shutdown(ProtocolHelper.disconnect(local))
                r <- roundTripWithHeartbeat(transportLayer, local, remote)
              } yield r

            val result: TwoNodesResult = run(false)

            inside(result()) {
              case Left(ProtocolException(e)) =>
                e.getMessage shouldEqual "The transport layer has been shut down"
            }

            protocolDispatcher.received shouldBe 'empty
          }
      }

      "sending a message" should {
        "not send the message" in
          new TwoNodesRuntime[CommErr[Unit]]() {
            def execute(
                transportLayer: TransportLayer[F],
                local: PeerNode,
                remote: PeerNode
            ): F[CommErr[Unit]] =
              for {
                _ <- transportLayer.shutdown(ProtocolHelper.disconnect(local))
                r <- sendHeartbeat(transportLayer, local, remote)
              } yield r

            run(false)
            protocolDispatcher.received shouldBe 'empty
          }
      }

      "broadcasting a message" should {
        "not send any messages" in
          new ThreeNodesRuntime[Seq[CommErr[Unit]]]() {
            def execute(
                transportLayer: TransportLayer[F],
                local: PeerNode,
                remote1: PeerNode,
                remote2: PeerNode
            ): F[Seq[CommErr[Unit]]] =
              for {
                _ <- transportLayer.shutdown(ProtocolHelper.disconnect(local))
                r <- broadcastHeartbeat(transportLayer, local, remote1, remote2)
              } yield r

            run(false)
            protocolDispatcher.received shouldBe 'empty
          }
      }

    }

    val bigContent: ByteString = {
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
            transportLayer.stream(
              List(remote),
              Blob(local, Packet("N/A", bigContent))
            )

          run()

          streamDispatcher.received should have length 1
          val (_, blob) = streamDispatcher.received.head
          blob.packet.typeId shouldBe ("N/A")
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
          blob1.packet.typeId shouldBe ("N/A")
          blob1.packet.content shouldBe (bigContent)
          blob2.packet.typeId shouldBe ("N/A")
          blob2.packet.content shouldBe (bigContent)
        }
      }

    }

  }
}
