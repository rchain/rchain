package coop.rchain.comm.transport

import scala.concurrent.duration._

import cats._
import cats.implicits._

import coop.rchain.comm._
import coop.rchain.comm.protocol.routing.Protocol
import coop.rchain.comm.CommError.CommErr

import org.scalatest._

abstract class TransportLayerSpec[F[_]: Monad, E <: Environment]
    extends TransportLayerRuntime[F, E]
    with WordSpecLike
    with Matchers
    with Inside {

  val transportLayerName: String = this.getClass.getSimpleName.replace("Spec", "")

  transportLayerName when {
    "doing a round trip to remote peer" when {
      "everything is fine" should {
        "send and receive the message" in
          new TwoNodesRuntime[CommErr[Protocol]](Dispatcher.pongDispatcher[F]) {
            def execute(transportLayer: TransportLayer[F],
                        local: PeerNode,
                        remote: PeerNode): F[CommErr[Protocol]] =
              roundTripWithPing(transportLayer, local, remote)

            val result: TwoNodesResult = run()

            inside(result()) {
              case Right(protocol1) =>
                val sender = ProtocolHelper.sender(protocol1)
                sender shouldBe 'defined
                sender.get shouldEqual result.remoteNode
                protocol1.message shouldBe 'pong
            }

            result.receivedMessages should have length 1
            val (_, protocol2)           = result.receivedMessages.head
            val sender: Option[PeerNode] = ProtocolHelper.sender(protocol2)
            sender shouldBe 'defined
            sender.get shouldEqual result.localNode
            protocol2.message shouldBe 'ping
          }
      }

      "response takes to long" should {
        "fail with a timeout" in
          new TwoNodesRuntime[CommErr[Protocol]](Dispatcher.pongDispatcherWithDelay(500)) {
            def execute(transportLayer: TransportLayer[F],
                        local: PeerNode,
                        remote: PeerNode): F[CommErr[Protocol]] =
              roundTripWithPing(transportLayer, local, remote, 200.millis)

            val result: TwoNodesResult = run()

            result() shouldEqual Left(TimeOut)
          }
      }

      "there is no response body" should {
        "fail with a communication error" in
          new TwoNodesRuntime[CommErr[Protocol]](Dispatcher.withoutMessageDispatcher) {
            def execute(transportLayer: TransportLayer[F],
                        local: PeerNode,
                        remote: PeerNode): F[CommErr[Protocol]] =
              roundTripWithPing(transportLayer, local, remote)

            val result: TwoNodesResult = run()

            result() shouldEqual Left(
              InternalCommunicationError("Was expecting message, nothing arrived"))
          }
      }

      "peer is not listening" should {
        "fail with peer unavailable error" in
          new TwoNodesRemoteDeadRuntime[CommErr[Protocol]](Dispatcher.pongDispatcher[F]) {
            def execute(transportLayer: TransportLayer[F],
                        local: PeerNode,
                        remote: PeerNode): F[CommErr[Protocol]] =
              roundTripWithPing(transportLayer, local, remote)

            val result: TwoNodesResult = run()

            result() shouldEqual Left(PeerUnavailable(result.remoteNode))
          }
      }

      "there was a peer-side error" should {
        "fail with an internal communication error" in
          new TwoNodesRuntime[CommErr[Protocol]](Dispatcher.internalCommunicationErrorDispatcher[F]) {
            def execute(transportLayer: TransportLayer[F],
                        local: PeerNode,
                        remote: PeerNode): F[CommErr[Protocol]] =
              roundTripWithPing(transportLayer, local, remote)

            val result: TwoNodesResult = run()

            result() shouldEqual Left(
              InternalCommunicationError("Got response: Internal communication error. Test"))
          }
      }
    }

    "sending a message" should {
      "deliver the message" in
        new TwoNodesRuntime[Unit](Dispatcher.dispatcherWithLatch[F]()) {
          def execute(transportLayer: TransportLayer[F],
                      local: PeerNode,
                      remote: PeerNode): F[Unit] =
            for {
              r <- sendPing(transportLayer, local, remote)
              _ = await()
            } yield r

          val result: TwoNodesResult = run()

          result.receivedMessages should have length 1
          val (_, protocol2)           = result.receivedMessages.head
          val sender: Option[PeerNode] = ProtocolHelper.sender(protocol2)
          sender shouldBe 'defined
          sender.get shouldEqual result.localNode
          protocol2.message shouldBe 'ping
        }

      "not wait for a response" in
        new TwoNodesRuntime[Long](Dispatcher.dispatcherWithLatch[F]()) {
          def execute(transportLayer: TransportLayer[F],
                      local: PeerNode,
                      remote: PeerNode): F[Long] =
            for {
              _ <- sendPing(transportLayer, local, remote)
              t = System.currentTimeMillis()
              _ = await()
            } yield t

          val result: TwoNodesResult = run()

          val sent = result()
          sent should be < result.lastProcessedMessageTimestamp
        }

      "wait for message being delivered" in {
        // future feature, not yet implemented
        pending
      }
    }

    "broadcasting a message" should {
      "send the message to all peers" in
        new ThreeNodesRuntime[Unit](Dispatcher.dispatcherWithLatch[F](2)) {
          def execute(transportLayer: TransportLayer[F],
                      local: PeerNode,
                      remote1: PeerNode,
                      remote2: PeerNode): F[Unit] =
            for {
              r <- broadcastPing(transportLayer, local, remote1, remote2)
              _ = await()
            } yield r

          val result: ThreeNodesResult = run()

          result.receivedMessages should have length 2
          val Seq((r1, p1), (r2, p2))   = result.receivedMessages
          val sender1: Option[PeerNode] = ProtocolHelper.sender(p1)
          val sender2: Option[PeerNode] = ProtocolHelper.sender(p2)
          sender1 shouldBe 'defined
          sender2 shouldBe 'defined
          sender1.get shouldEqual result.localNode
          sender2.get shouldEqual result.localNode
          p1.message shouldBe 'ping
          p2.message shouldBe 'ping
          r1 should (equal(result.remoteNode1) or equal(result.remoteNode2))
          r2 should (equal(result.remoteNode1) or equal(result.remoteNode2))
        }
    }

    "shutting down" when {
      "doing a round trip" should {
        "not send the message" in
          new TwoNodesRuntime[CommErr[Protocol]](Dispatcher.pongDispatcher[F]) {
            def execute(transportLayer: TransportLayer[F],
                        local: PeerNode,
                        remote: PeerNode): F[CommErr[Protocol]] =
              for {
                _ <- transportLayer.shutdown(CommMessages.disconnect(local))
                r <- roundTripWithPing(transportLayer, local, remote)
              } yield r

            val result: TwoNodesResult = run()

            inside(result()) {
              case Left(ProtocolException(e)) =>
                e.getMessage shouldEqual "The transport layer has been shut down"
            }

            result.receivedMessages shouldBe 'empty
          }
      }

      "sending a message" should {
        "not send the message" in
          new TwoNodesRuntime[Unit](Dispatcher.dispatcherWithLatch[F]()) {
            def execute(transportLayer: TransportLayer[F],
                        local: PeerNode,
                        remote: PeerNode): F[Unit] =
              for {
                _ <- transportLayer.shutdown(CommMessages.disconnect(local))
                r <- sendPing(transportLayer, local, remote)
                _ = await()
              } yield r

            val result: TwoNodesResult = run()

            result.receivedMessages shouldBe 'empty
          }
      }

      "broadcasting a message" should {
        "not send any messages" in
          new ThreeNodesRuntime[Unit](Dispatcher.dispatcherWithLatch[F](2)) {
            def execute(transportLayer: TransportLayer[F],
                        local: PeerNode,
                        remote1: PeerNode,
                        remote2: PeerNode): F[Unit] =
              for {
                _ <- transportLayer.shutdown(CommMessages.disconnect(local))
                r <- broadcastPing(transportLayer, local, remote1, remote2)
                _ = await()
              } yield r

            val result: ThreeNodesResult = run()

            result.receivedMessages shouldBe 'empty
          }
      }
    }
  }
}
