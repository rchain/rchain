package coop.rchain.comm.transport

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

import scala.collection.mutable
import scala.concurrent.duration._

import cats._
import cats.implicits._

import coop.rchain.comm._
import coop.rchain.comm.protocol.routing.Protocol
import coop.rchain.comm.CommError.CommErr

import org.scalatest._

abstract class TransportLayerSpec[F[_]: Monad, E <: Environment]()
    extends WordSpec
    with Matchers
    with Inside {

  val transportLayerName: String = this.getClass.getSimpleName.replace("Spec", "")

  transportLayerName when {
    "doing a round trip to remote peer" when {
      "everything is fine" should {
        "send and receive the message" in {

          val received = mutable.MutableList.empty[Protocol]

          def dispatch(peer: PeerNode): Protocol => F[CommunicationResponse] =
            p => {
              received += p
              CommunicationResponse.handledWithMessage(ProtocolHelper.pong(peer)).pure[F]
            }

          val (local, remote, result) = twoNodes(dispatch)(roundTripWithPing)

          inside(result) {
            case Right(protocol1) =>
              val sender = ProtocolHelper.sender(protocol1)
              sender shouldBe 'defined
              sender.get.toAddress shouldEqual remote.toAddress
              protocol1.message shouldBe 'pong
          }

          received should have length 1
          val protocol2 = received.head
          val sender    = ProtocolHelper.sender(protocol2)
          sender shouldBe 'defined
          sender.get.toAddress shouldEqual local.toAddress
          protocol2.message shouldBe 'ping
        }
      }

      "response takes to long" should {
        "fail with a timeout" in {

          def dispatch(peer: PeerNode): Protocol => F[CommunicationResponse] =
            _ => {
              Thread.sleep(500)
              CommunicationResponse.handledWithMessage(ProtocolHelper.pong(peer)).pure[F]
            }

          val (_, _, result) = twoNodes(dispatch)(roundTripWithPingAndTimeout(_, _, _, 100.millis))

          result shouldBe 'left
          val error = result.left.get
          error shouldEqual TimeOut
        }
      }

      "there is no response body" should {
        "fail with a communication error" in {

          def dispatch(peer: PeerNode): Protocol => F[CommunicationResponse] =
            _ => CommunicationResponse.handledWitoutMessage.pure[F]

          val (_, _, result) = twoNodes(dispatch)(roundTripWithPing)

          result shouldBe 'left
          val error = result.left.get
          error shouldEqual InternalCommunicationError("Was expecting message, nothing arrived")
        }
      }

      "peer is not listening" should {
        "fail with peer unavailable error" in {

          val (_, remote, result) = twoNodesRemoteDead(roundTripWithPing)

          result shouldBe 'left
          val error = result.left.get
          error shouldEqual PeerUnavailable(remote)
        }
      }

      "there was a peer-side error" should {
        "fail with an internal communication error" in {

          def dispatch(peer: PeerNode): Protocol => F[CommunicationResponse] =
            _ => CommunicationResponse.notHandled(InternalCommunicationError("Test")).pure[F]

          val (_, _, result) = twoNodes(dispatch)(roundTripWithPing)

          result shouldBe 'left
          val error = result.left.get
          error shouldEqual InternalCommunicationError(
            "Got response: Internal communication error. Test")
        }
      }
    }

    "sending a message" should {
      "deliver the message" in {

        val received = mutable.MutableList.empty[Protocol]
        val latch    = new java.util.concurrent.CountDownLatch(1)

        def dispatch(peer: PeerNode): Protocol => F[CommunicationResponse] =
          p => {
            received += p
            latch.countDown()
            CommunicationResponse.handledWitoutMessage.pure[F]
          }

        val (local, _, _) = twoNodes(dispatch) { (tl, local, remote) =>
          for {
            r <- sendPing(tl, local, remote)
            _ = latch.await(1, TimeUnit.SECONDS)
          } yield r
        }

        received should have length 1
        val protocol2 = received.head
        val sender    = ProtocolHelper.sender(protocol2)
        sender shouldBe 'defined
        sender.get.toAddress shouldEqual local.toAddress
        protocol2.message shouldBe 'ping
      }

      "not wait for a response" in {

        val latch     = new java.util.concurrent.CountDownLatch(1)
        var processed = 0L

        def dispatch(peer: PeerNode): Protocol => F[CommunicationResponse] =
          p => {
            latch.countDown()
            processed = System.currentTimeMillis()
            CommunicationResponse.handledWitoutMessage.pure[F]
          }

        val (_, _, sent) = twoNodes(dispatch) { (tl, local, remote) =>
          for {
            _ <- sendPing(tl, local, remote)
            t = System.currentTimeMillis()
            _ = latch.await(1, TimeUnit.SECONDS)
          } yield t
        }

        sent should be < processed
      }

      "wait for message being delivered" in {
        // future feature, not yet implemented
        pending
      }

    }

    "brodacasting a message" should {
      "send the message to all peers" in {

        val received = mutable.MutableList.empty[(PeerNode, Protocol)]
        val latch    = new java.util.concurrent.CountDownLatch(2)

        def dispatch(peer: PeerNode): Protocol => F[CommunicationResponse] =
          p => {
            received.synchronized(received += ((peer, p)))
            latch.countDown()
            CommunicationResponse.handledWitoutMessage.pure[F]
          }

        val (local, remote1, remote2, _) = threeNodes(dispatch) { (tl, local, remote1, remote2) =>
          for {
            r <- broadcastPing(tl, local, remote1, remote2)
            _ = latch.await(1, TimeUnit.SECONDS)
          } yield r
        }

        received should have length 2
        val Seq((r1, p1), (r2, p2)) = received
        val sender1                 = ProtocolHelper.sender(p1)
        val sender2                 = ProtocolHelper.sender(p2)
        sender1 shouldBe 'defined
        sender2 shouldBe 'defined
        sender1.get.toAddress shouldEqual local.toAddress
        sender2.get.toAddress shouldEqual local.toAddress
        p1.message shouldBe 'ping
        p2.message shouldBe 'ping
        r1 should (equal(remote1) or equal(remote2))
        r2 should (equal(remote1) or equal(remote2))
      }
    }

    "shutting down" when {
      "doing a round trip" should {
        "not send the message" in {

          val received = mutable.MutableList.empty[Protocol]

          def dispatch(peer: PeerNode): Protocol => F[CommunicationResponse] =
            p => {
              received += p
              CommunicationResponse.handledWithMessage(ProtocolHelper.pong(peer)).pure[F]
            }

          val (_, _, result) = twoNodes(dispatch) { (tl, local, remote) =>
            for {
              _ <- tl.shutdown(ProtocolHelper.disconnect(local))
              r <- roundTripWithPing(tl, local, remote)
            } yield r
          }

          inside(result) {
            case Left(ProtocolException(e)) =>
              e.getMessage shouldEqual "The transport layer has been shut down"
          }

          received shouldBe 'empty
        }
      }

      "sending a message" should {
        "not send the message" in {

          val received = mutable.MutableList.empty[Protocol]
          val latch    = new java.util.concurrent.CountDownLatch(1)

          def dispatch(peer: PeerNode): Protocol => F[CommunicationResponse] =
            p => {
              received += p
              latch.countDown()
              CommunicationResponse.handledWitoutMessage.pure[F]
            }

          twoNodes(dispatch) { (tl, local, remote) =>
            for {
              _ <- tl.shutdown(ProtocolHelper.disconnect(local))
              r <- sendPing(tl, local, remote)
              _ = latch.await(1, TimeUnit.SECONDS)
            } yield r
          }

          received shouldBe 'empty
        }
      }

      "broadcasting a message" should {
        "not send any messages" in {

          val received = mutable.MutableList.empty[(PeerNode, Protocol)]
          val latch    = new java.util.concurrent.CountDownLatch(2)

          def dispatch(peer: PeerNode): Protocol => F[CommunicationResponse] =
            p => {
              received.synchronized(received += ((peer, p)))
              latch.countDown()
              CommunicationResponse.handledWitoutMessage.pure[F]
            }

          threeNodes(dispatch) { (tl, local, remote1, remote2) =>
            for {
              _ <- tl.shutdown(ProtocolHelper.disconnect(local))
              r <- broadcastPing(tl, local, remote1, remote2)
              _ = latch.await(1, TimeUnit.SECONDS)
            } yield r
          }

          received shouldBe 'empty
        }
      }
    }
  }

  val nextPort = new AtomicInteger(41000)

  def createEnvironment(port: Int): F[E]

  def createTransportLayer(env: E): F[TransportLayer[F]]

  def extract[A](fa: F[A]): A

  private def roundTripWithPing(transportLayer: TransportLayer[F],
                                local: PeerNode,
                                remote: PeerNode): F[CommErr[Protocol]] =
    roundTripWithPingAndTimeout(transportLayer, local, remote)

  private def roundTripWithPingAndTimeout(
      transportLayer: TransportLayer[F],
      local: PeerNode,
      remote: PeerNode,
      timeout: FiniteDuration = 1.second): F[CommErr[Protocol]] =
    transportLayer.roundTrip(remote, ProtocolHelper.ping(local), timeout)

  private def sendPing(transportLayer: TransportLayer[F],
                       local: PeerNode,
                       remote: PeerNode): F[Unit] =
    transportLayer.send(remote, ProtocolHelper.ping(local))

  private def broadcastPing(transportLayer: TransportLayer[F],
                            local: PeerNode,
                            remotes: PeerNode*): F[Unit] =
    transportLayer.broadcast(remotes, ProtocolHelper.ping(local))

  private def twoNodesEnvironment[A](block: (E, E) => F[A]): F[A] =
    for {
      e1 <- createEnvironment(nextPort.incrementAndGet())
      e2 <- createEnvironment(nextPort.incrementAndGet())
      r  <- block(e1, e2)
    } yield r

  private def threeNodesEnvironment[A](block: (E, E, E) => F[A]): F[A] =
    for {
      e1 <- createEnvironment(nextPort.incrementAndGet())
      e2 <- createEnvironment(nextPort.incrementAndGet())
      e3 <- createEnvironment(nextPort.incrementAndGet())
      r  <- block(e1, e2, e3)
    } yield r

  private def twoNodes[A](dispatch: PeerNode => Protocol => F[CommunicationResponse])(
      block: (TransportLayer[F], PeerNode, PeerNode) => F[A]
  ): (PeerNode, PeerNode, A) =
    extract(
      twoNodesEnvironment { (e1, e2) =>
        for {
          localTl  <- createTransportLayer(e1)
          remoteTl <- createTransportLayer(e2)
          local    = e1.peer
          remote   = e2.peer
          _        <- remoteTl.receive(dispatch(remote))
          r        <- block(localTl, local, remote)
          _        <- remoteTl.shutdown(ProtocolHelper.disconnect(remote))
          _        <- localTl.shutdown(ProtocolHelper.disconnect(local))
        } yield (local, remote, r)
      }
    )

  private def twoNodesRemoteDead[A](
      block: (TransportLayer[F], PeerNode, PeerNode) => F[A]): (PeerNode, PeerNode, A) =
    extract(
      twoNodesEnvironment { (e1, e2) =>
        for {
          localTl <- createTransportLayer(e1)
          local   = e1.peer
          remote  = e2.peer
          r       <- block(localTl, local, remote)
          _       <- localTl.shutdown(ProtocolHelper.disconnect(local))
        } yield (local, remote, r)
      }
    )

  private def threeNodes[A](dispatch: PeerNode => Protocol => F[CommunicationResponse])(
      block: (TransportLayer[F], PeerNode, PeerNode, PeerNode) => F[A]
  ): (PeerNode, PeerNode, PeerNode, A) =
    extract(
      threeNodesEnvironment { (e1, e2, e3) =>
        for {
          localTl   <- createTransportLayer(e1)
          remoteTl1 <- createTransportLayer(e2)
          remoteTl2 <- createTransportLayer(e3)
          local     = e1.peer
          remote1   = e2.peer
          remote2   = e3.peer
          _         <- remoteTl1.receive(dispatch(remote1))
          _         <- remoteTl2.receive(dispatch(remote2))
          r         <- block(localTl, local, remote1, remote2)
          _         <- remoteTl1.shutdown(ProtocolHelper.disconnect(remote1))
          _         <- remoteTl2.shutdown(ProtocolHelper.disconnect(remote2))
          _         <- localTl.shutdown(ProtocolHelper.disconnect(local))
        } yield (local, remote1, remote2, r)
      }
    )
}

trait Environment {
  def peer: PeerNode
  def host: String
  def port: Int
}
