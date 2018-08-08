package coop.rchain.comm.transport

import java.util.concurrent.TimeUnit

import scala.collection.mutable
import scala.concurrent.duration._

import cats._
import cats.implicits._

import coop.rchain.comm._
import coop.rchain.comm.protocol.routing.Protocol

import org.scalatest._

abstract class TransportLayerSpec[F[_]: Monad, E <: Environment]()
    extends WordSpec
    with Matchers
    with Inside {

  val transportLayerName: String = this.getClass.getSimpleName.replace("Spec", "")

  def createEnvironment(port: Int): F[E]

  def createTransportLayer(env: E): F[TransportLayer[F]]

  def extract[A](fa: F[A]): A

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

          val resultF =
            for {
              e1  <- createEnvironment(41001)
              e2  <- createEnvironment(41002)
              tl1 <- createTransportLayer(e1)
              tl2 <- createTransportLayer(e2)
              _   <- tl1.receive(dispatch(e1.peer))
              r   <- tl2.roundTrip(e1.peer, ProtocolHelper.ping(e2.peer), 1.second)
              _   <- tl1.shutdown(ProtocolHelper.disconnect(e1.peer))
              _   <- tl2.shutdown(ProtocolHelper.disconnect(e2.peer))
            } yield (e1.peer, e2.peer, r)

          val (p1, p2, result) = extract(resultF)

          inside(result) {
            case Right(protocol1) =>
              val sender = ProtocolHelper.sender(protocol1)
              sender shouldBe 'defined
              sender.get.toAddress shouldEqual p1.toAddress
              protocol1.message shouldBe 'pong
          }

          received.length shouldEqual 1
          val protocol2 = received.head
          val sender    = ProtocolHelper.sender(protocol2)
          sender shouldBe 'defined
          sender.get.toAddress shouldEqual p2.toAddress
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

          val resultF =
            for {
              e1  <- createEnvironment(41003)
              e2  <- createEnvironment(41004)
              tl1 <- createTransportLayer(e1)
              tl2 <- createTransportLayer(e2)
              _   <- tl1.receive(dispatch(e1.peer))
              r   <- tl2.roundTrip(e1.peer, ProtocolHelper.ping(e2.peer), 100.millis)
              _   <- tl1.shutdown(ProtocolHelper.disconnect(e1.peer))
              _   <- tl2.shutdown(ProtocolHelper.disconnect(e2.peer))
            } yield r

          val result = extract(resultF)
          result shouldBe 'left
          val error = result.left.get
          error shouldEqual TimeOut
        }
      }

      "there is no response body" should {
        "fail with a communication error" in {
          def dispatch(peer: PeerNode): Protocol => F[CommunicationResponse] =
            _ => {
              CommunicationResponse.handledWitoutMessage.pure[F]
            }

          val resultF =
            for {
              e1  <- createEnvironment(41005)
              e2  <- createEnvironment(41006)
              tl1 <- createTransportLayer(e1)
              tl2 <- createTransportLayer(e2)
              _   <- tl1.receive(dispatch(e1.peer))
              r   <- tl2.roundTrip(e1.peer, ProtocolHelper.ping(e2.peer), 100.millis)
              _   <- tl1.shutdown(ProtocolHelper.disconnect(e1.peer))
              _   <- tl2.shutdown(ProtocolHelper.disconnect(e2.peer))
            } yield r

          val result = extract(resultF)
          result shouldBe 'left
          val error = result.left.get
          error shouldEqual InternalCommunicationError("Was expecting message, nothing arrived")
        }
      }

      "peer is not listening" should {
        "fail with peer unavailable error" in {
          val resultF =
            for {
              e1  <- createEnvironment(41007)
              e2  <- createEnvironment(41008)
              tl2 <- createTransportLayer(e2)
              r   <- tl2.roundTrip(e1.peer, ProtocolHelper.ping(e2.peer), 100.millis)
              _   <- tl2.shutdown(ProtocolHelper.disconnect(e2.peer))
            } yield (e1.peer, r)

          val (p1, result) = extract(resultF)
          result shouldBe 'left
          val error = result.left.get
          error shouldEqual PeerUnavailable(p1)
        }
      }

      "there was a peer-side error" should {
        "fail with an internal communication error" in {
          def dispatch(peer: PeerNode): Protocol => F[CommunicationResponse] =
            _ => {
              CommunicationResponse.notHandled(InternalCommunicationError("Test")).pure[F]
            }

          val resultF =
            for {
              e1  <- createEnvironment(41009)
              e2  <- createEnvironment(41010)
              tl1 <- createTransportLayer(e1)
              tl2 <- createTransportLayer(e2)
              _   <- tl1.receive(dispatch(e1.peer))
              r   <- tl2.roundTrip(e1.peer, ProtocolHelper.ping(e2.peer), 100.millis)
              _   <- tl1.shutdown(ProtocolHelper.disconnect(e1.peer))
              _   <- tl2.shutdown(ProtocolHelper.disconnect(e2.peer))
            } yield r

          val result = extract(resultF)
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

        val resultF =
          for {
            e1  <- createEnvironment(41001)
            e2  <- createEnvironment(41002)
            tl1 <- createTransportLayer(e1)
            tl2 <- createTransportLayer(e2)
            _   <- tl1.receive(dispatch(e1.peer))
            _   <- tl2.send(e1.peer, ProtocolHelper.ping(e2.peer))
            _   = latch.await(1, TimeUnit.SECONDS)
            _   <- tl1.shutdown(ProtocolHelper.disconnect(e1.peer))
            _   <- tl2.shutdown(ProtocolHelper.disconnect(e2.peer))
          } yield e2.peer

        val p2 = extract(resultF)

        received.length shouldEqual 1
        val protocol2 = received.head
        val sender    = ProtocolHelper.sender(protocol2)
        sender shouldBe 'defined
        sender.get.toAddress shouldEqual p2.toAddress
        protocol2.message shouldBe 'ping
      }

      "not wait for a response" in {
        pending
      }

      "wait for message being delivered" in {
        // future feature, not yet implemented
        pending
      }

    }

    "brodacasting a message" should {
      "send the message to all peers" in {
        pending
      }
    }

    "shutting down" when {
      "doing a round trip" should {
        "not send the message" in {
          pending
        }
      }

      "sending a message" should {
        "not send the message" in {
          pending
        }
      }

      "broadcasting a message" should {
        "not send any messages" in {
          pending
        }
      }
    }

  }
}

trait Environment {
  def peer: PeerNode
  def host: String
  def port: Int
}
