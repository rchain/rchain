package coop.rchain.comm.rp

import cats.effect.concurrent.Ref
import cats.{catsInstancesForId => _, _}
import coop.rchain.catscontrib.effect.implicits._
import coop.rchain.catscontrib.ski.kp
import coop.rchain.comm.CommError._
import coop.rchain.comm._
import coop.rchain.comm.protocol.routing.Protocol
import coop.rchain.comm.rp.Connect._
import coop.rchain.metrics.Metrics
import coop.rchain.p2p.EffectsTestInstances._
import coop.rchain.shared._
import coop.rchain.shared.scalatestcontrib.convertToAnyShouldWrapper
import fs2.concurrent.Queue
import org.scalatest.flatspec.AnyFlatSpec

class HandleProtocolHandshakeSpec extends AnyFlatSpec {
  private val currentLocalPeer  = peerNode("src", "127.0.0.1", 40400)
  private val currentInetPeer   = peerNode("src", "169.10.0.2", 40400)
  private val incomingLocalPeer = peerNode("remote", "0.0.0.0")
  private val incomingInetPeer  = peerNode("remote", "169.10.0.1")

  type Effect[A] = CommErrT[Id, A]
  implicit private val transportLayerEff = new TransportLayerStub[Effect]
  implicit private val logEffTest        = new Log.NOPLog[Effect]
  implicit private val connectionsCellEff =
    Ref.unsafe[Effect, Connections](Connect.Connections.empty)
  implicit private val metricEffEff = new Metrics.MetricsNOP[Effect]

  {
    implicit val rpConfAskLocalEff = createRPConfAsk[Effect](currentLocalPeer)
    "node with local address when connecting to other local node" should "accept connection" in {
      for {
        _           <- tryToHandshake(incomingLocalPeer)
        connections <- connectionsCellEff.get
        _           = connections.size shouldBe 1
      } yield ()
    }
    "node with local address when connecting to Internet node" should "decline connection" in {
      for {
        _           <- tryToHandshake(incomingInetPeer)
        connections <- connectionsCellEff.get

        _ = connections.size shouldBe 0
      } yield ()
    }
  }
  {
    implicit val rpConfAskInetEff = createRPConfAsk[Effect](currentInetPeer)
    "node with public Internet address when connecting to other Internet node" should "accept connection" in {
      for {
        _           <- tryToHandshake(incomingInetPeer)
        connections <- connectionsCellEff.get

        _ = connections.size shouldBe 1
      } yield ()
    }

    "node with public Internet address when connecting to local node" should "decline connection" in {
      for {
        _           <- tryToHandshake(incomingLocalPeer)
        connections <- connectionsCellEff.get

        _ = connections.size shouldBe 0
      } yield ()
    }
  }

  private def tryToHandshake(
      peer: PeerNode
  )(implicit rpConfAskEff: ConstApplicativeAsk[Effect, RPConf]) = {
    transportLayerEff.reset()
    Connect.resetConnections[Effect]

    val networkId = "test-network-id"
    val protocol  = ProtocolHelper.protocolHandshake(peer, networkId)
    transportLayerEff.setResponses(kp(alwaysSuccess))
    for {
      routingMessageQueue <- Queue.unbounded[Effect, RoutingMessage]

      _ <- HandleMessages.handle[Effect](protocol, routingMessageQueue)
    } yield ()
  }

  private def endpoint(host: String, tcpPort: Int, udpPort: Int): Endpoint =
    Endpoint(host, tcpPort, udpPort)
  private def peerNode(
      name: String,
      host: String,
      tcpPort: Int = 40401,
      udpPort: Int = 40402
  ): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), endpoint(host, tcpPort, udpPort))

  private def alwaysSuccess: Protocol => CommErr[Unit] = kp(Right(()))
}
