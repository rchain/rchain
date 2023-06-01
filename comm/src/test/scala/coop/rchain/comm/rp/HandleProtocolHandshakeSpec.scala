package coop.rchain.comm.rp

import cats.effect.{Async, IO}
import cats.syntax.all._
import coop.rchain.comm._
import coop.rchain.comm.rp.Connect._
import coop.rchain.metrics.Metrics
import coop.rchain.p2p.EffectsTestInstances._
import coop.rchain.shared._
import coop.rchain.shared.scalatestcontrib.convertToAnyShouldWrapper
import fs2.concurrent.Channel
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import cats.effect.Ref
import cats.effect.unsafe.implicits.global

class HandleProtocolHandshakeSpec extends AnyFlatSpec with ScalaCheckPropertyChecks {

  implicit private val logEffTest   = new Log.NOPLog[IO]
  implicit private val metricEffEff = new Metrics.MetricsNOP[IO]

  val validConnections = Table(
    ("src", "remote"),
    // Both local
    // 0.0.0.0
    ("0.0.0.0", "0.0.0.0"),
    ("0.0.0.0", "127.114.61.218"),
    ("0.0.0.0", "10.3.103.123"),
    ("0.0.0.0", "172.30.209.12"),
    ("0.0.0.0", "192.168.1.184"),
    // 127.0.0.0/8
    ("127.37.107.115", "0.0.0.0"),
    ("127.37.107.115", "127.114.61.218"),
    ("127.37.107.115", "10.3.103.123"),
    ("127.37.107.115", "172.30.209.12"),
    ("127.37.107.115", "192.168.1.184"),
    // 10.0.0.0/8
    ("10.84.153.6", "0.0.0.0"),
    ("10.84.153.6", "127.114.61.218"),
    ("10.84.153.6", "10.3.103.123"),
    ("10.84.153.6", "172.30.209.12"),
    ("10.84.153.6", "192.168.1.184"),
    // 172.16.0.0/12
    ("172.30.123.1", "0.0.0.0"),
    ("172.30.123.1", "127.114.61.218"),
    ("172.30.123.1", "10.3.103.123"),
    ("172.30.123.1", "172.30.209.12"),
    ("172.30.123.1", "192.168.1.184"),
    // 192.168.0.0/16
    ("192.168.1.121", "0.0.0.0"),
    ("192.168.1.121", "127.114.61.218"),
    ("192.168.1.121", "10.3.103.123"),
    ("192.168.1.121", "172.30.209.12"),
    ("192.168.1.121", "192.168.1.184"),
    // Both public
    ("205.115.163.25", "151.46.21.153"),
    ("14.239.205.45", "166.116.242.146"),
    ("35.169.51.192", "79.23.150.252"),
    ("35.133.188.29", "241.184.72.49"),
    ("120.124.224.240", "249.250.80.142")
  )

  "peer connection" should "be accepted if in the same subnetwork" in {
    forAll(validConnections) {
      case (srcHost, remoteHost) =>
        val src    = peerNode(srcHost)
        val remote = peerNode(remoteHost)
        val run = for {
          conn <- tryToHandshake[IO](src, remote)

          _ = conn.size shouldBe 1
        } yield ()

        run.unsafeRunSync()
    }
  }

  val inValidConnections = Table(
    ("src", "remote"),
    // Node is on local network, remote peer on public
    ("0.0.0.0", "151.46.21.153"),
    ("127.114.61.218", "166.116.242.146"),
    ("10.3.103.123", "79.23.150.252"),
    ("172.30.209.12", "241.184.72.49"),
    ("192.168.1.184", "249.250.80.142"),
    // Node is on public network, remote peer on local
    ("205.115.163.25", "0.0.0.0"),
    ("14.239.205.45", "127.114.61.218"),
    ("35.169.51.192", "10.3.103.123"),
    ("35.133.188.29", "172.30.209.12"),
    ("120.124.224.240", "192.168.1.184")
  )

  it should "be rejected if not in the same subnetwork" in {
    forAll(inValidConnections) {
      case (srcHost, remoteHost) =>
        val src    = peerNode(srcHost)
        val remote = peerNode(remoteHost)
        val run = for {
          conn <- tryToHandshake[IO](src, remote)

          _ = conn.size shouldBe 0
        } yield ()

        run.unsafeRunSync()
    }
  }

  private def tryToHandshake[F[_]: Async: Log: Metrics](
      srcPeer: PeerNode,
      remotePeer: PeerNode
  ): F[Connections] = {
    // Init local (src) node connection config
    implicit val rpConfAsk = createRPConfAsk[F](srcPeer)

    // Init transport layer (set response to succeed always)
    implicit val transportLayerEff = new TransportLayerStub[F]
    transportLayerEff.setResponses(_ => _ => ().asRight)

    // Init connections Ref
    implicit val connectionRef = Ref.unsafe(Connect.Connections.empty)

    for {
      routingMessageQueue <- Channel.unbounded[F, RoutingMessage]

      // Remote peer protocol handshake message
      protocol = ProtocolHelper.protocolHandshake(remotePeer, networkId = "test-network")

      // Handle remote message
      _ <- HandleMessages.handle[F](protocol, routingMessageQueue)

      // Remote peer is added to connections if from allowed host
      connections <- connectionRef.get
    } yield connections
  }

  private def peerNode(host: String): PeerNode =
    PeerNode(
      NodeIdentifier("node-name".getBytes.toIndexedSeq),
      Endpoint(host, tcpPort = 0, udpPort = 0)
    )
}
