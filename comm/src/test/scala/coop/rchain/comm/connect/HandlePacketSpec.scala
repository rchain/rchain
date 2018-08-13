package coop.rchain.comm.rp

import coop.rchain.comm.protocol.rchain.Packet
import coop.rchain.p2p.effects.PacketHandler
import Connect._, Connections._
import coop.rchain.comm.transport.BlockMessage
import com.google.protobuf.ByteString
import coop.rchain.comm._, CommError._, protocol.routing._
import coop.rchain.p2p.EffectsTestInstances._
import coop.rchain.metrics.Metrics
import scala.concurrent.duration._
import org.scalatest._
import org.scalatest.enablers.Containing
import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._, ski._
import coop.rchain.shared._
import coop.rchain.comm.transport._, CommMessages._

class HandlePacketSpec extends FunSpec with Matchers with BeforeAndAfterEach with AppendedClues {

  import ScalaTestCats._

  type Effect[A] = EitherT[Id, CommError, A]

  val src: PeerNode              = peer("src")
  val remote: PeerNode           = peer("remote")
  val deftimeout: FiniteDuration = FiniteDuration(1, MILLISECONDS)
  val packetInProtocol           = packet(remote, BlockMessage, ByteString.copyFromUtf8("content"))
  val thePacket                  = toPacket(packetInProtocol).right.get
  implicit val transport         = new TransportLayerStub[Id]
  implicit val log               = new Log.NOPLog[Id]
  implicit val time              = new LogicalTime[Id]
  implicit val metric            = new Metrics.MetricsNOP[Id]
  implicit val nodeDiscovery     = new NodeDiscoveryStub[Effect]()
  implicit val rpConf            = conf(defaultTimeout = deftimeout)
  implicit val packetHandler: PacketHandler[Id] =
    (peer: PeerNode, packet: Packet) => {
      packetsProcessed = packet :: packetsProcessed
      Some(packet)
    }

  var packetsProcessed: List[Packet] = List.empty[Packet]

  override def beforeEach(): Unit =
    packetsProcessed = List.empty[Packet]

  describe("When Node receives a Packet message") {
    describe("and remote peer sending the Packet went through the protocol handshake") {
      it("then packet is processed") {
        // given
        implicit val connections = mkConnections(remote)
        // when
        HandleMessages.handlePacket[Effect](remote, Some(thePacket))
        // then
        packetsProcessed should contain(thePacket)
      }
    }

    describe("and remote peer sending the Packet did NOT went through the protocol handshake") {
      it("then packet is not processed") {
        // given
        implicit val connections = mkConnections()
        // when
        HandleMessages.handlePacket[Effect](remote, Some(thePacket))
        // then
        packetsProcessed should not contain (thePacket)
      }
      it("should return not handled response") {
        // given
        implicit val connections = mkConnections()
        // when
        val result = HandleMessages.handlePacket[Effect](remote, Some(thePacket)).value
        // then
        result shouldBe (Right(NotHandled(NotConnectedToPeer(remote))))
      }
    }
  }

  private def peer(name: String): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), Endpoint("host", 80, 80))

  private def mkConnections(peers: PeerNode*): ConnectionsCell[Id] =
    Cell.id[Connections](peers.reverse.foldLeft(Connections.empty) {
      case (acc, el) => acc.addConn[Id](el)
    })

  private def conf(
      maxNumOfConnections: Int = 5,
      numOfConnectionsPinged: Int = 5,
      defaultTimeout: FiniteDuration
  ): RPConfAsk[Id] =
    new ConstApplicativeAsk(
      RPConf(clearConnections = ClearConnetionsConf(maxNumOfConnections, numOfConnectionsPinged),
             defaultTimeout = defaultTimeout,
             local = peer("src"))
    )

  implicit def eiterTrpConfAsk: RPConfAsk[Effect] =
    new EitherTApplicativeAsk[Id, RPConf, CommError]

}
