package coop.rchain.comm.rp

import scala.concurrent.duration._

import cats._

import coop.rchain.catscontrib._
import coop.rchain.catscontrib.ski._
import coop.rchain.comm._
import coop.rchain.comm.CommError._
import coop.rchain.comm.protocol.routing._
import coop.rchain.comm.rp.Connect._
import coop.rchain.comm.rp.ProtocolHelper._
import coop.rchain.metrics.Metrics
import coop.rchain.p2p.EffectsTestInstances.{LogicalTime, TransportLayerStub}
import coop.rchain.shared._

import org.scalatest._

class ResestConnectionsSpec
    extends FunSpec
    with Matchers
    with BeforeAndAfterEach
    with AppendedClues {

  val src: PeerNode      = peer("src")
  implicit val transport = new TransportLayerStub[Id]
  implicit val log       = new Log.NOPLog[Id]
  implicit val metric    = new Metrics.MetricsNOP[Id]
  implicit val time      = new LogicalTime[Id]

  override def beforeEach(): Unit = {
    transport.reset()
    transport.setResponses(kp(alwaysSuccess))
  }

  describe("if reset reconnections") {
    it("should disconnect from all peers and clear connections") {
      // given
      implicit val connections = mkConnections(peer("A"), peer("B"))
      implicit val rpconf =
        conf(maxNumOfConnections = 5)
      // when
      Connect.resetConnections[Id]
      // then
      connections.read.size shouldBe 0
      transport.requests.size shouldBe 2
      transport.requests.map(_.peer) should contain(peer("A"))
      transport.requests.map(_.peer) should contain(peer("B"))
      transport.requests.forall(_.msg.message.isDisconnect) shouldEqual true
      transport.disconnects.size shouldBe 2
      transport.disconnects should contain(peer("A"))
      transport.disconnects should contain(peer("B"))
    }
  }

  private def peer(name: String, host: String = "host"): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), Endpoint(host, 80, 80))

  private def mkConnections(peers: PeerNode*): ConnectionsCell[Id] =
    Cell.id[Connections](peers.toList)

  private def conf(
      maxNumOfConnections: Int,
      numOfConnectionsPinged: Int = 5
  ): RPConfAsk[Id] =
    new ConstApplicativeAsk(
      RPConf(
        clearConnections = ClearConnetionsConf(maxNumOfConnections, numOfConnectionsPinged),
        defaultTimeout = 1.milli,
        local = peer("src"),
        bootstrap = None
      )
    )

  def alwaysSuccess: Protocol => CommErr[Protocol] =
    kp(Right(heartbeat(peer("src"))))

}
