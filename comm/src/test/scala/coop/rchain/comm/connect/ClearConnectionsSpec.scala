package coop.rchain.comm.rp

import cats._
import coop.rchain.catscontrib._
import coop.rchain.catscontrib.ski._
import coop.rchain.comm.CommError._
import coop.rchain.comm._
import coop.rchain.comm.protocol.routing._
import coop.rchain.comm.rp.Connect.Connections._
import coop.rchain.comm.rp.Connect._
import coop.rchain.comm.rp.ProtocolHelper._
import coop.rchain.comm.transport._
import coop.rchain.metrics.Metrics
import coop.rchain.p2p.EffectsTestInstances.{LogicalTime, TransportLayerStub}
import coop.rchain.shared._
import org.scalatest._

import scala.concurrent.duration._

class ClearConnectionsSpec
    extends FunSpec
    with Matchers
    with BeforeAndAfterEach
    with AppendedClues {

  import ScalaTestCats._

  val src: PeerNode      = peer("src")
  implicit val transport = new TransportLayerStub[Id]
  implicit val log       = new Log.NOPLog[Id]
  implicit val metric    = new Metrics.MetricsNOP[Id]
  implicit val time      = new LogicalTime[Id]

  override def beforeEach(): Unit = {
    transport.reset()
    transport.setResponses(kp(alwaysSuccess))
  }

  describe("Node when called to clear connectios") {
    describe(
      "if number of connectons is smaller or equal to 2/3 of number of maximum connectons allowed"
    ) {
      it("should not clear any of existing connections") {
        // given
        implicit val connections = mkConnections(peer("A"), peer("B"))
        implicit val max         = conf(maxNumOfConnections = 5)
        // when
        Connect.clearConnections[Id]
        // then
        connections.read.size shouldBe (2)
        connections.read should contain(peer("A"))
        connections.read should contain(peer("B"))
      }
      it("should report that 0 connections were cleared") {
        // given
        implicit val connections = mkConnections(peer("A"), peer("B"))
        implicit val max         = conf(maxNumOfConnections = 5)
        // when
        val cleared = Connect.clearConnections[Id]
        // then
        cleared shouldBe (0)
      }
    }

    describe("if number of connectons is bigger then 2/3 of number of maximum connectons allowed") {
      it("should ping first few nodes with heartbeat") {
        // given
        implicit val connections = mkConnections(peer("A"), peer("B"), peer("C"), peer("D"))
        implicit val max         = conf(maxNumOfConnections = 5, numOfConnectionsPinged = 2)

        // when
        Connect.clearConnections[Id]
        // tehn
        transport.requests.size shouldBe (2)
        transport.requests.map(_.peer) should contain(peer("A"))
        transport.requests.map(_.peer) should contain(peer("B"))
      }

      it("should remove connections of peers that did not respond to heartbeat") {
        // given
        implicit val connections = mkConnections(peer("A"), peer("B"), peer("C"), peer("D"))
        implicit val max         = conf(maxNumOfConnections = 5, numOfConnectionsPinged = 2)
        transport.setResponses({
          case p if (p == peer("A")) => alwaysFail
          case _                     => alwaysSuccess
        })
        // when
        Connect.clearConnections[Id]
        // tehn
        connections.read.size shouldBe (3)
        connections.read should not contain (peer("A"))
        connections.read should contain(peer("B"))
        connections.read should contain(peer("C"))
        connections.read should contain(peer("D"))
      }

      it("should put the peers that responded to heartbeat to the end of the list") {
        // given
        implicit val connections = mkConnections(peer("A"), peer("B"), peer("C"), peer("D"))
        implicit val max         = conf(maxNumOfConnections = 5, numOfConnectionsPinged = 3)
        transport.setResponses({
          case p if (p == peer("A")) => alwaysFail
          case _                     => alwaysSuccess
        })
        // when
        Connect.clearConnections[Id]
        // tehn
        connections.read.size shouldBe (3)
        connections.read shouldEqual (List(peer("D"), peer("B"), peer("C")))
      }

      it("should report number of connections that were removed") {
        // given
        implicit val connections = mkConnections(peer("A"), peer("B"), peer("C"), peer("D"))
        implicit val max         = conf(maxNumOfConnections = 5, numOfConnectionsPinged = 3)
        transport.setResponses({
          case p if (p == peer("A")) => alwaysFail
          case _                     => alwaysSuccess
        })
        // when
        val cleared = Connect.clearConnections[Id]
        // tehn
        cleared shouldBe (1)
      }
    }
  }

  private def peer(name: String): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), Endpoint("host", 80, 80))

  private def mkConnections(peers: PeerNode*): ConnectionsCell[Id] =
    Cell.id[Connections](peers.toList)

  private def conf(maxNumOfConnections: Int, numOfConnectionsPinged: Int = 5): RPConfAsk[Id] =
    new ConstApplicativeAsk(
      RPConf(
        clearConnections = ClearConnetionsConf(maxNumOfConnections, numOfConnectionsPinged),
        defaultTimeout = FiniteDuration(1, MILLISECONDS),
        local = peer("src")
      )
    )

  def alwaysSuccess: Protocol => CommErr[Protocol] =
    kp(Right(heartbeat(peer("src"))))

  def alwaysFail: Protocol => CommErr[Protocol] =
    kp(Left(timeout))

}
