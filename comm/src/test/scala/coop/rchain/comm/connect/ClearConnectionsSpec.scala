package coop.rchain.comm.rp

import Connect._, Connections._
import coop.rchain.comm._

import org.scalatest._
import org.scalatest.enablers.Containing
import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._, ski._
import coop.rchain.shared._

class ClearConnectionsSpec
    extends FunSpec
    with Matchers
    with BeforeAndAfterEach
    with AppendedClues {

  import ScalaTestCats._

  describe("Node when called to clear connectios") {
    describe("if number of connectons is smaller or equal to 2/3 of number of maximum connectons allowed") {
      it("should not clear any of existing connections") {
        // given
        implicit val connections = mkConnections(peer("A"), peer("B"))
        implicit val max         = maxNumOfConnections(5)
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
        implicit val max         = maxNumOfConnections(5)
        // when
        val cleared = Connect.clearConnections[Id]
        // then
        cleared shouldBe (0)
      }
    }

    describe("if number of connectons is bigger then 2/3 of number of maximum connectons allowed") {

    }
  }

  private def peer(name: String): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), Endpoint("host", 80, 80))

  private def mkConnections(peers: PeerNode*): ConnectionsCell[Id] =
    Cell.const[Id, Connections](peers.foldLeft(Connections.empty) {
      case (acc, el) => acc.addConn[Id](el)
    })

  private def maxNumOfConnections(num: Int): RPConfAsk[Id] =
    new ConstApplicativeAsk(RPConf(maxNumOfConnections = 5))

}
