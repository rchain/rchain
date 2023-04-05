package coop.rchain.comm.rp

import cats.{catsInstancesForId => _, _}
import coop.rchain.catscontrib.effect.implicits._
import coop.rchain.catscontrib.ski._
import coop.rchain.comm.CommError._
import coop.rchain.comm._
import coop.rchain.comm.protocol.routing._
import coop.rchain.comm.rp.Connect._
import coop.rchain.metrics.Metrics
import coop.rchain.p2p.EffectsTestInstances.{LogicalTime, TransportLayerStub}
import coop.rchain.shared._
import org.scalatest._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._
import cats.effect.Ref

class ClearConnectionsSpec
    extends AnyFunSpec
    with Matchers
    with BeforeAndAfterEach
    with AppendedClues {

  import ScalaTestCats._

  val src: PeerNode      = peer("src")
  val networkId          = "test"
  implicit val transport = new TransportLayerStub[Id]
  implicit val log       = new Log.NOPLog[Id]
  implicit val metric    = new Metrics.MetricsNOP[Id]
  implicit val time      = new LogicalTime[Id]

  override def beforeEach(): Unit = {
    transport.reset()
    transport.setResponses(kp(alwaysSuccess))
  }

  describe("Node when called to clear connections") {
    describe(
      "if number of connections is smaller or equal to 2/3 of number of maximum connections allowed"
    ) {
      it("should not clear any of existing connections") {
        // given
        implicit val connections = mkConnections(peer("A"), peer("B"))
        implicit val rpconf      = conf(maxNumOfConnections = 5)
        // when
        Connect.clearConnections[Id]
        // then
        connections.get.size shouldBe 2
        connections.get should contain(peer("A"))
        connections.get should contain(peer("B"))
      }
      it("should report that 0 connections were cleared") {
        // given
        implicit val connections = mkConnections(peer("A"), peer("B"))
        implicit val rpconf      = conf(maxNumOfConnections = 5)
        // when
        val cleared = Connect.clearConnections[Id]
        // then
        cleared shouldBe 0
      }
    }

    describe("if number of connections is bigger then 2/3 of number of maximum connections allowed") {
      it("should ping first few nodes with heartbeat") {
        // given
        implicit val connections = mkConnections(peer("A"), peer("B"), peer("C"), peer("D"))
        implicit val rpconf      = conf(maxNumOfConnections = 5, numOfConnectionsPinged = 2)

        // when
        Connect.clearConnections[Id]
        // then
        transport.requests.size shouldBe 2
        transport.requests.map(_.peer) should contain(peer("A"))
        transport.requests.map(_.peer) should contain(peer("B"))
      }

      it("should remove connections of peers that did not respond to heartbeat") {
        // given
        implicit val connections = mkConnections(peer("A"), peer("B"), peer("C"), peer("D"))
        implicit val rpconf      = conf(maxNumOfConnections = 5, numOfConnectionsPinged = 2)
        transport.setResponses({
          case p if p == peer("A") => alwaysFail
          case _                   => alwaysSuccess
        })
        // when
        Connect.clearConnections[Id]
        // then
        connections.get.size shouldBe 3
        connections.get should not contain peer("A")
        connections.get should contain(peer("B"))
        connections.get should contain(peer("C"))
        connections.get should contain(peer("D"))
      }

      it("should put the peers that responded to heartbeat to the end of the list") {
        // given
        implicit val connections = mkConnections(peer("A"), peer("B"), peer("C"), peer("D"))
        implicit val rpconf      = conf(maxNumOfConnections = 5, numOfConnectionsPinged = 3)
        transport.setResponses({
          case p if p == peer("A") => alwaysFail
          case _                   => alwaysSuccess
        })
        // when
        Connect.clearConnections[Id]
        // then
        connections.get.size shouldBe 3
        connections.get shouldEqual List(peer("D"), peer("B"), peer("C"))
      }

      it("should report number of connections that were removed") {
        // given
        implicit val connections = mkConnections(peer("A"), peer("B"), peer("C"), peer("D"))
        implicit val rpconf      = conf(maxNumOfConnections = 5, numOfConnectionsPinged = 3)
        transport.setResponses({
          case p if p == peer("A") => alwaysFail
          case _                   => alwaysSuccess
        })
        // when
        val cleared = Connect.clearConnections[Id]
        // then
        cleared shouldBe 1
      }
    }
  }

  private def peer(name: String, host: String = "host"): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), Endpoint(host, 80, 80))

  private def mkConnections(peers: PeerNode*): ConnectionsCell[Id] =
    Ref.unsafe[Id, Connections](peers.toList)

  private def conf(
      maxNumOfConnections: Int,
      numOfConnectionsPinged: Int = 5
  ): RPConfAsk[Id] =
    new ConstApplicativeAsk(
      RPConf(
        clearConnections = ClearConnectionsConf(numOfConnectionsPinged),
        defaultTimeout = 1.milli,
        local = peer("src"),
        networkId = networkId,
        bootstrap = None,
        maxNumOfConnections = maxNumOfConnections
      )
    )

  def alwaysSuccess: Protocol => CommErr[Unit] = kp(Right(()))

  def alwaysFail: Protocol => CommErr[Unit] = kp(Left(timeout))

}
