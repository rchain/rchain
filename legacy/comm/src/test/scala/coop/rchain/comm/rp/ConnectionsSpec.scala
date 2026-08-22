package coop.rchain.comm.rp

import cats.{catsInstancesForId => _, _}
import coop.rchain.catscontrib.effect.implicits._
import coop.rchain.comm._
import coop.rchain.comm.rp.Connect.Connections._
import coop.rchain.comm.rp.Connect._
import coop.rchain.metrics.Metrics
import coop.rchain.p2p.EffectsTestInstances._
import coop.rchain.shared._
import org.scalatest._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ConnectionsSpec extends AnyFunSpec with Matchers with BeforeAndAfterEach with AppendedClues {

  implicit val logEff    = new Log.NOPLog[Id]
  implicit val timeEff   = new LogicalTime[Id]
  implicit val metricEff = new Metrics.MetricsNOP[Id]

  describe("Connections") {
    describe("addConn") {
      describe("if peer is not on the list yet") {
        it("should add new peers at the end of the list") {
          // when
          val connections =
            Connections.empty
              .addConn(peer("A"))
              .addConn(peer("B"))
              .addConn(peer("C"))
          // then
          connections should equal(List(peer("A"), peer("B"), peer("C")))
        }

      }

      describe("if peer is on the list already") {
        it("should not add peer second time") {
          // when
          val connections =
            Connections.empty
              .addConn(peer("A"))
              .addConn(peer("A"))
          // then
          connections should equal(List(peer("A")))
        }

        it("should move peer to the end of the list") {
          // when
          val connections =
            Connections.empty
              .addConn(peer("A"))
              .addConn(peer("B"))
              .addConn(peer("C"))
              .addConn(peer("A"))
          // then
          connections should equal(List(peer("B"), peer("C"), peer("A")))
        }

        it("should update endpoint information") {
          // when
          val connections =
            Connections.empty
              .addConn(peer("A", host = "10.10.0.1", port = 80))
              .addConn(peer("B"))
              .addConn(peer("A", host = "10.11.11.11", port = 8080))
          // then
          connections should equal(List(peer("B"), peer("A", host = "10.11.11.11", port = 8080)))
        }

      }

    }
    describe("removeConn") {
      it("should list unmodifed if connection not in the list") {
        val connections =
          Connections.empty
            .addConn(peer("A"))
            .addConn(peer("B"))
            .removeConn(peer("C"))
        // then
        connections should equal(List(peer("A"), peer("B")))
      }

      it("should remove connection from the list if exists") {
        val connections =
          Connections.empty
            .addConn(peer("A"))
            .addConn(peer("B"))
            .addConn(peer("C"))
            .removeConn(peer("B"))
        // then
        connections should equal(List(peer("A"), peer("C")))
      }
    }
  }

  private def endpoint(host: String, port: Int): Endpoint = Endpoint(host, port, port)
  private def peer(name: String, host: String = "host", port: Int = 8080): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), endpoint(host, port))

}
