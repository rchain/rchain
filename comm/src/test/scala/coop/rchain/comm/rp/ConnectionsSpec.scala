package coop.rchain.comm.rp

import Connect._, Connections._
import coop.rchain.shared._
import coop.rchain.comm._, CommError._
import coop.rchain.p2p.effects._
import coop.rchain.metrics.Metrics
import cats.{catsInstancesForId => _, _}, cats.data._, cats.syntax.all._
import coop.rchain.catscontrib._, Catscontrib._, ski._
import coop.rchain.catscontrib.effect.implicits._
import coop.rchain.p2p.EffectsTestInstances._
import org.scalatest._

class ConnectionsSpec extends FunSpec with Matchers with BeforeAndAfterEach with AppendedClues {

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
              .addConn[Id](peer("A"))
              .addConn[Id](peer("B"))
              .addConn[Id](peer("C"))
          // then
          connections should equal(List(peer("A"), peer("B"), peer("C")))
        }

      }

      describe("if peer is on the list already") {
        it("should not add peer second time") {
          // when
          val connections =
            Connections.empty
              .addConn[Id](peer("A"))
              .addConn[Id](peer("A"))
          // then
          connections should equal(List(peer("A")))
        }

        it("should move peer to the end of the list") {
          // when
          val connections =
            Connections.empty
              .addConn[Id](peer("A"))
              .addConn[Id](peer("B"))
              .addConn[Id](peer("C"))
              .addConn[Id](peer("A"))
          // then
          connections should equal(List(peer("B"), peer("C"), peer("A")))
        }

        it("should update endpoint information") {
          // when
          val connections =
            Connections.empty
              .addConn[Id](peer("A", host = "10.10.0.1", port = 80))
              .addConn[Id](peer("B"))
              .addConn[Id](peer("A", host = "10.11.11.11", port = 8080))
          // then
          connections should equal(List(peer("B"), peer("A", host = "10.11.11.11", port = 8080)))
        }

      }

    }
    describe("removeConn") {
      it("should list unmodifed if connection not in the list") {
        val connections =
          Connections.empty
            .addConn[Id](peer("A"))
            .addConn[Id](peer("B"))
            .removeConn[Id](peer("C"))
        // then
        connections should equal(List(peer("A"), peer("B")))
      }

      it("should remove connection from the list if exists") {
        val connections =
          Connections.empty
            .addConn[Id](peer("A"))
            .addConn[Id](peer("B"))
            .addConn[Id](peer("C"))
            .removeConn[Id](peer("B"))
        // then
        connections should equal(List(peer("A"), peer("C")))
      }
    }
  }

  private def endpoint(host: String, port: Int): Endpoint = Endpoint(host, port, port)
  private def peer(name: String, host: String = "host", port: Int = 8080): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), endpoint(host, port))

}
