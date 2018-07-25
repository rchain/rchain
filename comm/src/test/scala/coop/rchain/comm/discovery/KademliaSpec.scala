package coop.rchain.comm.discovery

import scala.collection.mutable

import cats.Id

import coop.rchain.catscontrib._, Catscontrib._
import coop.rchain.comm._

import org.scalatest._

class KademliaSpec extends FunSpec with Matchers with BeforeAndAfterEach {
  val endpoint = Endpoint("local", 0, 0)
  val local    = PeerNode(NodeIdentifier(Seq("00000001".b)), endpoint)
  val peer0    = PeerNode(NodeIdentifier(Seq("00000010".b)), endpoint)
  val peer1    = PeerNode(NodeIdentifier(Seq("00001000".b)), endpoint)
  val peer2    = PeerNode(NodeIdentifier(Seq("00001001".b)), endpoint)
  val peer3    = PeerNode(NodeIdentifier(Seq("00001010".b)), endpoint)
  val peer4    = PeerNode(NodeIdentifier(Seq("00001100".b)), endpoint)

  val DISTANCE_4 = Some(4)
  val DISTANCE_6 = Some(6)

  var table       = PeerTable(local, 3)
  var pingedPeers = mutable.MutableList.empty[PeerNode]

  override def beforeEach(): Unit = {
    table = PeerTable(local, 3)
    pingedPeers = mutable.MutableList.empty[PeerNode]

    // peer1-4 distance is 4
    table.distance(peer1) shouldBe DISTANCE_4
    table.distance(peer2) shouldBe DISTANCE_4
    table.distance(peer3) shouldBe DISTANCE_4
    table.distance(peer4) shouldBe DISTANCE_4
    // peer0 distance is 6
    table.distance(peer0) shouldBe DISTANCE_6
  }

  describe("A PeertTable with 1 byte addresses and k = 3") {
    describe("when adding a peer to an empty table") {
      it("should add it to a bucket according to its distance") {
        // given
        implicit val ping: Ping[Id] = pingOk
        // when
        table.observe[Id](peer0)
        // then
        bucketEntriesAt(DISTANCE_6) shouldEqual Seq(peer0)
      }

      it("should not ping the peer") {
        // given
        implicit val ping: Ping[Id] = pingOk
        // when
        table.observe[Id](peer0)
        // then
        pingedPeers shouldEqual Seq.empty[PeerNode]
      }
    }

    describe("when adding a peer to a table, where corresponding bucket is full") {
      it("should ping the oldest peer to check if it responds") {
        // given
        implicit val ping: Ping[Id] = pingOk
        thatBucket4IsFull
        // when
        table.observe[Id](peer4)
        // then
        pingedPeers shouldEqual Seq(peer1)
      }

      describe("and oldest peer IS responding to ping") {
        it("should drop the new peer") {
          // given
          implicit val ping: Ping[Id] = pingOk
          thatBucket4IsFull
          // when
          table.observe[Id](peer4)
          // then
          bucketEntriesAt(DISTANCE_4) shouldEqual Seq(peer2, peer3, peer1)
        }
      }
      describe("and oldest peer is NOT responding to ping") {
        it("should add the new peer and drop the oldest one") {
          // given
          implicit val ping: Ping[Id] = pingFail
          thatBucket4IsFull
          // when
          table.observe[Id](peer4)
          // then
          bucketEntriesAt(DISTANCE_4) shouldEqual Seq(peer2, peer3, peer4)
        }
      }
    }
  }

  private def thatBucket4IsFull(implicit ev: Ping[Id]): Unit = {
    table.observe[Id](peer1)
    table.observe[Id](peer2)
    table.observe[Id](peer3)
  }

  private def bucketEntriesAt(distance: Option[Int]): Seq[PeerNode] =
    distance.map(d => table.table(d).map(_.entry)).getOrElse(Seq.empty[PeerNode])

  private val pingOk: Ping[Id] = (pn: PeerNode) => {
    pingedPeers += pn
    true
  }

  private val pingFail: Ping[Id] = (pn: PeerNode) => {
    pingedPeers += pn
    false
  }

}
