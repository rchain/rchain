package coop.rchain.comm.discovery

import scala.collection.mutable

import cats.Id

import coop.rchain.catscontrib._, Catscontrib._
import coop.rchain.comm._

import org.scalatest._

class KademliaSpec extends FlatSpec with Matchers with BeforeAndAfterEach {
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

  val pingOk: Ping[Id] = (pn: PeerNode) => {
    pingedPeers += pn
    true
  }

  val failSecondTime: Ping[Id] = new Ping[Id] {
    var pinged = List.empty[PeerNode]
    def ping(peer: PeerNode): Boolean = {
      val result = !pinged.contains(peer)
      pinged = peer :: pinged
      result
    }
  }

  "A PeertTable with 1 byte addresses and k = 3" should "add new peer to the 6th bucket" in {
    // given
    implicit val ping: Ping[Id] = pingOk
    // when
    table.observe[Id](peer0)
    // then
    table.distance(peer0) shouldBe DISTANCE_6
    bucketEntriesAt(DISTANCE_6) shouldEqual Seq(peer0)
  }

  it should "drop new peer if the bucket is full and the oldest peer is responding to ping" in {
    // given
    implicit val ping: Ping[Id] = pingOk
    table.observe[Id](peer1)
    table.observe[Id](peer2)
    table.observe[Id](peer3)
    // when
    table.observe[Id](peer4)
    // then
    pingedPeers shouldEqual Seq(peer1)
    bucketEntriesAt(DISTANCE_4) shouldEqual Seq(peer2, peer3, peer1)
  }

  it should "drop oldest peer and add new peer if the bucket is full and the oldest peer is not responding to ping" in {
    // given
    implicit val ping: Ping[Id] = failSecondTime
    table.observe[Id](peer1)
    table.observe[Id](peer2)
    table.observe[Id](peer3)
    // when
    table.observe[Id](peer4)
    // then
    pingedPeers shouldEqual Seq(peer1)
    bucketEntriesAt(DISTANCE_4) shouldEqual Seq(peer2, peer3, peer4)
  }

  private def bucketEntriesAt(distance: Option[Int]): Seq[PeerNode] =
    distance.map(d => table.table(d).map(_.entry)).getOrElse(Seq.empty[PeerNode])

}
