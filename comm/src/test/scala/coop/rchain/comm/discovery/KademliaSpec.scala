package coop.rchain.comm.discovery

import scala.collection.mutable

import cats.Id

import coop.rchain.catscontrib.Capture
import coop.rchain.comm._

import org.scalatest._

class KademliaSpec extends FlatSpec with Matchers {
  val endpoint = Endpoint("local", 0, 0)
  val local    = PeerNode(NodeIdentifier(Seq("00000001".b)), endpoint)
  val peer0    = PeerNode(NodeIdentifier(Seq("00000010".b)), endpoint) // d = 6
  val peer1    = PeerNode(NodeIdentifier(Seq("00001000".b)), endpoint) // d = 4
  val peer2    = PeerNode(NodeIdentifier(Seq("00001001".b)), endpoint) // d = 4
  val peer3    = PeerNode(NodeIdentifier(Seq("00001010".b)), endpoint) // d = 4
  val peer4    = PeerNode(NodeIdentifier(Seq("00001100".b)), endpoint) // d = 4

  implicit val capture: Capture[Id] = new Capture[Id] {
    def capture[A](a: => A): Id[A]       = a
    def unsafeUncapture[A](fa: Id[A]): A = fa
  }

  // Distance is the XOR sequence of 0s counted from the left
  def distance(peer: PeerNode): Int = {
    val l = local.id.key.head // local byte
    val p = peer.id.key.head // peer byte
    val d = l ^ p // XOR
    // number of the most significant bit counted from the right
    val s = (0 to 7).foldLeft(0) {
      case (acc, i) =>
        if (d >> i > 0) acc + 1
        else acc
    }
    // count the distance from the left
    8 - s
  }

  "A PeertTable with 1 byte addresses and k = 3" should "add new peer to the right bucket" in {
    val table       = PeerTable(local, 3)
    val pingedPeers = mutable.MutableList.empty[PeerNode]
    implicit val ping: Ping[Id] = (pn: PeerNode) => {
      pingedPeers += pn
      true
    }
    table.observe[Id](peer0)

    val d = distance(peer0)
    table.distance(peer0) shouldBe Some(d)
    pingedPeers shouldEqual Seq(peer0)

    val entries = table.table(d).map(_.entry)
    entries shouldEqual Seq(peer0)
  }

  it should "drop new peer if the bucket is full and the oldest peer is responding to ping" in {
    val table       = PeerTable(local, 3)
    val pingedPeers = mutable.MutableList.empty[PeerNode]
    implicit val ping: Ping[Id] = (pn: PeerNode) => {
      pingedPeers += pn
      true
    }
    table.observe[Id](peer1)
    table.observe[Id](peer2)
    table.observe[Id](peer3)
    table.observe[Id](peer4)

    val d = distance(peer1)
    // peers 1-4 should have all the same distance to local
    table.distance(peer1) shouldBe Some(d)
    table.distance(peer2) shouldBe Some(d)
    table.distance(peer3) shouldBe Some(d)
    table.distance(peer4) shouldBe Some(d)
    pingedPeers shouldEqual Seq(peer1, peer2, peer3, peer4, peer1)

    val entries = table.table(d).map(_.entry)
    entries shouldEqual Seq(peer2, peer3, peer1)
  }

  it should "drop oldest peer and add new peer if the bucket is full and the oldest peer is not responding to ping" in {
    val table         = PeerTable(local, 3)
    val pingedPeers   = mutable.MutableList.empty[PeerNode]
    var failPingPeer1 = false
    implicit val ping: Ping[Id] = (pn: PeerNode) => {
      pingedPeers += pn
      !(failPingPeer1 && pn == peer1)
    }
    table.observe[Id](peer1)
    table.observe[Id](peer2)
    table.observe[Id](peer3)
    failPingPeer1 = true
    table.observe[Id](peer4)

    val d = distance(peer1)
    // peers 1-4 should have all the same distance to local
    table.distance(peer1) shouldBe Some(d)
    table.distance(peer2) shouldBe Some(d)
    table.distance(peer3) shouldBe Some(d)
    table.distance(peer4) shouldBe Some(d)
    pingedPeers shouldEqual Seq(peer1, peer2, peer3, peer4, peer1)

    val entries = table.table(d).map(_.entry)
    entries shouldEqual Seq(peer2, peer3, peer4)
  }
}
