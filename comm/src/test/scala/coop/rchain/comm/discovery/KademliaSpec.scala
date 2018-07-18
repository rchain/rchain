package coop.rchain.comm.discovery

import scala.collection.mutable

import cats.Id

import coop.rchain.catscontrib.Capture
import coop.rchain.comm._

import org.scalatest._

class KademliaSpec extends FlatSpec with Matchers {
  val endpoint = Endpoint("local", 0, 0)
  val local    = PeerNode(NodeIdentifier(Seq(1.toByte)), endpoint)

  implicit val caputre: Capture[Id] = new Capture[Id] {
    def capture[A](a: => A): Id[A]       = a
    def unsafeUncapture[A](fa: Id[A]): A = fa
  }

  "A PeertTable with 1 byte addresses and k = 3" should "add new peer to the right bucket" in {
    val table       = PeerTable(local, 3)
    val peer        = PeerNode(NodeIdentifier(Seq(2.toByte)), endpoint)
    val pingedPeers = mutable.MutableList.empty[PeerNode]
    implicit val ping: Ping[Id] = (pn: PeerNode) => {
      pingedPeers += pn
      true
    }
    table.observe[Id](peer)

    table.distance(peer) shouldBe Some(6)
    pingedPeers shouldEqual Seq(peer)

    val entries = table.table(6).map(_.entry)
    entries shouldEqual Seq(peer)
  }

  it should "drop new peer if the bucket is full and the oldest peer is responding to ping" in {
    val table       = PeerTable(local, 3)
    val peer1       = PeerNode(NodeIdentifier(Seq(8.toByte)), endpoint)
    val peer2       = PeerNode(NodeIdentifier(Seq(9.toByte)), endpoint)
    val peer3       = PeerNode(NodeIdentifier(Seq(10.toByte)), endpoint)
    val peer4       = PeerNode(NodeIdentifier(Seq(12.toByte)), endpoint)
    val pingedPeers = mutable.MutableList.empty[PeerNode]
    implicit val ping: Ping[Id] = (pn: PeerNode) => {
      pingedPeers += pn
      true
    }
    table.observe[Id](peer1)
    table.observe[Id](peer2)
    table.observe[Id](peer3)
    table.observe[Id](peer4)

    table.distance(peer1) shouldBe Some(4)
    table.distance(peer2) shouldBe Some(4)
    table.distance(peer3) shouldBe Some(4)
    table.distance(peer4) shouldBe Some(4)
    pingedPeers shouldEqual Seq(peer1, peer2, peer3, peer4, peer1)

    val entries = table.table(4).map(_.entry)
    entries shouldEqual Seq(peer2, peer3, peer1)
  }

  it should "drop oldest peer and add new peer if the bucket is full and the oldest peer is not responding to ping" in {
    val table         = PeerTable(local, 3)
    val peer1         = PeerNode(NodeIdentifier(Seq(8.toByte)), endpoint)
    val peer2         = PeerNode(NodeIdentifier(Seq(9.toByte)), endpoint)
    val peer3         = PeerNode(NodeIdentifier(Seq(10.toByte)), endpoint)
    val peer4         = PeerNode(NodeIdentifier(Seq(12.toByte)), endpoint)
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

    table.distance(peer1) shouldBe Some(4)
    table.distance(peer2) shouldBe Some(4)
    table.distance(peer3) shouldBe Some(4)
    table.distance(peer4) shouldBe Some(4)
    pingedPeers shouldEqual Seq(peer1, peer2, peer3, peer4, peer1)

    val entries = table.table(4).map(_.entry)
    entries shouldEqual Seq(peer2, peer3, peer4)
  }
}
