package coop.rchain.comm.discovery

import scala.util.Random

import cats.{Id, catsInstancesForId => _}

import coop.rchain.catscontrib.effect.implicits._
import coop.rchain.comm._

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PeerTableSpec extends AnyFlatSpec with Matchers with Inside {
  val addressWidth = 8
  val endpoint     = Endpoint("", 0, 0)
  val home         = PeerNode(NodeIdentifier(randBytes(addressWidth).toIndexedSeq), endpoint)

  private def randBytes(nbytes: Int): Array[Byte] = {
    val arr = Array.fill(nbytes)(0.toByte)
    Random.nextBytes(arr)
    arr
  }

  implicit val ping: KademliaRPC[Id] = new KademliaRPC[Id] {
    def ping(node: PeerNode): Boolean                         = true
    def lookup(key: Seq[Byte], peer: PeerNode): Seq[PeerNode] = Seq.empty[PeerNode]
  }

  "Peer that is already in the table" should "get updated" in {
    val id    = randBytes(addressWidth)
    val peer0 = PeerNode(NodeIdentifier(id.toIndexedSeq), Endpoint("new", 0, 0))
    val peer1 = PeerNode(NodeIdentifier(id.toIndexedSeq), Endpoint("changed", 0, 0))
    val table = PeerTable[PeerNode, Id](home.key)
    table.updateLastSeen(peer0)
    inside(table.peers) {
      case p +: Nil => p should equal(peer0)
    }
    table.updateLastSeen(peer1)
    inside(table.peers) {
      case p +: Nil => p should equal(peer1)
    }
  }
}
