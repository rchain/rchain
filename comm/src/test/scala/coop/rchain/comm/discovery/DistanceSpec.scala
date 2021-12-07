package coop.rchain.comm.discovery

import java.util
import cats.{catsInstancesForId => _, _}
import coop.rchain.catscontrib.effect.implicits._
import coop.rchain.comm._
import coop.rchain.shared.Base16
import org.scalatest._

import scala.util.Random

class DistanceSpec extends FlatSpec with Matchers {
  private def randBytes(nbytes: Int): Array[Byte] = {
    val arr = Array.fill(nbytes)(0.toByte)
    Random.nextBytes(arr)
    arr
  }

  val endpoint = Endpoint("", 0, 0)
  implicit val ping: KademliaRPC[Id] = new KademliaRPC[Id] {
    def ping(node: PeerNode): Boolean                         = true
    def lookup(key: Seq[Byte], peer: PeerNode): Seq[PeerNode] = Seq.empty[PeerNode]
  }

  "A PeerNode of width n bytes" should "have distance to itself equal to 8n" in {
    for (i <- 1 to 64) {
      val home = PeerNode(NodeIdentifier(randBytes(i)), endpoint)
      val nt   = PeerTable[PeerNode, Id](home.key)
      nt.distance(home) should be(Some(8 * nt.width))
    }
  }

  for (exp <- 1 to 8) {

    val width = 1 << exp

    // Make 8*width copies all of which differ in a single, distinct bit
    def oneOffs(key: Array[Byte]): Seq[Array[Byte]] =
      for {
        i <- 0 until width
        j <- 7 to 0 by -1
      } yield {
        val k1 = Array.fill(key.length)(0.toByte)
        Array.copy(key, 0, k1, 0, k1.length)
        k1(i) = (k1(i) ^ (1 << j).toByte).toByte
        k1
      }

    def testKey(key: Array[Byte]): Boolean = {
      val table = PeerTable[PeerNode, Id](key)
      oneOffs(key).map(table.distance(_)) == (0 until 8 * width).map(Option[Int])
    }

    def keyString(key: Array[Byte]): String =
      Base16.encode(key)

    val k0 = Array.fill(width)(0.toByte)
    s"A node with key all zeroes (${keyString(k0)})" should "compute distance correctly" in {
      testKey(k0) should be(true)
    }

    val k1 = Array.fill(width)(0xff.toByte)
    s"A node with key all ones (${keyString(k1)})" should "compute distance correctly" in {
      testKey(k1) should be(true)
    }

    val kr = randBytes(width)
    s"A node with random key (${keyString(kr)})" should "compute distance correctly" in {
      testKey(kr) should be(true)
    }

    s"An empty table of width $width" should "have no peers" in {
      val table = PeerTable[PeerNode, Id](kr)
      assert(table.table.forall(_.isEmpty))
    }

    it should "return no peers" in {
      val table = PeerTable[PeerNode, Id](kr)
      table.peers.size should be(0)
    }

    it should "return no values on lookup" in {
      val table = PeerTable[PeerNode, Id](kr)
      table.lookup(randBytes(width)).size should be(0)
    }

    s"A table of width $width" should "add a key at most once" in {
      val table = PeerTable[PeerNode, Id](kr)
      val toAdd = oneOffs(kr).head
      val dist  = table.distance(toAdd).get
      for (i <- 1 to 10) {
        table.updateLastSeen(PeerNode(NodeIdentifier(toAdd), endpoint))
        table.table(dist).size should be(1)
      }
    }

    s"A table of width $width with peers at all distances" should "have no empty buckets" in {
      val table = PeerTable[PeerNode, Id](kr)
      for (k <- oneOffs(kr)) {
        table.updateLastSeen(PeerNode(NodeIdentifier(k), endpoint))
      }
      assert(table.table.forall(_.nonEmpty))
    }

    it should s"return min(k, ${8 * width}) peers on lookup" in {
      val table     = PeerTable[PeerNode, Id](kr)
      val krOneOffs = oneOffs(kr)
      for (k <- krOneOffs) {
        table.updateLastSeen(PeerNode(NodeIdentifier(k), endpoint))
      }
      val randomKey = randBytes(width)
      val expected =
        if (krOneOffs.exists(util.Arrays.equals(_, randomKey))) 8 * width - 1 else 8 * width
      table.lookup(randomKey).size should be(scala.math.min(table.k, expected))
    }

    it should "not return sought peer on lookup" in {
      val table = PeerTable[PeerNode, Id](kr)
      for (k <- oneOffs(kr)) {
        table.updateLastSeen(PeerNode(NodeIdentifier(k), endpoint))
      }
      val target = table.table(table.width * 4)(0)
      val resp   = table.lookup(target.key)
      assert(resp.forall(_.key != target.key))
    }

    it should s"return ${8 * width} peers when sequenced" in {
      val table = PeerTable[PeerNode, Id](kr)
      for (k <- oneOffs(kr)) {
        table.updateLastSeen(PeerNode(NodeIdentifier(k), endpoint))
      }
      table.peers.size should be(8 * width)
    }

    it should "find each added peer" in {
      val table = PeerTable[PeerNode, Id](kr)
      for (k <- oneOffs(kr)) {
        table.updateLastSeen(PeerNode(NodeIdentifier(k), endpoint))
      }
      for (k <- oneOffs(kr)) {
        table.find(k) should be(Some(PeerNode(NodeIdentifier(k), endpoint)))
      }
    }
  }
}
