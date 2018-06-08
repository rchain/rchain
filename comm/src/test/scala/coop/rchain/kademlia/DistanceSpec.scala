package coop.rchain.kademlia

import org.scalatest._
import scala.util.{Success, Try}
import scala.concurrent.duration.{Duration, MILLISECONDS}

import cats._

import coop.rchain.catscontrib.Capture
import coop.rchain.comm._
import coop.rchain.p2p.effects.Ping

object b {
  val rand                   = new scala.util.Random(System.currentTimeMillis)
  def apply(i: Int): Byte    = i.toByte
  def apply(s: String): Byte = b(Integer.parseInt(s, 2))
  def rand(nbytes: Int): Seq[Byte] = {
    val arr = Array.fill(nbytes)(b(0))
    rand.nextBytes(arr)
    arr
  }
}

class DistanceSpec extends FlatSpec with Matchers {

  val endpoint                = Endpoint("", 0, 0)
  implicit val ping: Ping[Id] = (_: PeerNode) => true
  implicit val caputre: Capture[Id] = new Capture[Id] {
    def capture[A](a: => A): Id[A]       = a
    def unsafeUncapture[A](fa: Id[A]): A = fa
  }

  "A PeerNode of width n bytes" should "have distance to itself equal to 8n" in {
    for (i <- 1 to 64) {
      val home = PeerNode(NodeIdentifier(b.rand(i)), endpoint)
      val nt   = PeerTable(home)
      nt.distance(home) should be(Some(8 * nt.width))
    }
  }

  for (exp <- 1 to 8) {

    val width = 1 << exp

    // Make 8*width copies all of which differ in a single, distinct bit
    def oneOffs(key: Seq[Byte]): Seq[Array[Byte]] =
      for {
        i <- 0 until width
        j <- 7 to 0 by -1
      } yield {
        val k1 = Array.fill(key.size)(b(0))
        Array.copy(k1, 0, key.toArray, 0, key.size)
        k1(i) = b(k1(i) ^ b(1 << j))
        k1
      }

    def testKey(key: Array[Byte]): Boolean = {
      val table = PeerTable(PeerNode(NodeIdentifier(key), endpoint))
      oneOffs(key).map(table.distance(_)) == (0 until 8 * width).map(Option[Int])
    }

    def keyString(key: Seq[Byte]): String =
      key.map("%02x".format(_)).mkString

    val k0 = Array.fill(width)(b(0))
    s"A node with key all zeroes (${keyString(k0)})" should "compute distance correctly" in {
      testKey(k0) should be(true)
    }

    val k1 = Array.fill(width)(b(0xff))
    s"A node with key all ones (${keyString(k1)})" should "compute distance correctly" in {
      testKey(k1) should be(true)
    }

    val kr = b.rand(width)
    s"A node with random key (${keyString(kr)})" should "compute distance correctly" in {
      testKey(kr.toArray) should be(true)
    }

    s"An empty table of width $width" should "have no peers" in {
      val table = PeerTable(PeerNode(NodeIdentifier(kr), endpoint))
      assert(table.table.forall(_.isEmpty))
    }

    it should "return no peers" in {
      val table = PeerTable(PeerNode(NodeIdentifier(kr), endpoint))
      table.peers.size should be(0)
    }

    it should "return no values on lookup" in {
      val table = PeerTable(PeerNode(NodeIdentifier(kr), endpoint))
      table.lookup(b.rand(width)).size should be(0)
    }

    s"A table of width $width" should "add a key at most once" in {
      val table = PeerTable(PeerNode(NodeIdentifier(kr), endpoint))
      val toAdd = oneOffs(kr).head
      val dist  = table.distance(toAdd).get
      for (i <- 1 to 10) {
        table.observe[Id](PeerNode(NodeIdentifier(toAdd), endpoint), true)
        table.table(dist).size should be(1)
      }
    }

    s"A table of width $width with peers at all distances" should "have no empty buckets" in {
      val table = PeerTable(PeerNode(NodeIdentifier(kr), endpoint))
      for (k <- oneOffs(kr.toArray)) {
        table.observe[Id](PeerNode(NodeIdentifier(k), endpoint), true)
      }
      assert(table.table.forall(_.nonEmpty))
    }

    it should s"return min(k, ${8 * width}) peers on lookup" in {
      val table = PeerTable(PeerNode(NodeIdentifier(kr), endpoint))
      for (k <- oneOffs(kr.toArray)) {
        table.observe[Id](PeerNode(NodeIdentifier(k), endpoint), true)
      }
      table.lookup(b.rand(width)).size should be(scala.math.min(table.k, 8 * width))
    }

    it should "not return sought peer on lookup" in {
      val table = PeerTable(PeerNode(NodeIdentifier(kr), endpoint))
      for (k <- oneOffs(kr.toArray)) {
        table.observe[Id](PeerNode(NodeIdentifier(k), endpoint), true)
      }
      val target = table.table(table.width * 4)(0)
      val resp   = table.lookup(target.key)
      assert(resp.forall(_.key != target.key))
    }

    it should s"return ${8 * width} peers when sequenced" in {
      val table = PeerTable(PeerNode(NodeIdentifier(kr), endpoint))
      for (k <- oneOffs(kr.toArray)) {
        table.observe[Id](PeerNode(NodeIdentifier(k), endpoint), true)
      }
      table.peers.size should be(8 * width)
    }

    it should "find each added peer" in {
      val table = PeerTable(PeerNode(NodeIdentifier(kr), endpoint))
      for (k <- oneOffs(kr.toArray)) {
        table.observe[Id](PeerNode(NodeIdentifier(k), endpoint), true)
      }
      for (k <- oneOffs(kr.toArray)) {
        table.find(k) should be(Some(PeerNode(NodeIdentifier(k), endpoint)))
      }
    }
  }
}
