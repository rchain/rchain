package coop.rchain.node

import cats.Id
import cats.implicits.{catsSyntaxApplicativeId, toTraverseOps}
import coop.rchain.catscontrib.effect.implicits.concurrentId
import coop.rchain.rspace.history.RadixStore
import coop.rchain.store.InMemoryKeyValueStore
import org.scalatest.FlatSpec
import scodec.bits.ByteVector

import scala.concurrent.duration.Duration
import scala.language.higherKinds

class RadixTreeSpec extends FlatSpec {

  val store = new RadixStore(InMemoryKeyValueStore[Id])

  /*
  def sToBv(str: String): ByteVector = ByteVector.fromHex(str).get

  implicit class TreeBinOps[F[_]](rootNode: Vector[Item]) {
    def binUpdate(radixKey: String, radixValue: ByteVector)(
        impl: RadixTreeImpl[F]
    ): F[Node] =
      impl.update(rootNode, sToBv(radixKey), radixValue)

    def binDelete(radixKey: String)(impl: RadixTreeImpl[F]): F[Node] =
      impl.delete(rootNode, sToBv(radixKey))
  }

  it should "test update and delete" in {
    val impl1 = new RadixTreeImpl(store)

    val initialTree1 = emptyNode
      .binUpdate("11111101", ByteVector(0xaa, 0xaa))(impl1)
      .binUpdate("11111102", ByteVector(0xbb))(impl1)
      .binUpdate("33333301", ByteVector(0xcc, 0xcc))(impl1)
      .binUpdate("44444401", ByteVector(0xee, 0xee))(impl1)

    val updatedTree = initialTree1
      .binUpdate("33333302", ByteVector(0xdd, 0xdd))(impl1)
      .binUpdate("11110001", ByteVector(0xff, 0xff, 0xff))(impl1)

    impl1.commit

    println("")
    println(s"initial1:")
    impl1.print(initialTree1)
    println("")
    println(s"updatedTree1. Pasting (33333302, 0xdddd), (11110001, 0xffffff):")
    impl1.print(updatedTree)

    val impl2 = new RadixTreeImpl(store)

    val initialTree2 = emptyNode
      .binUpdate("33333302", ByteVector(0xdd, 0xdd))(impl2)
      .binUpdate("11110001", ByteVector(0xff, 0xff, 0xff))(impl2)
      .binUpdate("11111101", ByteVector(0xaa, 0xaa))(impl2)
      .binUpdate("11111102", ByteVector(0xbb))(impl2)
      .binUpdate("33333301", ByteVector(0xcc, 0xcc))(impl2)
      .binUpdate("44444401", ByteVector(0xee, 0xee))(impl2)

    val cutTree = initialTree2
      .binDelete("33333302")(impl2)
      .binDelete("11110001")(impl2)

    impl2.commit

    println("")
    println(s"initialTree2:")
    impl2.print(initialTree2)
    println("")
    println(s"cutTreePtr. Deleting 33333302, 11110001 :")
    impl2.print(cutTree)

    assert(initialTree2 == updatedTree, "Test update not passed")
    println(s"Test update passed")

    assert(cutTree == initialTree1, "Test delete not passed")
    println(s"Test delete passed")
  }

  it should "test read" in {
    val impl = new RadixTreeImpl(store)
    val initialTree = emptyNode
      .binUpdate("11111101", ByteVector(0xaa, 0xaa))(impl)
      .binUpdate("11111102", ByteVector(0xbb))(impl)
      .binUpdate("33333301", ByteVector(0xcc, 0xcc))(impl)
      .binUpdate("33333302", ByteVector(0xdd, 0xdd))(impl)
      .binUpdate("44444401", ByteVector(0xee, 0xee))(impl)
      .binUpdate("11110001", ByteVector(0xff, 0xff, 0xff))(impl)

    val r1 = impl.read(initialTree, sToBv("11111101"))
    val r2 = impl.read(initialTree, sToBv("11111102"))
    val r3 = impl.read(initialTree, sToBv("33333301"))
    val r4 = impl.read(initialTree, sToBv("33333302"))
    val r5 = impl.read(initialTree, sToBv("44444401"))
    val r6 = impl.read(initialTree, sToBv("11110001"))
    val r_ = impl.read(initialTree, sToBv("000000"))

    println(s"r1: $r1")
    println(s"r2: $r2")
    println(s"r3: $r3")
    println(s"r4: $r4")
    println(s"r5: $r5")
    println(s"r6: $r6")
    println(s"r_: $r_")
  }

  it should "calculate common prefix" in {
    val v1  = ByteVector(1, 2, 3, 4, 5)
    val v2  = ByteVector(1, 2, 4, 5)
    val res = commonPrefix(v1, v2)

    println(s"PREFIX: $res")
  }

  it should "test serialization" in {
    val impl = new RadixTreeImpl(store)
    val t0   = emptyNode
    val t1   = impl.update(t0, sToBv("011111"), ByteVector(0x55, 0x55, 0x55))
    val t2   = impl.update(t1, sToBv("012222"), ByteVector(0x66, 0x66, 0x66))

    val t0Ser = codecs.encode(t0)
    val t1Ser = codecs.encode(t1)
    val t2Ser = codecs.encode(t2)

    println(s"t0: $t0Ser")
    println(s"t1: $t1Ser")
    println(s"t2: $t2Ser")
  }
   */

  it should "new codec" in {
    import coop.rchain.rspace.history.RadixTree._

    def experiment(num: Int): Unit = {

      def bytes(size: Int) = (0 until size).map(
        i => i.toByte
      )

      val averageNum       = 50
      val averageWarmUp    = 1
      val averageStatistic = true

      val result =
        (0 until (averageNum + averageWarmUp)).toList
          .foldLeft((0L, 0L, 0L)) {
            case ((timeEncode, timeDecode, timeCompare), i) =>
              val nodes = (0 until num).map { numNode =>
                emptyNode
                  .updated(
                    numNode % 256,
                    Leaf(ByteVector(bytes(numNode % 128)), ByteVector(bytes(32)))
                  )
                  .updated(
                    (numNode + 3) % 256,
                    NodePtr(ByteVector(bytes((numNode + 3) % 128)), ByteVector(bytes(32)))
                  )
              }

              def statistic(timeEncode: Long, timeDecode: Long, timeCompare: Long): Unit = {
                def iI(v: Int) = "%7d) ".format(v)

                def numStr(v: Int) = "%7d ".format(v)

                def mS(v: Long) = "%7.3f ".format(v.toDouble / 1000)

                val str = iI(i) + numStr(num) + mS(timeEncode) + mS(timeDecode) + mS(timeCompare)
                println(str)
              }

              val (nodesEncode, timeEncodeTemp) = {
                val t0  = System.nanoTime
                val res = nodes.map(codecs.encode)
                val t1  = System.nanoTime
                val m   = Duration.fromNanos(t1 - t0).toMillis
                (res, m)
              }

              val (nodesDecode, timeDecodeTemp) = {
                val t0  = System.nanoTime
                val res = nodesEncode.map(v => codecs.decode(v))
                val t1  = System.nanoTime
                val m   = Duration.fromNanos(t1 - t0).toMillis
                (res, m)
              }

              val (equalArr, timeCompareTemp) = {
                val t0  = System.nanoTime
                val res = nodesDecode == nodes
                val t1  = System.nanoTime
                val m   = Duration.fromNanos(t1 - t0).toMillis
                (res, m)
              }

              assert(equalArr, "Encoded and decoded node should be same")

              if (averageStatistic) statistic(timeEncodeTemp, timeDecodeTemp, timeCompareTemp)

              if (i < averageWarmUp) (timeEncode, timeDecode, timeCompare)
              else
                (
                  timeEncode + timeEncodeTemp,
                  timeDecode + timeDecodeTemp,
                  timeCompare + timeCompareTemp
                )
          }

      def numStr(v: Int) = "%7d ".format(v)

      def mS(v: Long) = "%7.3f ".format(v.toDouble / (1000 * averageNum))

      val str = numStr(num) + mS(result._1) + mS(result._2) + mS(result._3)
      println(str)
    }

    def fS(v: String): String = "%7s, ".format(v)
    val strTitle              = fS("num") + fS("timeEnc(sec)") + fS("timeDec(sec)") + fS("timeCompare(sec)")
    println(strTitle)

    val tasks: List[Int] = List(100000)
    tasks.traverse(x => experiment(x).pure)
  }
}
