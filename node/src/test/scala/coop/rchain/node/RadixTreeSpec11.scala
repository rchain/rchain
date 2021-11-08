package coop.rchain.node

import cats.Id
import cats.implicits.{catsSyntaxApplicativeId, toTraverseOps}
import coop.rchain.catscontrib.effect.implicits.concurrentId
import coop.rchain.rspace.history.RadixStore
import coop.rchain.rspace.history.RadixTree5._
import coop.rchain.store.InMemoryKeyValueStore
import org.scalatest.FlatSpec
import scodec.bits.{BinStringSyntax, BitVector, ByteVector}

import scala.concurrent.duration.Duration
import scala.language.higherKinds

class RadixTreeSpec11 extends FlatSpec {

  val store = new RadixStore(InMemoryKeyValueStore[Id])

  def sToBv(str: String): ByteVector = ByteVector.fromHex(str).get

  implicit class TreeBinOps[F[_]](rootNode: Vector[Child]) {
    def binUpdate(radixKey: String, radixValue: ByteVector)(
        impl: RadixTree5Impl[F]
    ): F[Node] =
      impl.update(rootNode, sToBv(radixKey), radixValue)

    def binDelete(radixKey: String)(impl: RadixTree5Impl[F]): F[Node] =
      impl.delete(rootNode, sToBv(radixKey))
  }

  it should "test update and delete" in {
    val impl1 = new RadixTree5Impl(store)

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

    val impl2 = new RadixTree5Impl(store)

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
    val impl = new RadixTree5Impl(store)
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
    val impl = new RadixTree5Impl(store)
    val t0   = emptyNode
    val t1   = impl.update(t0, sToBv("011111"), ByteVector(0x55, 0x55, 0x55))
    val t2   = impl.update(t1, sToBv("012222"), ByteVector(0x66, 0x66, 0x66))

    val t0Ser = codecs.codecNode.encode(t0).require.toByteVector
    val t1Ser = codecs.codecNode.encode(t1).require.toByteVector
    val t2Ser = codecs.codecNode.encode(t2).require.toByteVector

    println(s"t0: $t0Ser")
    println(s"t1: $t1Ser")
    println(s"t2: $t2Ser")
  }

  it should "bitVector to bytebuffer" in {
    import coop.rchain.shared.ByteVectorOps.RichByteVector
    val bits: BitVector   = bin"0000001"
    val bytes: ByteVector = bits.bytes
    val bits2: BitVector  = bytes.bits
    val buffer            = bytes.toDirectByteBuffer
    val buffer2           = bits.toByteBuffer
    println(s"bits: $bits")
    println(s"bytes: $bytes")
    println(s"bits2: $bits2")
    println(s"buffer: $buffer")
    println(s"buffer2: $buffer2")
  }

  it should "collection test" in {
    val l1: List[Byte] = List(0x00, 0x10, 0x20)
    val l2: List[Byte] = List(0x00, 0x10, 0x20)
    assert(l1 == l2, "!!!")
    val v1: Vector[Byte] = Vector(0x00, 0x10, 0x20)
    val v2: Vector[Byte] = Vector(0x00, 0x10, 0x20)
    assert(v1 == v2, "!!!")

    assert(l1 == v2, "!!!")

    val ar1: Array[Byte] = Array(0x00, 0x10, 0x20)
    val ar2: Array[Byte] = Array(0x00, 0x10, 0x20)
    assert((ar1.deep, v1) == (ar2.deep, v2), "!!!")

  }

  it should "new codec" in {
    import coop.rchain.rspace.history.RadixTree13._

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
              val nodes = (0 until num).map { _ =>
                emptyNode
                  .updated(
                    (i + 5) % 256,
                    Leaf(ByteVector(bytes(i % 32)), ByteVector(bytes(255)), varLength = true)
                  )
                  .updated(
                    (i + 10) % 256,
                    Leaf(ByteVector(bytes(i % 32)), ByteVector(bytes(255)), varLength = true)
                  )
                  .updated(
                    (i + 15) % 256,
                    Leaf(ByteVector(bytes(i % 32)), ByteVector(bytes(255)), varLength = true)
                  )
                  .updated(
                    (i + 20) % 256,
                    Leaf(ByteVector(bytes(i % 32)), ByteVector(bytes(128)), varLength = true)
                  )
                  .updated(
                    (i + 3) % 256,
                    NodePtr(ByteVector(bytes((i + 3) % 32)), ByteVector(bytes(32).toArray))
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

//              val _                     = nodesEncode
//              val timeDecodeTemp: Long  = 0
//              val timeCompareTemp: Long = 0

//              val timeCompareTemp = {
//                val t0  = System.nanoTime
//                val qwe = (nodesEncode).map(_.head)
//                val t1  = System.nanoTime
//                val _   = qwe
//                val m   = Duration.fromNanos(t1 - t0).toMillis
//                m
//              }

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

    val tasks: List[Int] = List(10000)
//    val tasks: List[Int] = List(1)
    tasks.traverse(x => experiment(x).pure)
  }
}
