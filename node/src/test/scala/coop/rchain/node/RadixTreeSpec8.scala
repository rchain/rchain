package coop.rchain.node

import cats.Id
import coop.rchain.catscontrib.effect.implicits.concurrentId
import coop.rchain.rspace.history.RadixStore
import coop.rchain.rspace.history.RadixTree5._
import coop.rchain.store.InMemoryKeyValueStore
import org.scalatest.FlatSpec
import scodec.bits.{BinStringSyntax, BitVector, ByteVector}

import scala.concurrent.duration.Duration
import scala.language.higherKinds

class RadixTreeSpec8 extends FlatSpec {

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

  it should "new codec" in {
    import coop.rchain.rspace.history.RadixTree8._
    val num = 10 * 1000

    println(s"Number of experiments is : $num")
    println("")

    def bytes(size: Int) = (0 until size).map(
      i => i.toByte
    )

    val nodes = (0 until num).map { i =>
      emptyNode
        .updated(i % 256, Leaf(ByteVector(bytes(i % 32)), ByteVector(bytes(32)), is32ptr = false))
        .updated((i + 3) % 256, NodePtr(ByteVector(bytes((i + 3) % 32)), ByteVector(bytes(32))))
    }

    val numExp = 1

    for (_ <- 0 until numExp) {
      println("")

      val manualSer = {
        val _      = nodes.map(codecs.encode)
        val t0     = System.nanoTime
        val result = nodes.map(codecs.encode)
        val t1     = System.nanoTime
        val m      = Duration.fromNanos(t1 - t0).toMillis
        println(s"manualSer (manual codec) time is : $m ms")
        result
      }

      val manualSer2 = {
        val _      = nodes.map(codecs.encode2)
        val t0     = System.nanoTime
        val result = nodes.map(codecs.encode2)
        val t1     = System.nanoTime
        val m      = Duration.fromNanos(t1 - t0).toMillis
        println(s"manualSer2 (manual codec) time is : $m ms")
        result
      }

      println("")

      val manual = {
        val _      = manualSer.map(v => codecs.decode(v))
        val t0     = System.nanoTime
        val result = manualSer.map(v => codecs.decode(v))
        val t1     = System.nanoTime
        val m      = Duration.fromNanos(t1 - t0).toMillis
        println(s"manual (new codec decode with ByteVector) time is : $m ms")
        result
      }

      val manual2 = {
        val _      = manualSer2.map(v => codecs.decode2(v))
        val t0     = System.nanoTime
        val result = manualSer2.map(v => codecs.decode2(v))
        val t1     = System.nanoTime
        val m      = Duration.fromNanos(t1 - t0).toMillis
        println(s"manual2 (new codec decode with ByteVector) time is : $m ms")
        result
      }

      assert(
        (manual == nodes) &&
          (manual2 == nodes),
        "Encoded and decoded node should be same"
      )
    }
//    val serial3_bits_string_bad = serial3_bits.update(0, high = true).toBin
//    println(s"serial3_bits_string_bad: $serial3_bits_string_bad.")

//    val decodeNodeBad = codecs3.codecNode.decode(serial3_bits.update(0, high = true)).require.value
//
//    println("")
//    println("decodeNodeBad = " + decodeNodeBad)
  }

}
