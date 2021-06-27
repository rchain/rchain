package coop.rchain.node

import org.scalatest.FlatSpec
import scodec.bits.ByteVector

import scala.collection.concurrent.TrieMap

class RadixTreeSpec extends FlatSpec {
  import coop.rchain.rspace.history.RadixTree._

  implicit class TreeBinOps(treeImpl: (TreeR, RadixTreeImpl)) {
    def update(prefix: String, value: ByteVector): (TreeR, RadixTreeImpl) = {
      val (tree, impl) = treeImpl
      val newTree      = impl.update(tree, ByteVector.fromHex(prefix).get, value)
      (newTree, impl)
    }

    def delete(prefix: String): (TreeR, RadixTreeImpl) = {
      val (tree, impl) = treeImpl
      val newTree      = impl.delete(tree, ByteVector.fromHex(prefix).get)
      (newTree, impl)
    }

    def read(prefix: String) = {
      val (tree, impl) = treeImpl
      impl.read(tree, ByteVector.fromHex(prefix).get)
    }
  }

  it should "test root node" in {
    val store = TrieMap[ByteVector, ByteVector]()
    val impl  = new RadixTreeImpl(store)
    val t0    = emptyTree

    val (t1, _) = (t0, impl)
    // 1-byte prefix
      .update("010203", ByteVector(0x66))
//      .update("010506", ByteVector(0x77))

    // Delete
    val (t2, _) = (t1, impl)
//      .delete("010506")

    impl.print(t1)
    impl.print(t2)

    val r1 = (t2, impl).read("77010209")
    val r2 = (t2, impl).read("7701aa00")

    println(s"r1: $r1")
    println(s"r2: $r2")
  }

  it should "test delete" in {
    val store = TrieMap[ByteVector, ByteVector]()
    val impl  = new RadixTreeImpl(store)
    val t0    = emptyTree

    val (t1, _) = (t0, impl)
    // 1-byte prefix
      .update("010203", ByteVector(0x66))
      .update("010506", ByteVector(0x77))
      // 2-byte prefix
      .update("040507", ByteVector(0xbb))
      .update("040506", ByteVector(0x88))
      // 1-byte prefix to existing 2-byte
      .update("040607", ByteVector(0xcc))
      // Another sub node
      .update("7701aa00", ByteVector(0xff))
      .update("77010203", ByteVector(0x7a))
      .update("77010209", ByteVector(0x7b))

    // Delete
    val (t2, _) = (t1, impl)
      .delete("77010203")
      .delete("77010209")
      // ...
      .delete("040507")
      .delete("040506")
      // ...
      .delete("010506")
    // ...

    impl.print(t1)
    impl.print(t2)

    val r1 = (t2, impl).read("77010209")
    val r2 = (t2, impl).read("7701aa00")

    println(s"r1: $r1")
    println(s"r2: $r2")
  }

  it should "test update" in {
    val store = TrieMap[ByteVector, ByteVector]()
    val impl  = new RadixTreeImpl(store)
    val t0    = emptyTree

    val (t1, _) = (t0, impl)
    // 1-byte prefix
      .update("010203", ByteVector(0x66))
      .update("010506", ByteVector(0x77))
      // 2-byte prefix
      .update("040507", ByteVector(0xbb))
      .update("040506", ByteVector(0x88))
      // 1 - byte prefix to existing 2 - byte
      .update("040607", ByteVector(0xcc))
      // Another sub node
      .update("77010203", ByteVector(0x7a))
      .update("77010209", ByteVector(0x7b))

    impl.print(t1)
  }

  it should "calculate common prefix" in {

    val v1 = ByteVector(1, 2, 3, 4, 5)
    val v2 = ByteVector(1, 2, 4, 5)

    val res = commonPrefix(v1, v2)

    println(s"PREFIX: $res")
  }

  it should "test serialization" in {
    val store = TrieMap[ByteVector, ByteVector]()
    val impl  = new RadixTreeImpl(store)

    val t0 = emptyTree

    val t1 = impl.update(t0, ByteVector(1, 2, 3), ByteVector(0x66))

    val t0Ser = codecs.codecTreeR.encode(t0).require.toByteVector
    val t1Ser = codecs.codecTreeR.encode(t1).require.toByteVector

    println(s"t0: $t0Ser")
    println(s"t1: $t1Ser")
  }
}
