package coop.rchain.rspace.history

import java.nio.ByteBuffer

import coop.rchain.rspace.test.TestKey5
import org.lmdbjava.Txn
import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs.{bytes, int64, variableSizeBytesLong}

class TrieStructureTestsKeyLength5
    extends HistoryTestsBase[Txn[ByteBuffer], TestKey5, ByteVector]
    with LMDBWithTestTrieStore[TestKey5] {

  implicit val codecV: Codec[ByteVector] = variableSizeBytesLong(int64, bytes)
  implicit val codecK: Codec[TestKey5]   = TestKey5.codecTestKey

  implicit def lift2TestKey5(s: String): TestKey5 =
    TestKey5.create(s.map(c => Integer.parseInt(c.toString)))

  behavior of "A trie"

  it should "insert skip node " in withTestTrieStore { (store, branch) =>
    val k1: TestKey5 = "10000"
    insert(store, branch, k1, TestData.val1)

    store.withTxn(store.createTxnRead()) { txn =>
      val root = store.get(txn, store.getRoot(txn, branch).get).get.asInstanceOf[Node]
      root.pointerBlock.children should have size 1
      root.pointerBlock.childrenWithIndex(0)._2 shouldBe 1

      val skip = store.get(txn, root.pointerBlock.children(0).hash).get.asInstanceOf[Skip]
      skip.pointer shouldBe a[LeafPointer]
      skip.affix shouldBe ByteVector(0, 0, 0, 0)

      val leaf = store.get(txn, skip.pointer.hash).get.asInstanceOf[Leaf[TestKey5, ByteVector]]
      leaf.key shouldBe k1
      leaf.value shouldBe TestData.val1
    }
  }

  it should "build two levels of skip nodes" in withTestTrieStore { (store, branch) =>
    val k1: TestKey5 = "01000"
    val k2: TestKey5 = "01100"
    insert(store, branch, k1, TestData.val1)
    insert(store, branch, k2, TestData.val1)

    store.withTxn(store.createTxnRead()) { txn =>
      val root = store.get(txn, store.getRoot(txn, branch).get).get.asInstanceOf[Node]
      root.pointerBlock.children should have size 1
      root.pointerBlock.childrenWithIndex(0)._2 shouldBe 0

      val skip = store.get(txn, root.pointerBlock.children(0).hash).get.asInstanceOf[Skip]
      skip.pointer shouldBe a[NodePointer]
      skip.affix shouldBe ByteVector(1)

      val node = store.get(txn, skip.pointer.hash).get.asInstanceOf[Node]

      node.pointerBlock.children should have size 2
      node.pointerBlock.childrenWithIndex(0)._2 shouldBe 0
      node.pointerBlock.childrenWithIndex(1)._2 shouldBe 1

      node.pointerBlock.childrenWithIndex(0)._1 shouldBe a[NodePointer]
      node.pointerBlock.childrenWithIndex(1)._1 shouldBe a[NodePointer]

      val skipLeaf1 = store.get(txn, node.pointerBlock.children(0).hash).get.asInstanceOf[Skip]
      skipLeaf1.pointer shouldBe a[LeafPointer]
      skipLeaf1.affix shouldBe ByteVector(0, 0)

      val leaf1 =
        store.get(txn, skipLeaf1.pointer.hash).get.asInstanceOf[Leaf[TestKey5, ByteVector]]
      leaf1.key shouldBe k1
      leaf1.value shouldBe TestData.val1

      val skipLeaf2 = store.get(txn, node.pointerBlock.children(1).hash).get.asInstanceOf[Skip]
      skipLeaf2.pointer shouldBe a[LeafPointer]
      skipLeaf2.affix shouldBe ByteVector(0, 0)

      val leaf2 =
        store.get(txn, skipLeaf2.pointer.hash).get.asInstanceOf[Leaf[TestKey5, ByteVector]]
      leaf2.key shouldBe k2
      leaf2.value shouldBe TestData.val1
    }
  }

  it should "collapse structure after delete" in withTestTrieStore { (store, branch) =>
    val k1: TestKey5 = "01000"
    val k2: TestKey5 = "01100"
    insert(store, branch, k1, TestData.val1)
    insert(store, branch, k2, TestData.val1)
    delete(store, branch, k2, TestData.val1)

    store.withTxn(store.createTxnRead()) { txn =>
      val root = store.get(txn, store.getRoot(txn, branch).get).get.asInstanceOf[Node]
      root.pointerBlock.children should have size 1
      root.pointerBlock.childrenWithIndex(0)._2 shouldBe 0

      val skip = store.get(txn, root.pointerBlock.children(0).hash).get.asInstanceOf[Skip]
      skip.pointer shouldBe a[LeafPointer]
      skip.affix shouldBe ByteVector(1, 0, 0, 0)

      val leaf = store.get(txn, skip.pointer.hash).get.asInstanceOf[Leaf[TestKey5, ByteVector]]
      leaf.key shouldBe k1
      leaf.value shouldBe TestData.val1
    }
  }
}
