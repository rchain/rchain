package coop.rchain.rspace.history

import java.nio.ByteBuffer

import coop.rchain.rspace.Blake2b256Hash
import org.lmdbjava.Txn
import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs._

import scala.collection.immutable

class TrieStructureTests
    extends HistoryTestsBase[Txn[ByteBuffer], TestKey4, ByteVector]
    with LMDBTrieStoreFixtures {

  implicit val codecByteVector: Codec[ByteVector] = variableSizeBytesLong(int64, bytes)

  def withTrie[R](f: Trie[TestKey4, ByteVector] => R): R =
    withTestTrieStore { store =>
      store.withTxn(store.createTxnRead()) { txn =>
        val trieOpt = store.get(txn, store.getRoot(txn).get)
        trieOpt should not be empty
        f(trieOpt.get)
      }
    }

  def withTrieTxnAndStore[R](
      f: (ITrieStore[Txn[ByteBuffer], TestKey4, ByteVector],
          Txn[ByteBuffer],
          Trie[TestKey4, ByteVector]) => R): R =
    withTestTrieStore { store =>
      store.withTxn(store.createTxnRead()) { txn =>
        val trieOpt = store.get(txn, store.getRoot(txn).get)
        trieOpt should not be empty
        f(store, txn, trieOpt.get)
      }
    }

  private[this] val SingleElementData = new {
    val key1    = TestData.key1
    val val1    = TestData.val1
    val rootHex = "f8758db35082dc03c90db2e1686e2a72394a7618f74e4e8cea2da516896b8a68"
    val leafHex = "8d329ed700f130f40b15b73b1bd4f7b70d982acb9dce55e58f58425038f5db1c"
  }

  private[this] val CommonPrefixData = new {
    val key1 = TestData.key1
    val val1 = TestData.val1
    val key2 = TestData.key2
    val val2 = TestData.val2

    val rootHex   = "67bbd01c5d66fc26b0c966c2bf0698ecb12cef1d718598ec6f0364431c32c675"
    val level1Hex = "536a45e654a07426112ac146e11b18e929f61687499e19925832c0d1dba60013"
    val level2Hex = "488827a0e0e6a09f46719888a23a19ca4ceb18df2bc3a5bca2462dd0d278bbb3"
    val level3Hex = "7b60933db17d93a3b1039131a33137b16e5e60818f5c1f264639e20c2d4874af"
    val leaf1Hex  = "8d329ed700f130f40b15b73b1bd4f7b70d982acb9dce55e58f58425038f5db1c"
    val leaf2Hex  = "f22c71982cf8663fb1ea77a444233c99d8c00cd187b0253cfc4213228fea6625"
  }

  "insert's effect" should "be visible in the outer read transaction" ignore {
    withTrieTxnAndStore { (store, txn, trie) =>
      import SingleElementData._
      insert(store, key1, val1)
      // Insert was made in a nested transaction, so it's effect should be visible
      store.get(txn, store.getRoot(txn).get) should not be None
    }
  }

  behavior of "A trie"
  it should "be created as an empty pointer block" in
    withTrie {
      case Node(PointerBlock(vector)) =>
        vector should have size 256
        vector should contain only EmptyPointer
      case _ => fail("expected a node")
    }

  it should "have two levels after inserting one element" in {
    withTestTrieStore { implicit store =>
      import SingleElementData._
      insert(store, key1, val1)

      assertSingleElementTrie
    }
  }

  it should "have four levels after inserting second element with same hash prefix" in {
    withTestTrieStore { implicit store =>
      import CommonPrefixData._
      insert(store, key1, val1)
      insert(store, key2, val2)

      assertCommonPrefixTrie
    }
  }

  it should "retain previous structure after delete" in {
    withTestTrieStore { implicit store =>
      import CommonPrefixData._

      insert(store, key1, val1)
      insert(store, key2, val2)
      delete(store, key2, val2)
      delete(store, key1, val1)

      assertSingleElementTrie
      assertCommonPrefixTrie
    }
  }

  it should "retain previous structure after rollback" in {
    withTestTrieStore { implicit store =>
      import CommonPrefixData._

      insert(store, key1, val1)
      insert(store, key2, val2)

      store.withTxn(store.createTxnWrite()) { txn =>
        store.putRoot(txn,
                      Blake2b256Hash
                        .fromHex(SingleElementData.rootHex))
      }

      assertSingleElementTrie
      assertCommonPrefixTrie
    }
  }

  implicit def lift2TestKey5(s: String): TestKey5 =
    TestKey5.create(s.map(c => Integer.parseInt(c.toString)))

  it should "insert skip node " in withTestTrieStoreKey5 { store =>
    val k1: TestKey5 = "10000"
    insert(store, k1, TestData.val1)

    store.withTxn(store.createTxnRead()) { txn =>
      val root = store.get(txn, store.getRoot(txn).get).get.asInstanceOf[Node]
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

  it should "build two levels of skip nodes" in withTestTrieStoreKey5 { store =>
    val k1: TestKey5 = "01000"
    val k2: TestKey5 = "01100"
    insert(store, k1, TestData.val1)
    insert(store, k2, TestData.val1)

    store.withTxn(store.createTxnRead()) { txn =>
      val root = store.get(txn, store.getRoot(txn).get).get.asInstanceOf[Node]
      root.pointerBlock.children should have size 1
      root.pointerBlock.childrenWithIndex(0)._2 shouldBe 0

      val skip = store.get(txn, root.pointerBlock.children(0).hash).get.asInstanceOf[Skip]
      skip.pointer shouldBe a[NodePointer]
      skip.affix shouldBe ByteVector(1)

      val node = store.get(txn, skip.pointer.hash).get.asInstanceOf[Node]

      node.pointerBlock.children should have size 2
      node.pointerBlock.childrenWithIndex(0)._2 shouldBe 0
      node.pointerBlock.childrenWithIndex(1)._2 shouldBe 1

      node.pointerBlock.childrenWithIndex(0)._1 shouldBe a[NodePointer] // do we need skip pointers?
      node.pointerBlock.childrenWithIndex(1)._1 shouldBe a[NodePointer]

      val skipLeaf1 = store.get(txn, node.pointerBlock.children(0).hash).get.asInstanceOf[Skip]
      skipLeaf1.pointer shouldBe a[LeafPointer]
      skipLeaf1.affix shouldBe ByteVector(0, 0)

      val skipLeaf2 = store.get(txn, node.pointerBlock.children(1).hash).get.asInstanceOf[Skip]
      skipLeaf2.pointer shouldBe a[LeafPointer]
      skipLeaf2.affix shouldBe ByteVector(0, 0)
    }
  }

  private[this] def assertSingleElementTrie(
      implicit store: ITrieStore[Txn[ByteBuffer], TestKey4, ByteVector]) = {
    import SingleElementData._
    store.withTxn(store.createTxnRead()) { implicit txn =>
      expectNode(rootHex, Seq((1, LeafPointer(leafHex))))
      val expectedLeafHash = Blake2b256Hash
        .fromHex(leafHex)

      store.get(txn, expectedLeafHash) shouldBe Some(Leaf(key1, val1))
    }

  }

  private[this] def assertCommonPrefixTrie(
      implicit store: ITrieStore[Txn[ByteBuffer], TestKey4, ByteVector]) = {
    import CommonPrefixData._
    store.withTxn(store.createTxnRead()) { implicit txn =>
      expectNode(rootHex, Seq((1, NodePointer(level1Hex))))
      expectSkip(level1Hex, ByteVector(Seq(0, 0).map(_.toByte)), NodePointer(level3Hex))
      expectNode(level3Hex, Seq((0, LeafPointer(leaf1Hex)), (1, LeafPointer(leaf2Hex))))

      val expectedLeaf1Hash = Blake2b256Hash
        .fromHex(leaf1Hex)
      val expectedLeaf2Hash = Blake2b256Hash
        .fromHex(leaf2Hex)

      store.get(txn, expectedLeaf1Hash) shouldBe Some(Leaf(key1, val1))
      store.get(txn, expectedLeaf2Hash) shouldBe Some(Leaf(key2, val2))
    }
  }

  private[this] implicit def liftHexToBlake(hex: String): Blake2b256Hash =
    Blake2b256Hash
      .fromHex(hex)

  private[this] def expectNode(currentHex: String, childHexes: Seq[(Int, Pointer)])(
      implicit txn: Txn[ByteBuffer],
      store: ITrieStore[Txn[ByteBuffer], TestKey4, ByteVector]) =
    store.get(txn,
              Blake2b256Hash
                .fromHex(currentHex)) match {
      case Some(Node(PointerBlock(vector))) =>
        vector should have size 256
        val expectedPointers = childHexes.map {
          case (expectedPosition, expectedPointer) =>
            vector(expectedPosition) shouldBe expectedPointer
            expectedPointer
        }

        vector.filterNot(expectedPointers.contains) should contain only EmptyPointer

      case default => fail(s"Expected a node under $currentHex, got $default")
    }

  def expectSkip(currentHex: String, expectedAffix: ByteVector, expectedPointer: NonEmptyPointer)(
      implicit txn: Txn[ByteBuffer],
      store: ITrieStore[Txn[ByteBuffer], TestKey4, ByteVector]) =
    store.get(txn,
              Blake2b256Hash
                .fromHex(currentHex)) match {
      case Some(Skip(affix, pointer)) =>
        affix shouldBe expectedAffix
        pointer shouldBe expectedPointer
      //byteVector shouldEqual ByteVector((Seq(0)).map(_.toByte))

      case default => fail(s"Expected a skip node under $currentHex, got $default")
    }
}
