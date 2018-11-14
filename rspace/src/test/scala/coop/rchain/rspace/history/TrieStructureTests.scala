package coop.rchain.rspace.history

import java.nio.ByteBuffer

import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.test.{printTree, TestKey4}
import org.lmdbjava.Txn
import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs._

class TrieStructureTests
    extends HistoryTestsBase[Txn[ByteBuffer], TestKey4, ByteVector]
    with LMDBWithTestTrieStore[TestKey4] {

  implicit val codecV: Codec[ByteVector] = variableSizeBytesLong(int64, bytes)
  implicit val codecK: Codec[TestKey4]   = TestKey4.codecTestKey

  def withTrie[R](f: Trie[TestKey4, ByteVector] => R): R =
    withTestTrieStore { (store, branch) =>
      store.withTxn(store.createTxnRead()) { txn =>
        val trieOpt = store.get(txn, store.getRoot(txn, branch).get)
        trieOpt should not be empty
        f(trieOpt.get)
      }
    }

  def withTrieTxnAndStore[R](
      f: (
          ITrieStore[Txn[ByteBuffer], TestKey4, ByteVector],
          Branch,
          Txn[ByteBuffer],
          Trie[TestKey4, ByteVector]
      ) => R
  ): R =
    withTestTrieStore { (store, branch) =>
      store.withTxn(store.createTxnRead()) { txn =>
        val trieOpt = store.get(txn, store.getRoot(txn, branch).get)
        trieOpt should not be empty
        f(store, branch, txn, trieOpt.get)
      }
    }

  private[this] val SingleElementData = new {
    val key1    = TestData.key1
    val val1    = TestData.val1
    val rootHex = "a63410f3d715a183efbd3763189f9150deefeb538d51dc451d54d3a08100b114"
    val skipHex = "4869cd2d0a62a8eb80e54764d940b404ce098bb0bcda55877ea42c3ffbe49e4e"
    val leafHex = "8d329ed700f130f40b15b73b1bd4f7b70d982acb9dce55e58f58425038f5db1c"
  }

  private[this] val CommonPrefixData = new {
    val key1 = TestData.key1
    val val1 = TestData.val1
    val key2 = TestData.key2
    val val2 = TestData.val2

    val rootHex   = "244d4b0f670b4a3b30a2e494b42c054d9e9add68c0d6af4145ebf769182bb542"
    val level1Hex = "4946e174de1a6cac3deeee59465d152412a62f4486f9aa67afcd51788d8e4023"
    val level3Hex = "7b60933db17d93a3b1039131a33137b16e5e60818f5c1f264639e20c2d4874af"
    val leaf1Hex  = "8d329ed700f130f40b15b73b1bd4f7b70d982acb9dce55e58f58425038f5db1c"
    val leaf2Hex  = "f22c71982cf8663fb1ea77a444233c99d8c00cd187b0253cfc4213228fea6625"
  }

  "insert's effect" should "be visible in the outer read transaction" ignore {
    withTrieTxnAndStore { (store, branch, txn, trie) =>
      import SingleElementData._
      insert(store, branch, key1, val1)
      // Insert was made in a nested transaction, so it's effect should be visible
      store.get(txn, store.getRoot(txn, branch).get) should not be None
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
    withTestTrieStore { (store, branch) =>
      import SingleElementData._
      insert(store, branch, key1, val1)

      assertSingleElementTrie(store)
    }
  }

  it should "have four levels after inserting second element with same hash prefix" in {
    withTestTrieStore { (store, branch) =>
      import CommonPrefixData._
      insert(store, branch, key1, val1)
      insert(store, branch, key2, val2)

      assertCommonPrefixTrie(store)
    }
  }

  it should "retain previous structure after delete" in {
    withTestTrieStore { (store, branch) =>
      import CommonPrefixData._

      insert(store, branch, key1, val1)
      insert(store, branch, key2, val2)
      delete(store, branch, key2, val2)
      delete(store, branch, key1, val1)

      assertSingleElementTrie(store)
      assertCommonPrefixTrie(store)
    }
  }

  it should "retain previous structure after rollback" in {
    withTestTrieStore { (store, branch) =>
      import CommonPrefixData._

      insert(store, branch, key1, val1)
      insert(store, branch, key2, val2)

      store.withTxn(store.createTxnWrite()) { txn =>
        store.putRoot(
          txn,
          branch,
          Blake2b256Hash
            .fromHex(SingleElementData.rootHex)
        )
      }

      assertSingleElementTrie(store)
      assertCommonPrefixTrie(store)
    }
  }

  implicit def lift2TestKey4(s: String): TestKey4 =
    TestKey4.create(s.map(c => Integer.parseInt(c.toString)))

  it should "build one level of skip nodes for 4 element key" in withTestTrieStore {
    (store, branch) =>
      val k1: TestKey4 = "1000"
      insert(store, branch, k1, TestData.val1)

      store.withTxn(store.createTxnRead()) { txn =>
        val root = store.get(txn, store.getRoot(txn, branch).get).get.asInstanceOf[Node]
        root.pointerBlock.children should have size 1
        root.pointerBlock.childrenWithIndex(0)._2 shouldBe 1

        val skip = store.get(txn, root.pointerBlock.children(0).hash).get.asInstanceOf[Skip]
        skip.pointer shouldBe a[LeafPointer]
        skip.affix shouldBe ByteVector(0, 0, 0)

        val leaf1 =
          store.get(txn, skip.pointer.hash).get.asInstanceOf[Leaf[TestKey4, ByteVector]]
        leaf1.key shouldBe k1
        leaf1.value shouldBe TestData.val1
      }
  }

  it should "build two levels of skip nodes for 4 element key" in withTestTrieStore {
    (store, branch) =>
      val k1: TestKey4 = "1000"
      val k2: TestKey4 = "1001"
      insert(store, branch, k1, TestData.val1)
      insert(store, branch, k2, TestData.val1)

      store.withTxn(store.createTxnRead()) { txn =>
        val root = store.get(txn, store.getRoot(txn, branch).get).get.asInstanceOf[Node]
        root.pointerBlock.children should have size 1
        root.pointerBlock.childrenWithIndex(0)._2 shouldBe 1

        val skip = store.get(txn, root.pointerBlock.children(0).hash).get.asInstanceOf[Skip]
        skip.pointer shouldBe a[NodePointer]
        skip.affix shouldBe ByteVector(0, 0)

        val node = store.get(txn, skip.pointer.hash).get.asInstanceOf[Node]

        node.pointerBlock.children should have size 2
        node.pointerBlock.childrenWithIndex(0)._2 shouldBe 0
        node.pointerBlock.childrenWithIndex(1)._2 shouldBe 1

        node.pointerBlock
          .childrenWithIndex(0)
          ._1 shouldBe a[LeafPointer] // do we need skip pointers?
        node.pointerBlock.childrenWithIndex(1)._1 shouldBe a[LeafPointer]

        val leaf1 =
          store
            .get(txn, node.pointerBlock.children(0).hash)
            .get
            .asInstanceOf[Leaf[TestKey4, ByteVector]]
        leaf1.key shouldBe k1
        leaf1.value shouldBe TestData.val1

        val leaf2 =
          store
            .get(txn, node.pointerBlock.children(1).hash)
            .get
            .asInstanceOf[Leaf[TestKey4, ByteVector]]
        leaf2.key shouldBe k2
        leaf2.value shouldBe TestData.val1
      }
  }

  it should "not add unnecessary skip on 1st level after root" in withTestTrieStore {
    (store, branch) =>
      val k1 = TestKey4.create(Vector(1, 0, 1, 0))
      val k2 = TestKey4.create(Vector(1, 1, 0, 0))
      insert(store, branch, k1, TestData.val1)
      insert(store, branch, k2, TestData.val2)

      store.withTxn(store.createTxnRead()) { txn =>
        val root = store.get(txn, store.getRoot(txn, branch).get).get.asInstanceOf[Node]
        root.pointerBlock.children should have size 1
        root.pointerBlock.childrenWithIndex(0)._2 shouldBe 1

        val pb1 = store.get(txn, root.pointerBlock.children(0).hash).get.asInstanceOf[Node]
        pb1.pointerBlock.children should have size 2
        pb1.pointerBlock.childrenWithIndex(0)._2 shouldBe 0
        pb1.pointerBlock.childrenWithIndex(1)._2 shouldBe 1

        val skip = store.get(txn, pb1.pointerBlock.children(0).hash).get.asInstanceOf[Skip]
        skip.pointer shouldBe a[LeafPointer]
        skip.affix shouldBe ByteVector(1, 0)

        val leaf = store.get(txn, skip.pointer.hash).get.asInstanceOf[Leaf[TestKey4, ByteVector]]
        leaf.key shouldBe k1
        leaf.value shouldBe TestData.val1

        val skip2 = store.get(txn, pb1.pointerBlock.children(1).hash).get.asInstanceOf[Skip]
        skip2.pointer shouldBe a[LeafPointer]
        skip2.affix shouldBe ByteVector(0, 0)

        val leaf2 = store.get(txn, skip2.pointer.hash).get.asInstanceOf[Leaf[TestKey4, ByteVector]]
        leaf2.key shouldBe k2
        leaf2.value shouldBe TestData.val2
      }
  }

  private[this] def assertSingleElementTrie(
      implicit store: ITrieStore[Txn[ByteBuffer], TestKey4, ByteVector]
  ) = {
    import SingleElementData._
    store.withTxn(store.createTxnRead()) { implicit txn =>
      expectNode(rootHex, Seq((1, NodePointer(skipHex))))
      val expectedSkipHash = Blake2b256Hash.fromHex(skipHex)
      store.get(txn, expectedSkipHash) shouldBe Some(
        Skip(ByteVector(0, 0, 0), LeafPointer(leafHex))
      )

      val expectedLeafHash = Blake2b256Hash.fromHex(leafHex)
      store.get(txn, expectedLeafHash) shouldBe Some(Leaf(key1, val1))
    }

  }

  private[this] def assertCommonPrefixTrie(
      implicit store: ITrieStore[Txn[ByteBuffer], TestKey4, ByteVector]
  ) = {
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
    Blake2b256Hash.fromHex(hex)

  private[this] def expectNode(
      currentHex: String,
      childHexes: Seq[(Int, Pointer)]
  )(implicit txn: Txn[ByteBuffer], store: ITrieStore[Txn[ByteBuffer], TestKey4, ByteVector]) =
    store.get(
      txn,
      Blake2b256Hash
        .fromHex(currentHex)
    ) match {
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
      store: ITrieStore[Txn[ByteBuffer], TestKey4, ByteVector]
  ) =
    store.get(
      txn,
      Blake2b256Hash
        .fromHex(currentHex)
    ) match {
      case Some(Skip(affix, pointer)) =>
        affix shouldBe expectedAffix
        pointer shouldBe expectedPointer
      //byteVector shouldEqual ByteVector((Seq(0)).map(_.toByte))

      case default => fail(s"Expected a skip node under $currentHex, got $default")
    }
}
