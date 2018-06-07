package coop.rchain.rspace.history

import java.nio.ByteBuffer

import coop.rchain.rspace.Blake2b256Hash
import org.lmdbjava.Txn
import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs._

class TrieStructureTests
    extends HistoryTestsBase[Txn[ByteBuffer], TestKey, ByteVector]
    with LMDBTrieStoreFixtures {

  implicit val codecByteVector: Codec[ByteVector] = variableSizeBytesLong(int64, bytes)

  def withTrie[R](f: Trie[TestKey, ByteVector] => R): R =
    withTestTrieStore { store =>
      store.withTxn(store.createTxnRead()) { txn =>
        val trieOpt = store.get(txn, store.getRoot(txn).get)
        trieOpt should not be empty
        f(trieOpt.get)
      }
    }

  def withTrieTxnAndStore[R](
      f: (ITrieStore[Txn[ByteBuffer], TestKey, ByteVector],
          Txn[ByteBuffer],
          Trie[TestKey, ByteVector]) => R): R =
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

    val rootHex   = "2f9a7d304661daa8a2f20575e48967b541c2009df3c39b2daa49eacf59324bf0"
    val level1Hex = "904f54ce5a6876c0d995f9ec4009a73e33cab29f241e507a806d02ae9ed387b5"
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

  private[this] def assertSingleElementTrie(
      implicit store: ITrieStore[Txn[ByteBuffer], TestKey, ByteVector]) = {
    import SingleElementData._
    store.withTxn(store.createTxnRead()) { implicit txn =>
      expectNode(rootHex, Seq((1, LeafPointer(leafHex))))
      val expectedLeafHash = Blake2b256Hash
        .fromHex(leafHex)

      store.get(txn, expectedLeafHash) shouldBe Some(Leaf(key1, val1))
    }

  }

  private[this] def assertCommonPrefixTrie(
      implicit store: ITrieStore[Txn[ByteBuffer], TestKey, ByteVector]) = {
    import CommonPrefixData._
    store.withTxn(store.createTxnRead()) { implicit txn =>
      expectNode(rootHex, Seq((1, NodePointer(level1Hex))))
      expectNode(level1Hex, Seq((0, NodePointer(level2Hex))))
      expectNode(level2Hex, Seq((0, NodePointer(level3Hex))))
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
      store: ITrieStore[Txn[ByteBuffer], TestKey, ByteVector]) =
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

}
