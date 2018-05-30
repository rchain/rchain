package coop.rchain.rspace.history

import java.nio.ByteBuffer

import org.lmdbjava.Txn
import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs._

class TrieStructureTests
    extends HistoryTestsBase[Txn[ByteBuffer], TestKey, ByteVector]
    with WithLMDBStore {

  implicit val codecByteVector: Codec[ByteVector] = variableSizeBytesLong(int64, bytes)

  def withTrie[R](f: Trie[TestKey, ByteVector] => R): R =
    withTestTrieStore { store =>
      store.withTxn(store.createTxnRead()) { txn =>
        val trieOpt = store.get(txn, store.workingRootHash.get)
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
        val trieOpt = store.get(txn, store.workingRootHash.get)
        trieOpt should not be empty
        f(store, txn, trieOpt.get)
      }
    }

  "insert's effect" should "be visible in the outer read transaction" ignore {
    withTrieTxnAndStore { (store, txn, trie) =>
      val key1 = TestKey.create(Seq(1, 0, 0, 0))
      val val1 = ByteVector("value1".getBytes)
      insert(store, key1, val1)
      // Insert was made in a nested transaction, so it's effect should be visible
      store.get(txn, store.workingRootHash.get) should not be None
    }
  }

  behavior of "A trie"
  it should "be created as an empty pointer block" in
    withTrie {
      case Node(PointerBlock(vector)) =>
        vector should have size 256
        vector should contain only None
      case _ => fail("expected a node")
    }

  it should "have two levels after inserting one element" in {
    withTestTrieStore { store =>
      val key1 = TestKey.create(Seq(1, 0, 0, 0))
      val val1 = ByteVector("value1".getBytes)
      // Root before: 0xc575260cf13e36f179a50b0882bd64fc0466ecd25bdd7bc88766c2cc2e4c0dfe
      insert(store, key1, val1)
      // Root after: 0x538b82b41d4360492c17a112864ffb989571504b73b1281677a692e9fd2ee4cc
      store.withTxn(store.createTxnRead()) { txn =>
        val trie = store.get(txn, store.workingRootHash.get)
        trie match {
          case Some(Node(PointerBlock(vector))) =>
            vector should have size 256
            val expectedHash = Blake2b256Hash
              .fromHex("0x8d329ed700f130f40b15b73b1bd4f7b70d982acb9dce55e58f58425038f5db1c")
              .get
            val maybeExpectedHash = Some(expectedHash)
            vector(1) shouldBe maybeExpectedHash
            vector.filterNot(_ == maybeExpectedHash) should contain only None

            store.get(txn, expectedHash) shouldBe Some(Leaf(key1, val1))

          case _ => fail("expected a node")
        }
      }
    }
  }

}
