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

  import TestData._

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
      insert(store, key1, val1)

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

  it should "have four levels after inserting second element with same hash prefix" in {
    withTestTrieStore { store =>
      insert(store, key1, val1)
      insert(store, key2, val2)

      store.withTxn(store.createTxnRead()) { txn =>
        val trie = store.get(txn, store.workingRootHash.get)
        trie match {
          case Some(Node(PointerBlock(vector))) =>
            vector should have size 256
            val expectedHash = Blake2b256Hash
              .fromHex("0xc67f3d92de6f9e75b9561274fbcc4c8efe8d1161554e3b0f66307a111ddd11ce")
              .get

            val maybeExpectedHash = Some(expectedHash)
            vector(1) shouldBe maybeExpectedHash
            vector.filterNot(_ == maybeExpectedHash) should contain only None

            store.get(txn, expectedHash) match {
              case Some(Node(PointerBlock(vector))) =>
                vector should have size 256
                val expectedHash = Blake2b256Hash
                  .fromHex("0xdb61c95e4ea234de4fe154861f86d090af2029da3cfca83cf210c71c539f1942")
                  .get

                val maybeExpectedHash = Some(expectedHash)
                vector(0) shouldBe maybeExpectedHash
                vector.filterNot(_ == maybeExpectedHash) should contain only None

                store.get(txn, expectedHash) match {

                  case Some(Node(PointerBlock(vector))) =>
                    vector should have size 256
                    val expectedHash = Blake2b256Hash
                      .fromHex("0x681aff745729ccec1d5e3f23f5b56796416a6604a8a07b646fc16f1b0e8c70c8")
                      .get

                    val maybeExpectedHash = Some(expectedHash)
                    vector(0) shouldBe maybeExpectedHash
                    vector.filterNot(_ == maybeExpectedHash) should contain only None
                    store.get(txn, expectedHash) match {

                      case Some(Node(PointerBlock(vector))) =>
                        vector should have size 256
                        val expectedHash1 = Blake2b256Hash
                          .fromHex(
                            "0x8d329ed700f130f40b15b73b1bd4f7b70d982acb9dce55e58f58425038f5db1c")
                          .get
                        val expectedHash2 = Blake2b256Hash
                          .fromHex(
                            "0xf22c71982cf8663fb1ea77a444233c99d8c00cd187b0253cfc4213228fea6625")
                          .get

                        val maybeExpectedHash1 = Some(expectedHash1)
                        val maybeExpectedHash2 = Some(expectedHash2)

                        vector(0) shouldBe maybeExpectedHash1
                        vector(1) shouldBe maybeExpectedHash2
                        vector
                          .filterNot(_ == maybeExpectedHash1)
                          .filterNot(_ == maybeExpectedHash2) should contain only None

                        store.get(txn, expectedHash1) shouldBe Some(Leaf(key1, val1))
                        store.get(txn, expectedHash2) shouldBe Some(Leaf(key2, val2))
                    }
                }

              case _ =>
            }

          case _ => fail("expected a node")
        }
      }

    }
  }
}
