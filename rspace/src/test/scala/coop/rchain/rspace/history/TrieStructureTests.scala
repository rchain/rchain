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
  behavior of "A trie"

  def withTrie[R](f: Trie[TestKey, ByteVector] => R): R =
    withTestTrieStore { store =>
      store.withTxn(store.createTxnRead()) { txn =>
        val trieOpt = store.get(txn, store.workingRootHash.get)
        trieOpt should not be empty
        f(trieOpt.get)
      }
    }

  it should "be created as an empty pointer block" in
    withTrie {
      case Node(PointerBlock(vector)) =>
        vector should have size 256
        vector should contain only None
      case _ => fail("expected a node")
    }

  it should "have two levels after inserting one element" in {

    //    insert(store, key1, val1)
  }
}
