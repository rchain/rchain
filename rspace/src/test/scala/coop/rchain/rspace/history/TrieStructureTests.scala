package coop.rchain.rspace.history

import org.scalatest.prop.{Checkers, Configuration}
import org.scalatest.{FlatSpecLike, Matchers}
import scodec.bits.ByteVector

class TrieStructureTests
    extends LMDBHistoryActionsTests
    with FlatSpecLike
    with Matchers
    with Checkers
    with Configuration {
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
