package coop.rchain.trie

import org.scalatest.{FlatSpec, Matchers}

class TrieSpec extends FlatSpec with Matchers {
  
  behavior of "Trie"
  
  def ns = "test://"
  
  it should "return None when getting non-existent keys" in {
    assert(Trie.get(ns, "missing") == None)
  }
  
  it should "return None when getting empty keys" in {
    assert(Trie.get(ns, "") == None)
  }
  
  it should "have a persistent root at a namespace" in {
    val root = Trie.root("test://")
    root.put("foo", "bar")
    assert(Trie.root("test://").id == root.id)
  }
  
  it should "put and get keys" in {
    Trie.put(ns, "romane", "42")
    Trie.put(ns, "romanus", "43")
    Trie.put(ns, "romulus", "44")
    Trie.put(ns, "rubens", "45")
    Trie.put(ns, "ruber", "46")
    Trie.put(ns, "rubicon", "47")
    Trie.put(ns, "rubicundus", "48")
    
    assert(Trie.get(ns, "romane") == Some("42"))
    assert(Trie.get(ns, "romanus") == Some("43"))
    assert(Trie.get(ns, "romulus") == Some("44"))
    assert(Trie.get(ns, "rubens") == Some("45"))
    assert(Trie.get(ns, "ruber") == Some("46"))
    assert(Trie.get(ns, "rubicon") == Some("47"))
    assert(Trie.get(ns, "rubicundus") == Some("48"))

    Trie.put(ns, "and", "i")
    Trie.put(ns, "a", "a")
    Trie.put(ns, "an", "an")
    Trie.put(ns, "ant", "ii")
    Trie.put(ns, "any", "iii")
    Trie.put(ns, "ar", "ar")
    Trie.put(ns, "are", "iv")
    Trie.put(ns, "art", "v")

    assert(Trie.get(ns, "and") == Some("i"))
    assert(Trie.get(ns, "a") == Some("a"))
    assert(Trie.get(ns, "an") == Some("an"))
    assert(Trie.get(ns, "ant") == Some("ii"))
    assert(Trie.get(ns, "any") == Some("iii"))
    assert(Trie.get(ns, "are") == Some("iv"))
    assert(Trie.get(ns, "ar") == Some("ar"))
    assert(Trie.get(ns, "art") == Some("v"))

    Trie.put(ns, "test", "1")
    Trie.put(ns, "toaster", "2")
    Trie.put(ns, "toasting", "3")
    Trie.put(ns, "slow", "4")
    Trie.put(ns, "slowly", "5")

    assert(Trie.get(ns, "test") == Some("1"))
    assert(Trie.get(ns, "toaster") == Some("2"))
    assert(Trie.get(ns, "toasting") == Some("3"))
    assert(Trie.get(ns, "slow") == Some("4"))
    assert(Trie.get(ns, "slowly") == Some("5"))
  }
}
