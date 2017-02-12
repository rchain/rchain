package coop.rchain.trie

import org.scalatest._
import org.scalatest.{FlatSpec, Matchers}

class TrieSpec extends FlatSpec with Matchers with BeforeAndAfterAll {
  
  override def afterAll() { 
    Datastore.db.client.close() 
  }  
  
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
  
  it should "put keys - r*" in {
    assert(Trie.put(ns, "romane", "42").v == Some("42"))
    assert(Trie.put(ns, "romanus", "43").v == Some("43"))
    assert(Trie.put(ns, "romulus", "44").v == Some("44"))
    assert(Trie.put(ns, "rubens", "45").v == Some("45"))
    assert(Trie.put(ns, "ruber", "46").v == Some("46"))
    assert(Trie.put(ns, "rubicon", "47").v == Some("47"))
    assert(Trie.put(ns, "rubicundus", "48").v == Some("48"))
  }
  
  it should "get keys - r*" in {
    assert(Trie.get(ns, "romane") == Some("42"))
    assert(Trie.get(ns, "romanus") == Some("43"))
    assert(Trie.get(ns, "romulus") == Some("44"))
    assert(Trie.get(ns, "rubens") == Some("45"))
    assert(Trie.get(ns, "ruber") == Some("46"))
    assert(Trie.get(ns, "rubicon") == Some("47"))
    assert(Trie.get(ns, "rubicundus") == Some("48"))
  }

  it should "put keys - a*" in {
    assert(Trie.put(ns, "and", "i").v == Some("i"))
    assert(Trie.put(ns, "a", "a").v == Some("a"))
    assert(Trie.put(ns, "an", "an").v == Some("an"))
    assert(Trie.put(ns, "ant", "ii").v == Some("ii"))
    assert(Trie.put(ns, "any", "iii").v == Some("iii"))
    assert(Trie.put(ns, "ar", "ar").v == Some("ar"))
    assert(Trie.put(ns, "are", "iv").v == Some("iv"))
    assert(Trie.put(ns, "art", "v").v == Some("v"))
  }
  
  it should "get keys - a*" in {
    assert(Trie.get(ns, "and") == Some("i"))
    assert(Trie.get(ns, "a") == Some("a"))
    assert(Trie.get(ns, "an") == Some("an"))
    assert(Trie.get(ns, "ant") == Some("ii"))
    assert(Trie.get(ns, "any") == Some("iii"))
    assert(Trie.get(ns, "are") == Some("iv"))
    assert(Trie.get(ns, "ar") == Some("ar"))
    assert(Trie.get(ns, "art") == Some("v"))
  }
  
  it should "put keys - t*" in {
    assert(Trie.put(ns, "test", "1").v == Some("1"))
    assert(Trie.put(ns, "toaster", "2").v == Some("2"))
    assert(Trie.put(ns, "toasting", "3").v == Some("3"))
    assert(Trie.put(ns, "slow", "4").v == Some("4"))
    assert(Trie.put(ns, "slowly", "5").v == Some("5"))
  }
  
  it should "get keys - t*" in {
    assert(Trie.get(ns, "test") == Some("1"))
    assert(Trie.get(ns, "toaster") == Some("2"))
    assert(Trie.get(ns, "toasting") == Some("3"))
    assert(Trie.get(ns, "slow") == Some("4"))
    assert(Trie.get(ns, "slowly") == Some("5"))
  }
}