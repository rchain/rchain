package coop.rchain.trie

import org.scalatest.{FlatSpec, Matchers}

class TrieSpec extends FlatSpec with Matchers {
  
  behavior of "Empty Trie"
  
  val trie = Trie.empty
  
  it should "have size 0" in {
    assert(trie.size === 0)
  }
  
  it should "have value None" in {
    assert(trie.value == None)
  }
  
  it should "return None when getting keys" in {
    assert(trie.get("foo") == None)
  }
  
  it should "return None when getting empty keys" in {
    assert(trie.get("") == None)
  }
  
  //it should "put and get keys and values" in {
  //  assert(trie.put("phone", "42").value == Some("42"))
  //}
  
  it should "get keys and values" in {
    assert(trie.get("phone") == Some("42"))
  }
}
