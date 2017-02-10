package coop.rchain.trie

import org.scalatest.{FlatSpec, Matchers}

class SuffixMapSpec extends FlatSpec with Matchers {
  
  behavior of "SuffixMap with Vectors"
  
  val smv = SuffixMap(Vector("a"), Vector("a-key"))
  
  it should "append string tuples" in {
    smv + ("b" -> "b-key") should equal(SuffixMap(Vector("a", "b"), Vector("a-key", "b-key")))
  }
  
  it should "replace string tuples" in {
    smv + ("a" -> "new-a-key") should equal(SuffixMap(Vector("a"), Vector("new-a-key")))
  }
  
  it should "remove keys" in {
    smv - "a" should equal(SuffixMap(Vector(), Vector()))
  }
  
  behavior of "SuffixMap with Tuples"

  val sm = SuffixMap("a" -> "a-key")
  
  it should "append pairs of strings" in {
    sm + ("b" -> "b-key") should equal(SuffixMap("a" -> "a-key", "b" -> "b-key"))
  }
  
  it should "replace pairs of strings" in {
    sm + ("a" -> "new-a-key") should equal(SuffixMap("a" -> "new-a-key"))
  }
  
  it should "remove keys" in {
    assert((SuffixMap("a" -> "b", "c" -> "d") - "a") === (SuffixMap("c" -> "d")))
  }
  
  it should "chain add and remove ops" in {
    assert(smv - "a" + ("foo" -> "bar") == (SuffixMap("foo" -> "bar")))
  }
  
  it should "get keys when present" in {
    assert(SuffixMap("foo" -> "bar").get("foo") == Some("bar"))
  }
  
  it should "get terminator keys when present" in {
    assert(SuffixMap(Trie.Terminator -> "bar").get(Trie.Terminator) == Some("bar"))
  }
  
  it should "return None when key not present" in {
    assert(SuffixMap("foo" -> "bar").get("c") == None)
  }
  
  behavior of "keyWithPrefix"
  
  val sm1 = SuffixMap("and" -> "and-key", "raid" -> "raid-key")
  
  it should "find partial keys" in {
    assert(sm1.keyWithPrefix("an") == Some("and"))
  }
  
  it should "find whole keys" in {
    assert(sm1.keyWithPrefix("and") == Some("and"))
  }
  
  it should "not find non-overlapping keys" in {
    assert(sm1.keyWithPrefix("rnd") == None)
  }
  
  behavior of "checkPrefix"
  
  it should "match on shared prefixes (Partial)" in {
    val sm = SuffixMap("and" -> "and-key")
    val found = sm.checkPrefix("ant") 
    assert(found == Partial("and-key", ("an", "d"), "t")) 
    assert(found.suffix == "and") 
  }
  
  it should "match on overlapping prefixes (PartialRight)" in {
    val sm = SuffixMap("an" -> "an-key")   
    val found = sm.checkPrefix("andover")
    assert(found == PartialRight("an-key", ("an", Trie.Terminator), "dover")) 
    assert(found.suffix == "an") 
  }
  
  it should "match on sub-prefixes (PartialLeft)" in {
    val sm = SuffixMap("ralism" ->"ralism-key")    
    val found = sm.checkPrefix("r")
    assert(found == PartialLeft("ralism-key", ("r", "alism"), Trie.Terminator)) 
    assert(found.suffix == "ralism") 
  }
  
  it should "match on whole prefixes" in {
    val sm = SuffixMap("foo" -> "foo-key")
    (sm.checkPrefix("foo") == Hit("foo-key")) 
  }
  
  it should "not match on non-overlapping prefixes" in {
    val sm = SuffixMap("and" -> "and-key")
    assert(sm.checkPrefix("foo") == Miss("foo"))
  }
}