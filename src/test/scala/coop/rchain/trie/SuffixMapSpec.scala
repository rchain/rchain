package coop.rchain.trie

import org.scalatest.{FlatSpec, Matchers}

class SuffixMapSpec extends FlatSpec with Matchers {
  
  behavior of "SuffixMap"

  val sm = SuffixMap(Vector("a"), Vector("a-key"))
  
  it should "append pairs of strings" in {
    sm :+ ("b", "b-key") should equal(SuffixMap(Vector("a", "b"), Vector("a-key", "b-key")))
  }
  
  it should "replace pairs of strings" in {
    sm :+ ("a", "new-a-key") should equal(SuffixMap(Vector("a"), Vector("new-a-key")))
  }
  
  it should "get keys when present" in {
    assert(sm.keyAt("a") == Some("a-key"))
  }
  
  it should "return None when key not present" in {
    assert(sm.keyAt("c") == None)
  }
  
  it should "remove a suffix pair" in {
    sm.without("a") should equal(SuffixMap(Vector(), Vector()))
  }
  
  behavior of "prefix matching"
  
  val sm1 = SuffixMap(Vector("and", "raid"), Vector("and-key", "raid-key"))
  
  it should "find partial keys" in {
    assert(sm1.startsWith("an") == Some("and"))
  }
  
  it should "find whole keys" in {
    assert(sm1.startsWith("and") == Some("and"))
  }
  
  it should "not find unmatched keys" in {
    assert(sm1.startsWith("rnd") == None)
  }
  
  it should "match on shared prefixes (partial 1)" in {
    val sm = SuffixMap(Vector("and"), Vector("and-key"))
    val found = sm.findPrefix("ant")
    
    assert(found == Some(PrefixMatch(MatchResult("ant", "an"), MatchResult("and", "an"), "and-key"))) 
    assert(found.get.exact == false)
    assert(found.get.partial1 == true)
    assert(found.get.partial2 == false)
  }
  
  it should "match on overhanging prefixes (partial 2)" in {
    val sm = SuffixMap(Vector("an"), Vector("an-key"))
    val found = sm.findPrefix("andover")
    
    assert(found == Some(PrefixMatch(MatchResult("andover", "an"), MatchResult("an", "an"), "an-key"))) 
    assert(found.get.exact == false)
    assert(found.get.partial2 == true)
    assert(found.get.partial1 == false)
  }
  
  it should "match on sub prefixes" in {
    val sm = SuffixMap(Vector("ralism"), Vector("ralism-key"))
    val found = sm.findPrefix("r")
    
    assert(found == Some(PrefixMatch(MatchResult("r", "r"), MatchResult("ralism", "r"), "ralism-key"))) 
    assert(found.get.exact == false)
    assert(found.get.partial1 == false)
    assert(found.get.partial2 == false)
    assert(found.get.partial3 == true)
  }
  
  it should "match on whole prefixes" in {
    val sm = SuffixMap(Vector("foo"), Vector("foo-key"))
    val found = sm.findPrefix("foo")
    
    assert(found == Some(PrefixMatch(MatchResult("foo", "foo"), MatchResult("foo", "foo"), "foo-key"))) 
    assert(found.get.exact == true)
  }
  
  it should "not match on non-overlapping prefixes" in {
    val sm = SuffixMap(Vector("and"), Vector("and-key"))
    assert(sm.findPrefix("foo") == None)
  }

}