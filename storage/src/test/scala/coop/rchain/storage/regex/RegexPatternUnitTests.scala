package coop.rchain.storage.regex

import org.scalatest._

class RegexPatternUnitTests extends FlatSpec with Matchers {

  "CharClassPattern" should "pass equality checks" in {
    assert(CharClassPattern("a") == CharClassPattern("a"))
    assert(~CharClassPattern("a") == ~CharClassPattern("a"))
    assert(~CharClassPattern("a") != CharClassPattern("a"))
    assert(CharClassPattern("ab") == CharClassPattern("ba"))
  }

  "CharClassPattern" should "support union operation" in {
    assert((CharClassPattern("ab") | CharClassPattern("bc")) == CharClassPattern("abc"))
    assert((CharClassPattern("ab") | ~CharClassPattern("bc")) == ~CharClassPattern("c"))
    assert((~CharClassPattern("ab") | CharClassPattern("bc")) == ~CharClassPattern("a"))
    assert((~CharClassPattern("ab") | ~CharClassPattern("bc")) == ~CharClassPattern("b"))
  }

  "CharClassPattern" should "support intersection operation" in {
    assert((CharClassPattern("ab") & CharClassPattern("bc")) == CharClassPattern("b"))
    assert((CharClassPattern("ab") & ~CharClassPattern("bc")) == CharClassPattern("a"))
    assert((~CharClassPattern("ab") & CharClassPattern("bc")) == CharClassPattern("c"))
    assert((~CharClassPattern("ab") & ~CharClassPattern("bc")) == ~CharClassPattern("abc"))
  }

  "CharClassPattern" should "suport isEmpty" in {
    assert(CharClassPattern("").isEmpty)
    assert(!(~CharClassPattern("")).isEmpty)
  }

  "CharClassPattern" should "support multiplication" in {
    assert(CharClassPattern("a") * 1 == CharClassPattern("a"))
    //TODO: assert(CharClassPattern("a") * Multiplier(1, 3) == CharClassPattern("a"))
  }

  "CharClassPattern" should "produce good Fsm" in {
    val notA = (~CharClassPattern("a")).toFsm()
    assert(notA.alphabet == Set('a', Fsm.anythingElse))
    assert(notA.accepts("b"))
    assert(notA.accepts(Fsm.anythingElse.toString))
  }
}
