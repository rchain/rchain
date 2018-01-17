package coop.rchain.storage.regex

import org.scalatest._

class RegexPatternUnitTests extends FlatSpec with Matchers {

  "CharClassPattern" should "pass equality checks" in {
    assert(CharClassPattern("a") == CharClassPattern("a"))
    assert(~(~CharClassPattern("a")) == CharClassPattern("a"))
    assert(CharClassPattern("a") == ~(~CharClassPattern("a")))
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
    //assert(CharClassPattern("a") * 1 == CharClassPattern("a"))
    assert((CharClassPattern("a") * Multiplier(Some(1), Some(2))) != CharClassPattern("a"))
  }

  "CharClassPattern" should "produce good Fsm" in {
    val notA = (~CharClassPattern("a")).toFsm()
    assert(notA.alphabet == Set('a', Fsm.anythingElse))
    assert(notA.accepts("b"))
    assert(notA.accepts(Fsm.anythingElse.toString))
  }

  "Empty" should "work for all char classes" in {
    assert(CharClassPattern("").isEmpty)
    assert(ConcPattern().isEmpty)
    assert(AltPattern(Nil).isEmpty)
    assert(MultPattern(CharClassPattern(""), Multiplier.presetOne).isEmpty)
    assert(MultPattern(CharClassPattern("a"), Multiplier.presetZero).isEmpty)
  }

  "MultPattern" should "produce good Fsm" in {
    val a = CharClassPattern("a")
    val a1 = a * 1
    assert(a1.accepts("a"))
    assert(!a1.accepts("b"))
    assert(!a1.accepts("aa"))

    val a2 = a * 2
    assert(!a2.accepts("a"))
    assert(!a2.accepts("b"))
    assert(a2.accepts("aa"))
    assert(!a2.accepts("aaa"))

    val aQuest = a * Multiplier.presetQuestion
    assert(aQuest.accepts(""))
    assert(aQuest.accepts("a"))
    assert(!aQuest.accepts("b"))
    assert(!aQuest.accepts("aa"))

    val aStar = a * Multiplier.presetStar
    assert(aStar.accepts(""))
    assert(aStar.accepts("a"))
    assert(!aStar.accepts("b"))
    assert(aStar.accepts("aa"))
    assert(aStar.accepts("aaaaaaaaaaaaaaaaaaa"))

    val aPlus = a * Multiplier.presetPlus
    assert(!aPlus.accepts(""))
    assert(aPlus.accepts("a"))
    assert(!aPlus.accepts("b"))
    assert(aStar.accepts("aa"))
    assert(aStar.accepts("aaaaaaaaaaaaaaaaaaa"))

    val aZero = a * Multiplier.presetZero
    assert(aZero.accepts(""))
    assert(!aZero.accepts("a"))
    assert(!aZero.accepts("b"))
  }

  "AltPattern" should "pass equality check" in {}

  "reverse" should "work for all pattern types" in {
    assert(CharClassPattern("a").reversed == CharClassPattern("a"))

    val aOrB = AltPattern(CharClassPattern("a"), CharClassPattern("b"))
    val bOrA = AltPattern(CharClassPattern("b"), CharClassPattern("a"))

    assert(aOrB.reversed == aOrB)
    assert(aOrB.reversed == bOrA)
    assert(aOrB.reversed == bOrA.reversed)

    val ab = ConcPattern(CharClassPattern("a"), CharClassPattern("b"))
    val ba = ConcPattern(CharClassPattern("b"), CharClassPattern("a"))

    assert(ab.reversed == ba)
    assert(ba.reversed == ab)

    val aa = CharClassPattern("a") * 2
    assert(aa.reversed == aa)
  }

  "AltPattern" should "produce good Fsm" in {
    val fsm = AltPattern(CharClassPattern("a"), CharClassPattern("b")).toFsm()
    assert(fsm.accepts("a"))
    assert(fsm.accepts("b"))
    assert(!fsm.accepts("c"))
    assert(!fsm.accepts("aa"))
    assert(!fsm.accepts("ab"))
    assert(!fsm.accepts("ba"))
    assert(!fsm.accepts("bb"))
    assert(!fsm.accepts("a" + Fsm.anythingElse))
    assert(!fsm.accepts("b" + Fsm.anythingElse))
    assert(!fsm.accepts(Fsm.anythingElse + "b"))
    assert(!fsm.accepts(Fsm.anythingElse + "a"))
    assert(!fsm.accepts("" + Fsm.anythingElse + Fsm.anythingElse))
  }

  "ConcPattern" should "produce good Fsm" in {
    val fsm =
      ConcPattern(CharClassPattern("a"), CharClassPattern("a", negateCharSet = true)).toFsm()
    assert(fsm.states.size == 3)
    assert(!fsm.accepts("a"))
    assert(!fsm.accepts("b"))
    assert(!fsm.accepts("aa"))
    assert(fsm.accepts("ab"))
    assert(fsm.accepts("a" + Fsm.anythingElse))
    assert(!fsm.accepts("ba"))
    assert(!fsm.accepts("bb"))
  }

  "MultPattern" should "pass equality check" in {
    assert(
      MultPattern(CharClassPattern("a"), Multiplier.presetOne)
        == MultPattern(CharClassPattern("a"), Multiplier.presetOne))
    assert(
      MultPattern(CharClassPattern("a"), Multiplier.presetOne)
        != MultPattern(CharClassPattern("b"), Multiplier.presetOne))
    assert(
      MultPattern(CharClassPattern("a"), Multiplier.presetOne)
        != MultPattern(CharClassPattern("a"), Multiplier.presetQuestion))
    assert(
      MultPattern(CharClassPattern("a"), Multiplier.presetOne)
        != MultPattern(CharClassPattern("a"), Multiplier(Some(1), Some(2))))
    assert(MultPattern(CharClassPattern("a"), Multiplier.presetOne) != CharClassPattern("a"))
  }

  "ConcPattern" should "pass equality check" in {
    assert(
      ConcPattern(MultPattern(CharClassPattern("a"), Multiplier.presetOne))
        == ConcPattern(MultPattern(CharClassPattern("a"), Multiplier.presetOne)))
    assert(
      ConcPattern(MultPattern(CharClassPattern("a"), Multiplier.presetOne))
        != ConcPattern(MultPattern(CharClassPattern("b"), Multiplier.presetOne)))
    assert(
      ConcPattern(MultPattern(CharClassPattern("a"), Multiplier.presetOne))
        != ConcPattern(MultPattern(CharClassPattern("a"), Multiplier.presetQuestion)))
    assert(
      ConcPattern(MultPattern(CharClassPattern("a"), Multiplier.presetOne))
        != ConcPattern(MultPattern(CharClassPattern("a"), Multiplier(Some(1), Some(2)))))
    assert(
      ConcPattern(MultPattern(CharClassPattern("a"), Multiplier.presetOne))
        != ConcPattern.presetEmptyString)
  }

  "Nested Patterns" should "pass equality checks" in {
    assert(
      AltPattern(ConcPattern(MultPattern(CharClassPattern("a"), Multiplier.presetOne)) ::
        ConcPattern(MultPattern(CharClassPattern("a"), Multiplier.presetOne)) :: Nil)
        == AltPattern(ConcPattern(MultPattern(CharClassPattern("a"), Multiplier.presetOne))))

    assert(
      AltPattern(ConcPattern(MultPattern(CharClassPattern("a"), Multiplier.presetOne)) ::
        ConcPattern(MultPattern(CharClassPattern("b"), Multiplier.presetOne)) :: Nil)
        == AltPattern(ConcPattern(MultPattern(CharClassPattern("b"), Multiplier.presetOne)) ::
          ConcPattern(MultPattern(CharClassPattern("a"), Multiplier.presetOne)) :: Nil))
  }
}
