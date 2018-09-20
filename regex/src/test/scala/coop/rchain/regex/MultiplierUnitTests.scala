package coop.rchain.regex

import org.scalatest._

class MultiplierUnitTests extends FlatSpec with Matchers {
  "Multiplier tryParse" should "work" in {
    assert(Multiplier.tryParse("{10}") == (Multiplier(Some(10), Some(10)), 4))
    assert(Multiplier.tryParse("{2,}") == (Multiplier(Some(2), Multiplier.Inf), 4))
    assert(Multiplier.tryParse("{2,}c") == (Multiplier(Some(2), Multiplier.Inf), 4))
    assert(Multiplier.tryParse("{4 , }def") == (Multiplier(Some(4), Multiplier.Inf), 6))
    assert(Multiplier.tryParse("{  4 , 5 }def") == (Multiplier(Some(4), Some(5)), 10))
  }

  "Multiplier tryParse" should "ignore invalid strings" in {
    assert(Multiplier.tryParse("{}") == (Multiplier.presetOne, 0))
    assert(Multiplier.tryParse("}") == (Multiplier.presetOne, 0))
    assert(Multiplier.tryParse("{1") == (Multiplier.presetOne, 0))
    assert(Multiplier.tryParse("{,") == (Multiplier.presetOne, 0))
    assert(Multiplier.tryParse("{1,") == (Multiplier.presetOne, 0))
  }

  "Mutltiplier substraction and parse" should "work" in {
    assert(Multiplier.parse("{13,21}").contains(Multiplier(13, 21)))
    //a{3,4}, a{2,5} -> a{2,3} (with a{1,1}, a{0,2} left over)
    assert(
      Multiplier.parse("{3,4}").get.common(Multiplier.parse("{2,5}").get)
        == Multiplier.parse("{2,3}").get
    )
    assert(
      (Multiplier.parse("{3,4}").get - Multiplier.parse("{2,3}").get)
        == Multiplier.presetOne
    )
    assert(
      (Multiplier.parse("{2,5}").get - Multiplier.parse("{2,3}").get)
        == Multiplier.parse("{0,2}").get
    )
    //a{2,}, a{1,5} -> a{1,5} (with a{1,}, a{0,0} left over)
    assert(
      Multiplier.parse("{2,}").get.common(Multiplier.parse("{1,5}").get)
        == Multiplier.parse("{1,5}").get
    )
    assert(
      (Multiplier.parse("{2,}").get - Multiplier.parse("{1,5}").get)
        == Multiplier.presetPlus
    )
    assert(
      (Multiplier.parse("{1,5}").get - Multiplier.parse("{1,5}").get)
        == Multiplier.presetZero
    )
    //a{3,}, a{2,} -> a{2,} (with a, epsilon left over)
    assert(
      Multiplier.parse("{3,}").get.common(Multiplier.parse("{2,}").get)
        == Multiplier.parse("{2,}").get
    )
    assert(
      (Multiplier.parse("{3,}").get - Multiplier.parse("{2,}").get)
        == Multiplier.presetOne
    )
    assert(
      (Multiplier.parse("{2,}").get - Multiplier.parse("{2,}").get)
        == Multiplier.presetZero
    )
    //a{3,}, a{3,} -> a{3,} (with zero, zero left over)
    assert(
      Multiplier.parse("{3,}").get.common(Multiplier.parse("{3,}").get)
        == Multiplier.parse("{3,}").get
    )
    assert(
      (Multiplier.parse("{3,}").get - Multiplier.parse("{3,}").get)
        == Multiplier.presetZero
    )
  }

  "Multiplier common operation" should "work as expected" in {
    assert(Multiplier.presetOne.common(Multiplier.presetStar) == Multiplier.presetZero)
    //* common + => *
    assert(Multiplier.parse("*").get.common(Multiplier.parse("+").get) == Multiplier.presetStar)

    assert(
      Multiplier.parse("{3,}").get.common(Multiplier.parse("{2,5}").get) == Multiplier
        .parse("{2,5}")
        .get
    )
  }

  "Multiplier union" should "work" in {
    assert((Multiplier.presetZero | Multiplier.presetZero) == Multiplier.presetZero)
    assert((Multiplier.presetZero | Multiplier.presetQuestion) == Multiplier.presetQuestion)
    assert((Multiplier.presetZero | Multiplier.presetOne) == Multiplier.presetQuestion)
    assert((Multiplier.presetZero | Multiplier.presetStar) == Multiplier.presetStar)
    assert((Multiplier.presetZero | Multiplier.presetPlus) == Multiplier.presetStar)

    assert((Multiplier.presetQuestion | Multiplier.presetZero) == Multiplier.presetQuestion)
    assert((Multiplier.presetQuestion | Multiplier.presetQuestion) == Multiplier.presetQuestion)
    assert((Multiplier.presetQuestion | Multiplier.presetOne) == Multiplier.presetQuestion)
    assert((Multiplier.presetQuestion | Multiplier.presetStar) == Multiplier.presetStar)
    assert((Multiplier.presetQuestion | Multiplier.presetPlus) == Multiplier.presetStar)

    assert((Multiplier.presetOne | Multiplier.presetZero) == Multiplier.presetQuestion)
    assert((Multiplier.presetOne | Multiplier.presetQuestion) == Multiplier.presetQuestion)
    assert((Multiplier.presetOne | Multiplier.presetOne) == Multiplier.presetOne)
    assert((Multiplier.presetOne | Multiplier.presetStar) == Multiplier.presetStar)
    assert((Multiplier.presetOne | Multiplier.presetPlus) == Multiplier.presetPlus)

    assert((Multiplier.presetStar | Multiplier.presetZero) == Multiplier.presetStar)
    assert((Multiplier.presetStar | Multiplier.presetQuestion) == Multiplier.presetStar)
    assert((Multiplier.presetStar | Multiplier.presetOne) == Multiplier.presetStar)
    assert((Multiplier.presetStar | Multiplier.presetStar) == Multiplier.presetStar)
    assert((Multiplier.presetStar | Multiplier.presetPlus) == Multiplier.presetStar)

    assert((Multiplier.presetPlus | Multiplier.presetZero) == Multiplier.presetStar)
    assert((Multiplier.presetPlus | Multiplier.presetQuestion) == Multiplier.presetStar)
    assert((Multiplier.presetPlus | Multiplier.presetOne) == Multiplier.presetPlus)
    assert((Multiplier.presetPlus | Multiplier.presetStar) == Multiplier.presetStar)
    assert((Multiplier.presetPlus | Multiplier.presetPlus) == Multiplier.presetPlus)

    assert(!Multiplier.presetZero.canUnion(Multiplier(Some(2), Multiplier.Inf)))
    assert(!Multiplier.presetOne.canUnion(Multiplier(3, 4)))
    assert(!Multiplier(Some(8), Multiplier.Inf).canUnion(Multiplier(3, 4)))

    assertThrows[IllegalArgumentException](Multiplier.presetZero | Multiplier(3, 4))
  }
}
