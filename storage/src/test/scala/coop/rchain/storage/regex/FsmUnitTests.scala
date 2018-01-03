package coop.rchain.storage.regex
import org.scalatest._

class FsmUnitTests extends FlatSpec with Matchers {
  val _ob = -5

  def createFsmA : Fsm = Fsm(
      Set('a', 'b'),
      Set(0, 1, _ob),
      0,
      Set(1),
      Map(
        0 -> Map('a' -> 1, 'b' -> _ob),
        1 -> Map('a' -> _ob, 'b' -> _ob),
        _ob -> Map('a' -> _ob, 'b' -> _ob)
      )
    )

  def createFsmB : Fsm = Fsm(
    Set('a', 'b'),
    Set(0, 1, _ob),
    0,
    Set(1),
    Map(
      0 -> Map('a' -> _ob, 'b' -> 1),
      1 -> Map('a' -> _ob, 'b' -> _ob),
      _ob -> Map('a' -> _ob, 'b' -> _ob)
    )
  )

  def createFsmAbc : Fsm = Fsm(
    Set('a', 'b', 'c'),
    Set(0, 1, 2, 3, _ob),
    0,
    Set(3),
    Map(
      0 -> Map('a' -> 1, 'b' -> _ob, 'c' -> _ob),
      1 -> Map('a' -> _ob, 'b' -> 2, 'c' -> _ob),
      2 -> Map('a' -> _ob, 'b' -> _ob, 'c' -> 3),
      3 -> Map('a' -> _ob, 'b'-> _ob, 'c' -> _ob)
    ))

  def createBrzozowski : Fsm = Fsm(
    Set('a', 'b'),
    Set(0, 1, 2, 3, 4),
    0,
    Set(2, 4),
    Map(
      0 -> Map('a' -> 1, 'b' -> 3),
      1 -> Map('a' -> 2, 'b' -> 4),
      2 -> Map('a' -> 2, 'b' -> 4),
      3 -> Map('a' -> 1, 'b' -> 3),
      4 -> Map('a' -> 1, 'b' -> 3)
    )
  )

  "Single char FSMs" should "pass simple check" in {
    val fsmA = createFsmA
    assert(!fsmA.IsEmpty)
    assert(!fsmA.Accepts(""))
    assert(!fsmA.Accepts("b"))
    assert(fsmA.Accepts("a"))

    val fsmB = createFsmB
    assert(!fsmB.IsEmpty)
    assert(!fsmB.Accepts(""))
    assert(fsmB.Accepts("b"))
    assert(!fsmB.Accepts("a"))
  }

  "Epsilon FSM" should "be empty" in {
    val epsilonFsmA = Fsm.EpsilonFsm(Set('a'))
    assert(epsilonFsmA.Accepts(""))
    assert(!epsilonFsmA.Accepts("a"))

    val epsilonFsmAB = Fsm.EpsilonFsm(Set('a', 'b'))
    assert(!epsilonFsmAB.IsEmpty)

    assert(Fsm(Set(), Set(0, 1), 0, Set(1), Map(0 -> Map(), 1 -> Map()))
      .IsEmpty)

    assert(!Fsm(Set(), Set(0), 0, Set(0), Map(0 -> Map())).IsEmpty)

    assert(Fsm(Set(), Set(0,1), 1, Set(0), Map(0 -> Map())).IsEmpty)

    assert(Fsm(Set('a', 'b'), Set(0, 1, _ob, 2), 0, Set(2),
      Map(
        0 -> Map('a' -> 1, 'b' -> 1),
        1 -> Map('a' -> _ob, 'b' -> _ob),
        _ob -> Map('a' -> _ob, 'b' -> _ob),
        2 -> Map('a' -> _ob, 'b' -> _ob),
      )).IsEmpty)
  }

  "Null FSM" should "behave as expected" in {
    val nullFsm = Fsm.NullFsm(Set('a'))
    assert(!nullFsm.Accepts("a"))
    assert(!nullFsm.Accepts(""))
  }

  "IsLive" should "succeed for fsmA" in {
    val fsmA = createFsmA
    assertResult(true) { fsmA.IsLive(0) }
    assertResult(true) { fsmA.IsLive(1) }
    assertResult(false) { fsmA.IsLive(_ob) }
  }

  "IsEmpty" should "pass basic check" in {
    assert(!createFsmA.IsEmpty)
    assert(!createFsmB.IsEmpty)

    val fsmE1 = Fsm(Set(), Set(0, 1), 0, Set(1), Map(0 -> Map(), 1 -> Map()))
    assert(fsmE1.IsEmpty)

    val fsmE2 = Fsm(Set(), Set(0), 0, Set(0), Map(0 -> Map()))
    assert(!fsmE2.IsEmpty)

    val fsmE3 = Fsm(Set('a', 'b'), Set(0, 1, _ob, 2), 0, Set(2), Map(
      0 -> Map('a' -> 1, 'b' -> 1),
      1 -> Map('a' -> _ob, 'b' -> _ob),
      _ob -> Map('a' -> _ob, 'b' -> _ob),
      2 -> Map('a' -> _ob, 'b' -> _ob)))

    assert(fsmE3.IsEmpty)
  }

  "Fsm ABC" should "accept abs" in {
    val fsmAbc = createFsmAbc
    assert(fsmAbc.Accepts("abc"))
  }

  "Fsm" should "be reversible" in {
    val fsmAbc = createFsmAbc
    assert(fsmAbc.Accepts("abc"))
    val fsmCba = fsmAbc.Reversed
    assert(fsmCba.Accepts("cba"))
  }

  "Brzozowski" should "be reversible" in {
    val fsmBr = createBrzozowski //This is (a|b)*a(a|b)

    assert(fsmBr.Accepts("aa"))
    assert(fsmBr.Accepts("ab"))
    assert(fsmBr.Accepts("aab"))
    assert(fsmBr.Accepts("bab"))
    assert(fsmBr.Accepts("abbbbbbbab"))

    assert(!fsmBr.Accepts(""))
    assert(!fsmBr.Accepts("a"))
    assert(!fsmBr.Accepts("b"))
    assert(!fsmBr.Accepts("ba"))
    assert(!fsmBr.Accepts("bb"))
    assert(!fsmBr.Accepts("bbbbbbbbbbbb"))

    val revBr = fsmBr.Reversed

    assert(revBr.Accepts("aa"))
    assert(revBr.Accepts("ba"))
    assert(revBr.Accepts("baa"))
    assert(revBr.Accepts("bab"))
    assert(revBr.Accepts("babbbbbbba"))

    assert(!revBr.Accepts(""))
    assert(!revBr.Accepts("a"))
    assert(!revBr.Accepts("b"))
    assert(!revBr.Accepts("ab"))
    assert(!revBr.Accepts("bb"))
    assert(!revBr.Accepts("bbbbbbbbbbbb"))
  }

  "Reversed Epsilon" should "stay Epsilon" in {
    val epsA = Fsm.EpsilonFsm(Set('a'))
    val revEpsA = epsA.Reversed
    assert(revEpsA.Accepts(""))
  }

  "Fsm" should "be inversible" in {
    val fsmA = createFsmA
    val notA = fsmA.EverythingBut

    assert(notA.Accepts(""))
    assert(!notA.Accepts("a"))
    assert(notA.Accepts("b"))
    assert(notA.Accepts("aa"))
    assert(notA.Accepts("ab"))
  }

  "Anything Else" should "be accepted" in {
    val fsm = Fsm(
      Set('a','b', 'c', Fsm.AnythingElse),
      Set(1),
      1,
      Set(1),
      Map(1 -> Map('a' -> 1, 'b'->1, 'c'->1, Fsm.AnythingElse -> 1))
    )

    assert(fsm.Accepts("a"))
    assert(fsm.Accepts("b"))
    assert(fsm.Accepts("c"))
    assert(fsm.Accepts("d"))
  }

  /** this is "0*1" in heavy disguise. crawl should resolve this duplication
   Notice how states 2 and 3 behave identically. When resolved together,
  states 1 and 2&3 also behave identically, so they, too should be resolved
  (this is impossible to spot before 2 and 3 have been combined).
  Finally, the oblivion state should be omitted.*/
  "Crawl reduction" should "do it's job" in {
    val merged = Fsm(Set('0', '1'),
      Set(1, 2, 3, 4, _ob),
      1,
      Set(4),
      Map(1 -> Map('0' -> 2, '1' -> 4),
        2 -> Map('0' -> 3, '1' -> 4),
        3 -> Map('0' -> 3, '1' -> 4),
        4 -> Map('0' -> _ob, '1' -> _ob),
        _ob -> Map('0' -> _ob, '1' -> _ob))
    )

    assert(merged.Reversed.states.size == 2)
  }

  "Cardinality" should "calculate length of the states set" in {
    val fsmA = createFsmA
    assert(createFsmA.Cardinality.contains(1))

    val fsmAbc = createFsmAbc
    assert(createFsmA.Cardinality.contains(1))

    val nullFsm = Fsm.NullFsm(Set('a'))
    assert(nullFsm.Cardinality.contains(0))

    val epsFsm = Fsm.EpsilonFsm(Set('a'))
    assert(nullFsm.Cardinality.contains(0))

    //check for recursion
    val brFsm = createBrzozowski
    assert(brFsm.Cardinality.isEmpty)
  }

  "Reduce" should "remove unreachable states" in {
    val fsm = Fsm(Set('a'),
      Set(0, 1, 2),
      0,
      Set(1),
      Map(0 -> Map('a' -> 2),
        1 -> Map('a' -> 2),
        2 -> Map('a' -> 2)
      ))

    assert(fsm.IsEmpty)
    assert(!fsm.Accepts("a"))

    val reduced = fsm.Reduced
    assert(reduced.states.size == 1)

    assert(reduced.IsEmpty)
    assert(!reduced.Accepts("a"))
  }

  "Concatenate" should "pass basic check" in {
    val fsmEmpty = Fsm.Concatenate()
    assert(fsmEmpty.Strings.toList == Nil)

    val fsmA = createFsmA
    assert(fsmA.Concatenate(fsmA, fsmA).Strings.toList == "aaa" :: Nil)
    assert(fsmA.Concatenate().Strings.toList == "a" :: Nil)

    val fsmBAB = Fsm.Concatenate(createFsmB, createFsmA, createFsmB)
    assert(fsmBAB.Strings.toList == "bab" :: Nil)

    val fsmAA = fsmA + fsmA
    assert(!fsmAA.Accepts(""))
    assert(!fsmAA.Accepts("a"))
    assert(fsmAA.Accepts("aa"))
    assert(!fsmAA.Accepts("aaa"))

    val fsmAB = createFsmA + createFsmB
    assert(!fsmAB.Accepts(""))
    assert(!fsmAB.Accepts("a"))
    assert(!fsmAB.Accepts("b"))
    assert(!fsmAB.Accepts("aa"))
    assert(!fsmAB.Accepts("bb"))
    assert(fsmAB.Accepts("ab"))
    assert(!fsmAB.Accepts("ba"))
    assert(!fsmAB.Accepts("aaa"))
    assert(!fsmAB.Accepts("bbb"))
  }

  "Concatenate with Epsilon" should "have no defect" in {
    val fsmA = createFsmA
    val epsA = Fsm.EpsilonFsm(Set('a', 'b'))

    val concatAeA = Fsm.Concatenate(fsmA, epsA, fsmA)
    assert(!concatAeA.Accepts(""))
    assert(!concatAeA.Accepts("a"))
    assert(concatAeA.Accepts("aa"))
    assert(!concatAeA.Accepts("aaa"))

    val concatAeeA = Fsm.Concatenate(fsmA, epsA, epsA, fsmA)
    assert(!concatAeA.Accepts(""))
    assert(!concatAeA.Accepts("a"))
    assert(concatAeA.Accepts("aa"))
    assert(!concatAeA.Accepts("aaa"))

    val concatEeAA = Fsm.Concatenate(epsA, epsA, fsmA, fsmA)
    assert(!concatAeA.Accepts(""))
    assert(!concatAeA.Accepts("a"))
    assert(concatAeA.Accepts("aa"))
    assert(!concatAeA.Accepts("aaa"))
  }

  // original greenery test named: "test_addbug"
  "Concatenate [bc]*c" should "work" in {
    val fsm1 = Fsm(Set('a', 'b', 'c', Fsm.AnythingElse),
      Set(0, 1), 1, Set(1), Map(
        0 -> Map(Fsm.AnythingElse -> 0, 'a' -> 0, 'b' -> 0, 'c' -> 0),
        1 -> Map(Fsm.AnythingElse -> 0, 'a' -> 0, 'b' -> 1, 'c' -> 1)))

    assert(fsm1.Accepts(""))

    val fsm2 = Fsm(Set('a', 'b', 'c', Fsm.AnythingElse),
      Set(0, 1, 2), 1, Set(0), Map(
        0 -> Map(Fsm.AnythingElse -> 2, 'a' -> 2, 'b' -> 2, 'c' -> 2),
        1 -> Map(Fsm.AnythingElse -> 2, 'a' -> 2, 'b' -> 2, 'c' -> 0),
        2 -> Map(Fsm.AnythingElse -> 2, 'a' -> 2, 'b' -> 2, 'c' -> 2)))

    assert(fsm2.Accepts("c"))

    val fsm12 = fsm1 + fsm2
    assert(fsm12.Accepts("c"))
  }

  // Thanks to sparse maps it should now be possible to compute the union of FSMs
  // disagreeing alphabets!
  "Disagreeing alphabets" should "have valid unions" in {
    val fsmA = Fsm(Set('a'), Set(0, 1), 0, Set(1), Map(0 -> Map('a' -> 1)))
    val fsmB = Fsm(Set('b'), Set(0, 1), 0, Set(1), Map(0 -> Map('b' -> 1)))

    assert((fsmA | fsmB).Accepts("a"))
    assert((fsmA | fsmB).Accepts("b"))

    assert((fsmA & fsmB).IsEmpty)

    assert((fsmA + fsmB).Accepts("ab"))

    assert((fsmA ^ fsmB).Accepts("a"))
    assert((fsmA ^ fsmB).Accepts("b"))
  }

  "Star" should "pass basic check" in {
    var fsmA = createFsmA
    var starA = fsmA.Star

    assert(starA.Accepts(""))
    assert(starA.Accepts("a"))
    assert(!starA.Accepts("b"))
    assert(starA.Accepts("aaaaaaaaa"))
  }

  /** This is (a*ba)*. Naively connecting the final states to the initial state
	gives the incorrect result here. */
  "Star" should "pass advanced check" in {
    val fsmS = Fsm(Set('a', 'b'),
      Set(0, 1, 2, _ob),
      0,
      Set(2), Map(
        0 -> Map('a' -> 0, 'b' -> 1),
        1 -> Map('a' -> 2, 'b' -> _ob),
        2 -> Map('a' -> _ob, 'b' -> _ob),
        _ob -> Map('a'->_ob, 'b' ->_ob))).Star

    assert(fsmS.alphabet == Set('a', 'b'))
    assert(fsmS.Accepts(""))

    assert(!fsmS.Accepts("a"))
    assert(!fsmS.Accepts("b"))
    assert(!fsmS.Accepts("aa"))
    assert(!fsmS.Accepts("aabb"))

    assert(fsmS.Accepts("ba"))
    assert(fsmS.Accepts("aba"))
    assert(fsmS.Accepts("aaba"))
    assert(fsmS.Accepts("abababa"))
  }

  "Fsm (ab*)*" should "work properly (greenery bug 28)" in {
    val abStar = Fsm(Set('a','b'),
      Set(0,1),
      0,
      Set(1),
      Map(0 -> Map('a' -> 1),
        1 -> Map('b' -> 1)))

    assert(abStar.Accepts("a"))
    assert(!abStar.Accepts("b"))
    assert(abStar.Accepts("ab"))
    assert(abStar.Accepts("abb"))

    val abstarstar = abStar.Star
    assert(abstarstar.Accepts("a"))
    assert(!abstarstar.Accepts("b"))
    assert(abstarstar.Accepts("ab"))
    assert(!abstarstar.Accepts("bb"))
  }

  "Derive" should "pass basic check" in {
    val fsmA = createFsmA
    val fsmB = createFsmB

    assert(fsmA.Derive("a") == Fsm.EpsilonFsm(Set('a','b')))
    assert(fsmA.Derive("b") == Fsm.NullFsm(Set('a','b')))

    assertThrows[NoSuchElementException] {
      fsmA.Derive("c")
    }

    assert(fsmA.Derive("a") == Fsm.EpsilonFsm(Set('a', 'b')))
    assert(fsmA.Derive("b") == Fsm.NullFsm(Set('a', 'b')))
    assert((fsmA.Star - Fsm.EpsilonFsm(Set('a','b'))).Derive("a") == fsmA.Star)
    assert((fsmA * 3).Derive("a") == fsmA * 2)
  }

  "Multiply" should "fail on multiplier <0" in {
    assertThrows[IllegalArgumentException](createFsmA * -1)
  }

  "Multiply" should "correctly multiply by 0" in {
    val zeroA = createFsmA * 0
    assert(zeroA.Accepts(""))
    assert(!zeroA.Accepts("a"))
  }

  "Multiply" should "correctly multiply by 1" in {
    val fsmA = createFsmA * 1
    assert(!fsmA.Accepts(""))
    assert(fsmA.Accepts("a"))
    assert(!fsmA.Accepts("aa"))
  }

  "Multiply" should "correctly multiply by 2" in {
    val twoA = createFsmA * 2
    assert(!twoA.Accepts(""))
    assert(!twoA.Accepts("a"))
    assert(twoA.Accepts("aa"))
    assert(!twoA.Accepts("aaa"))
  }

  "Multiply" should "correctly multiply by 7" in {
    val sevenA = createFsmA * 7
    assert(!sevenA.Accepts("aaaaaa"))
    assert(sevenA.Accepts("aaaaaaa"))
    assert(!sevenA.Accepts("aaaaaaaa"))
  }

  "Multiply" should "correctly applied to unions" in {
    val fsmAB = createFsmA + createFsmB
    val fsmOpt = Fsm.EpsilonFsm(createFsmA.alphabet) | fsmAB //accepts (ab)?

    assert(fsmOpt.Accepts(""))
    assert(!fsmOpt.Accepts("a"))
    assert(!fsmOpt.Accepts("b"))
    assert(fsmOpt.Accepts("ab"))
    assert(!fsmOpt.Accepts("aa"))

    val fsmOpt2 = fsmOpt * 2 //accepts (ab)?(ab)?
    assert(fsmOpt2.Accepts(""))
    assert(!fsmOpt2.Accepts("a"))
    assert(!fsmOpt2.Accepts("b"))
    assert(!fsmOpt2.Accepts("aa"))
    assert(!fsmOpt2.Accepts("bb"))
    assert(!fsmOpt2.Accepts("ba"))
    assert(!fsmOpt2.Accepts("aba"))
    assert(!fsmOpt2.Accepts("abba"))
    assert(!fsmOpt2.Accepts("bab"))
    assert(!fsmOpt2.Accepts("baba"))

    assert(fsmOpt2.Accepts("abab"))
    assert(fsmOpt2.Accepts("ab"))
  }

  "Intersection" should "produce correct FSM" in {
    val fsmA = createFsmA
    val fsmB = createFsmB

    val fsmAB = fsmA & fsmB

    assert(!fsmAB.Accepts(""))
    assert(!fsmAB.Accepts("a"))
    assert(!fsmAB.Accepts("b"))
  }

  "Union with Null" should "pass basic check" in {
    val fsmA = createFsmA | Fsm.NullFsm(Set('a', 'b'))
    assert(!fsmA.Accepts(""))
    assert(fsmA.Accepts("a"))
    assert(!fsmA.Accepts("aa"))
    assert(!fsmA.Accepts("b"))
  }

  "Union" should "produce correct FSM" in {
    val fsmA = createFsmA
    val fsmB = createFsmB

    val fsmAB = fsmA | fsmB

    assert(!fsmAB.Accepts(""))
    assert(fsmAB.Accepts("a"))
    assert(fsmAB.Accepts("b"))
    assert(!fsmAB.Accepts("aa"))
    assert(!fsmAB.Accepts("ab"))
    assert(!fsmAB.Accepts("ba"))
    assert(!fsmAB.Accepts("bb"))
  }

  "Difference" should "work" in {
    val fsmAorB = Fsm(Set('a', 'b'),
      Set(0, 1, _ob),
      0,
      Set(1),
      Map(0 -> Map('a' -> 1, 'b' -> 1),
        1 -> Map('a' -> _ob, 'b' -> _ob),
        _ob -> Map('a' -> _ob, 'b' -> _ob)))

    val fsmA = createFsmA
    val fsmB = createFsmB

    assert((fsmA ^ fsmA).Strings.isEmpty)
    assert((fsmB ^ fsmB).Strings.isEmpty)
    assert((fsmA ^ fsmB).Strings.toList == "a" :: "b" :: Nil)
    assert((fsmAorB ^ fsmA).Strings.toList == "b" :: Nil)
    assert((fsmAorB ^ fsmB).Strings.toList == "a" :: Nil)
  }

  "String generator" should "generate all strings" in {
    val fsmBr = createBrzozowski //This is (a|b)*a(a|b)
    val check = "aa" :: "ab" :: "aaa" :: "aab" :: "baa" :: "bab" :: "aaaa" :: Nil
    assert(fsmBr.Strings.take(7).toList == check)
  }

  //FSM accepts no strings but has 3 states, needs only 1
  "Reduce" should "eliminate unused states" in {
    val fsm3 = Fsm(Set('a'), Set(0, 1, 2), 0, Set(1), Map(
      0 -> Map('a' -> 2),
      1 -> Map('a' -> 2),
      2 -> Map('a' -> 2)
    ))

    val fsm1 = fsm3.Reduced
    assert(fsm1.states.size == 1)
  }

//  this is "0*1" in heavy disguise. crawl should resolve this duplication
//  Notice how states 2 and 3 behave identically. When resolved together,
//  states 1 and 2&3 also behave identically, so they, too should be resolved
//  (this is impossible to spot before 2 and 3 have been combined).
//  Finally, the oblivion state should be omitted.
  "Crawl reduction" should "resolve duplication" in {
    val mergedFsm = Fsm(Set('a', 'b'),
      Set(1, 2, 3, 4, _ob),
      1,
      Set(4),
      Map(1 -> Map('a' -> 2, 'b' -> 4),
        2 -> Map('a' -> 3, 'b' -> 4),
        3 -> Map('a' -> 3, 'b' -> 4),
        4 -> Map('a' -> _ob, 'b' -> _ob),
        _ob -> Map('a' -> _ob, 'b' -> _ob)))

    val reducedFsm = mergedFsm.Reduced

    assert(reducedFsm.states.size == 2)
  }

  "Equivalent" should "pass basic check" in {
    val fsmA = createFsmA
    val fsmB = createFsmB

    val fsmAB = fsmA | fsmB
    val fsmBA = fsmB | fsmA

    assert(fsmAB.Equivalent(fsmBA))
    assert(fsmAB == fsmBA)//ensure proper == overload
  }

  // Binary numbers divisible by 3.
  // Disallows the empty string
  // Allows "0" on its own, but not leading zeroes.
  "Binary Div 3" should "work" in {
    val _init = -2
    val _zero = -1 //states 0 1 and 2 are for division results
    val fsm3 = Fsm(Set('0', '1'), Set(_init, _zero, 0, 1, 2, _ob), _init, Set(_zero, 0), Map(
      _init -> Map('0' -> _zero, '1' -> 1),
      _zero -> Map('0' -> _ob, '1' -> _ob),
      0 -> Map('0' -> 0, '1' -> 1),
      1 -> Map('0' -> 2, '1' -> 0),
      2 -> Map('0' -> 1, '1' -> 2),
      _ob -> Map('0' -> _ob, '1' -> _ob)
    ))

    assert(!fsm3.Accepts(""))
    assert(fsm3.Accepts("0"))
    assert(!fsm3.Accepts("1"))
    assert(!fsm3.Accepts("00"))
    assert(!fsm3.Accepts("01"))
    assert(!fsm3.Accepts("10"))

    assert(fsm3.Accepts("11"))

    assert(!fsm3.Accepts("000"))
    assert(!fsm3.Accepts("001"))
    assert(!fsm3.Accepts("010"))
    assert(!fsm3.Accepts("011")) //3, but we don't allow leading zeroes
    assert(!fsm3.Accepts("100"))
    assert(!fsm3.Accepts("101"))

    assert(fsm3.Accepts("110"))

    assert(!fsm3.Accepts("111"))

    assert(!fsm3.Accepts("0000"))
    assert(!fsm3.Accepts("0001"))
    assert(!fsm3.Accepts("0010"))
    assert(!fsm3.Accepts("0011"))
    assert(!fsm3.Accepts("0100"))
    assert(!fsm3.Accepts("0101"))
    assert(!fsm3.Accepts("0110"))
    assert(!fsm3.Accepts("0111"))
    assert(!fsm3.Accepts("1000"))

    assert(fsm3.Accepts("1001"))
  }

  "Oblivion crawl" should "avoid generating an oblivion state when crawling a new FSM" in {
    val abc = Fsm(
      Set('a', 'b', 'c'),
      Set(0, 1, 2, 3),
      0,
      Set(3),
      Map(
        0 -> Map('a' -> 1),
        1 -> Map('b' -> 2),
        2 -> Map('c' -> 3)
      )
    )

    assert((abc - abc).states.size == 1)
    assert((abc + abc).states.size == 7)
    assert(abc.Star.states.size == 3)
    assert((abc * 3).states.size == 10)
    assert((abc | abc).states.size == 4)
    assert((abc & abc).states.size == 4)
    assert((abc ^ abc).states.size == 1)
  }

  // You may now omit a transition, or even an entire state, from the map. This
  // affects every usage of `fsm.transitions`.
  "Dead states" should "be allowed" in {
    val fsm = Fsm(Set('/', '*', Fsm.AnythingElse), Set(0, 1, 2, 3, 4, 5), 0, Set(4), Map(
      0 -> Map('/' -> 1),
      1 -> Map('*' -> 2),
      2 -> Map('/' -> 2, Fsm.AnythingElse -> 2, '*' -> 3),
      3 -> Map('/' -> 4, Fsm.AnythingElse -> 2, '*' -> 3)))

    assert(fsm.Strings.take(1).toList == "/**/" :: Nil)

    assert(fsm.IsLive(3))
    assert(fsm.IsLive(4))
    assert(!fsm.IsLive(5))

    assert(fsm.Accepts("/* whatever */"))
    assert(!fsm.Accepts("** whatever */"))

    val but = fsm.EverythingBut
    assert(!but.Accepts("/* whatever */"))
    assert(but.Accepts("*"))//deliberately seek oblivion
  }

  "Fsm's" should "be have properties similar to List[String]" in {
    val fsmA = createFsmA
    val fsmB = createFsmB

    assert(fsmA.Length.contains(1))
    assert(((fsmA | fsmB)*4).Length.contains(16))
    assert(fsmA.Star.Length.isEmpty)

    assert(Fsm.Union(fsmA, fsmB) == fsmA.Union(fsmB))
    assert(Fsm.Union().Length.contains(0))
    assert(Fsm.Intersection(fsmA, fsmB) == fsmA.Intersection(fsmB))
  }

  // In general, `a & b & c` is equivalent to
  // `EVERYTHING & a & b & c` where `EVERYTHING` is an FSM accepting every
  // possible string. Similarly `a` is equivalent to `EVERYTHING & a`, and the
  // intersection of no sets at all is... `EVERYTHING`.
  // However, since we compute the union of alphabets, and there are no
  // alphabets, the union is the empty set. So the only string which `EVERYTHING`
  // actually recognises is the empty string, [] (or "" if you prefer).
  "Logical operations" should "behave as expected" in {
    val fsmNone = Fsm.Intersection()
    assert(fsmNone.Length.contains(1))
    assert(fsmNone.Strings.toList == "" :: Nil)

    val fsmA = createFsmA
    val fsmB = createFsmB

    assert((fsmA | fsmB).Difference(fsmA) == fsmB)
    assert((fsmA | fsmB) - fsmA - fsmB == Fsm.NullFsm(Set('a', 'b')))
    assert(fsmA.IsDisjoint(fsmB))
    assert(fsmA <= (fsmA | fsmB))
    assert(fsmA < (fsmA | fsmB))
    assert(fsmA != (fsmA | fsmB))
    assert((fsmA | fsmB) > fsmA)
    assert((fsmA | fsmB) >= fsmB)
  }

  "Invalid Fsm" should "fail if alphabet null" in {
    assertThrows[IllegalArgumentException] {
      val fsm1 = Fsm(null, null, 0, null, null)
    }
  }

  "Invalid Fsm" should "fail if initial state is not a state" in {
    assertThrows[IllegalArgumentException] {
      val fsm1 = Fsm(Set('a'), Set(0), 1, Set(0), Map())
    }
  }

  "Invalid Fsm" should "fail if final state is not a state" in {
    assertThrows[IllegalArgumentException] {
      val fsm1 = Fsm(Set('a'), Set(1), 1, Set(2), Map())
    }
  }

  "Invalid Fsm" should "fail if unexpected transition state detected" in {
    assertThrows[IllegalArgumentException] {
      val fsm1 = Fsm(Set('a'), Set(1, 2), 1, Set(2), Map(1 -> Map('a' -> 3)))
    }
  }
}
