package coop.rchain.regex
import org.scalatest.{FlatSpec, Matchers}

class FsmUnitTests extends FlatSpec with Matchers {
  def ignore[A](a: => A): Unit = {
    val _: A = a
    ()
  }

  val _ob: Int = -5

  def createFsmA: Fsm = Fsm(
    Set('a', 'b'),
    Set(0, 1, _ob),
    0,
    Set(1),
    Map(
      0   -> Map('a' -> 1, 'b'   -> _ob),
      1   -> Map('a' -> _ob, 'b' -> _ob),
      _ob -> Map('a' -> _ob, 'b' -> _ob)
    )
  )

  def createFsmB: Fsm = Fsm(
    Set('a', 'b'),
    Set(0, 1, _ob),
    0,
    Set(1),
    Map(
      0   -> Map('a' -> _ob, 'b' -> 1),
      1   -> Map('a' -> _ob, 'b' -> _ob),
      _ob -> Map('a' -> _ob, 'b' -> _ob)
    )
  )

  def createFsmAbc: Fsm =
    Fsm(
      Set('a', 'b', 'c'),
      Set(0, 1, 2, 3, _ob),
      0,
      Set(3),
      Map(
        0 -> Map('a' -> 1, 'b'   -> _ob, 'c' -> _ob),
        1 -> Map('a' -> _ob, 'b' -> 2, 'c'   -> _ob),
        2 -> Map('a' -> _ob, 'b' -> _ob, 'c' -> 3),
        3 -> Map('a' -> _ob, 'b' -> _ob, 'c' -> _ob)
      )
    )

  def createBrzozowski: Fsm = Fsm(
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
    assert(!fsmA.isEmpty)
    assert(!fsmA.accepts(""))
    assert(!fsmA.accepts("b"))
    assert(fsmA.accepts("a"))

    val fsmB = createFsmB
    assert(!fsmB.isEmpty)
    assert(!fsmB.accepts(""))
    assert(fsmB.accepts("b"))
    assert(!fsmB.accepts("a"))
  }

  "Epsilon FSM" should "be empty" in {
    val epsilonFsmA = Fsm.epsilonFsm(Set('a'))
    assert(epsilonFsmA.accepts(""))
    assert(!epsilonFsmA.accepts("a"))

    val epsilonFsmAB = Fsm.epsilonFsm(Set('a', 'b'))
    assert(!epsilonFsmAB.isEmpty)

    assert(Fsm(Set(), Set(0, 1), 0, Set(1), Map(0 -> Map(), 1 -> Map())).isEmpty)

    assert(!Fsm(Set(), Set(0), 0, Set(0), Map(0 -> Map())).isEmpty)

    assert(Fsm(Set(), Set(0, 1), 1, Set(0), Map(0 -> Map())).isEmpty)

    assert(
      Fsm(
        Set('a', 'b'),
        Set(0, 1, _ob, 2),
        0,
        Set(2),
        Map(
          0   -> Map('a' -> 1, 'b'   -> 1),
          1   -> Map('a' -> _ob, 'b' -> _ob),
          _ob -> Map('a' -> _ob, 'b' -> _ob),
          2   -> Map('a' -> _ob, 'b' -> _ob)
        )
      ).isEmpty
    )
  }

  "Null FSM" should "behave as expected" in {
    val nullFsm = Fsm.nullFsm(Set('a'))
    assert(!nullFsm.accepts("a"))
    assert(!nullFsm.accepts(""))
  }

  "IsLive" should "succeed for fsmA" in {
    val fsmA = createFsmA
    assertResult(true) { fsmA.isLive(0) }
    assertResult(true) { fsmA.isLive(1) }
    assertResult(false) { fsmA.isLive(_ob) }
  }

  "IsEmpty" should "pass basic check" in {
    assert(!createFsmA.isEmpty)
    assert(!createFsmB.isEmpty)

    val fsmE1 = Fsm(Set(), Set(0, 1), 0, Set(1), Map(0 -> Map(), 1 -> Map()))
    assert(fsmE1.isEmpty)

    val fsmE2 = Fsm(Set(), Set(0), 0, Set(0), Map(0 -> Map()))
    assert(!fsmE2.isEmpty)

    val fsmE3 = Fsm(
      Set('a', 'b'),
      Set(0, 1, _ob, 2),
      0,
      Set(2),
      Map(
        0   -> Map('a' -> 1, 'b'   -> 1),
        1   -> Map('a' -> _ob, 'b' -> _ob),
        _ob -> Map('a' -> _ob, 'b' -> _ob),
        2   -> Map('a' -> _ob, 'b' -> _ob)
      )
    )

    assert(fsmE3.isEmpty)
  }

  "Fsm ABC" should "accept abs" in {
    val fsmAbc = createFsmAbc
    assert(fsmAbc.accepts("abc"))
  }

  "Fsm" should "be reversible" in {
    val fsmAbc = createFsmAbc
    assert(fsmAbc.accepts("abc"))
    val fsmCba = fsmAbc.reversed
    assert(fsmCba.accepts("cba"))
  }

  "Brzozowski" should "be reversible" in {
    val fsmBr = createBrzozowski
    //fsmBr now (a|b)*a(a|b)
    assert(fsmBr.accepts("aa"))
    assert(fsmBr.accepts("ab"))
    assert(fsmBr.accepts("aab"))
    assert(fsmBr.accepts("bab"))
    assert(fsmBr.accepts("abbbbbbbab"))

    assert(!fsmBr.accepts(""))
    assert(!fsmBr.accepts("a"))
    assert(!fsmBr.accepts("b"))
    assert(!fsmBr.accepts("ba"))
    assert(!fsmBr.accepts("bb"))
    assert(!fsmBr.accepts("bbbbbbbbbbbb"))

    val revBr = fsmBr.reversed

    assert(revBr.accepts("aa"))
    assert(revBr.accepts("ba"))
    assert(revBr.accepts("baa"))
    assert(revBr.accepts("bab"))
    assert(revBr.accepts("babbbbbbba"))

    assert(!revBr.accepts(""))
    assert(!revBr.accepts("a"))
    assert(!revBr.accepts("b"))
    assert(!revBr.accepts("ab"))
    assert(!revBr.accepts("bb"))
    assert(!revBr.accepts("bbbbbbbbbbbb"))
  }

  "Reversed Epsilon" should "stay Epsilon" in {
    val epsA    = Fsm.epsilonFsm(Set('a'))
    val revEpsA = epsA.reversed
    assert(revEpsA.accepts(""))
  }

  "Fsm" should "be inversible" in {
    val fsmA = createFsmA
    val notA = fsmA.everythingBut

    assert(notA.accepts(""))
    assert(!notA.accepts("a"))
    assert(notA.accepts("b"))
    assert(notA.accepts("aa"))
    assert(notA.accepts("ab"))
  }

  "Anything Else" should "be accepted" in {
    val fsm = Fsm(
      Set('a', 'b', 'c', Fsm.anythingElse),
      Set(1),
      1,
      Set(1),
      Map(1 -> Map('a' -> 1, 'b' -> 1, 'c' -> 1, Fsm.anythingElse -> 1))
    )

    assert(fsm.accepts("a"))
    assert(fsm.accepts("b"))
    assert(fsm.accepts("c"))
    assert(fsm.accepts("d"))
  }

  /**
    * this is "0*1" in heavy disguise. crawl should resolve this duplication
    * Notice how states 2 and 3 behave identically. When resolved together,
    * states 1 and 2&3 also behave identically, so they, too should be resolved
    * (this is impossible to spot before 2 and 3 have been combined).
    * Finally, the oblivion state should be omitted.
    */
  "Crawl reduction" should "do it's job" in {
    val merged = Fsm(
      Set('0', '1'),
      Set(1, 2, 3, 4, _ob),
      1,
      Set(4),
      Map(
        1   -> Map('0' -> 2, '1'   -> 4),
        2   -> Map('0' -> 3, '1'   -> 4),
        3   -> Map('0' -> 3, '1'   -> 4),
        4   -> Map('0' -> _ob, '1' -> _ob),
        _ob -> Map('0' -> _ob, '1' -> _ob)
      )
    )

    assert(merged.reversed.states.size == 2)
  }

  "Cardinality" should "calculate length of the states set" in {
    val fsmA = createFsmA
    assert(fsmA.cardinality.contains(1))

    val fsmAbc = createFsmAbc
    assert(fsmAbc.cardinality.contains(1))

    val nullFsm = Fsm.nullFsm(Set('a'))
    assert(nullFsm.cardinality.contains(0))

    val epsFsm = Fsm.epsilonFsm(Set('a'))
    assert(epsFsm.cardinality.contains(1))

    //check for recursion
    val brFsm = createBrzozowski
    assert(brFsm.cardinality.isEmpty)
  }

  "Reduce" should "remove unreachable states" in {
    val fsm = Fsm(
      Set('a'),
      Set(0, 1, 2),
      0,
      Set(1),
      Map(0 -> Map('a' -> 2), 1 -> Map('a' -> 2), 2 -> Map('a' -> 2))
    )

    assert(fsm.isEmpty)
    assert(!fsm.accepts("a"))

    val reduced = fsm.reduced
    assert(reduced.states.size == 1)

    assert(reduced.isEmpty)
    assert(!reduced.accepts("a"))
  }

  "Concatenate" should "pass basic check" in {
    val fsmEmpty = Fsm.concatenate()
    assert(fsmEmpty.strings.toList == Nil)

    val fsmA = createFsmA
    assert(fsmA.concatenate(fsmA, fsmA).strings.toList == "aaa" :: Nil)
    assert(fsmA.concatenate().strings.toList == "a" :: Nil)

    val fsmBAB = Fsm.concatenate(createFsmB, createFsmA, createFsmB)
    assert(fsmBAB.strings.toList == "bab" :: Nil)

    val fsmAA = fsmA + fsmA
    assert(!fsmAA.accepts(""))
    assert(!fsmAA.accepts("a"))
    assert(fsmAA.accepts("aa"))
    assert(!fsmAA.accepts("aaa"))

    val fsmAB = createFsmA + createFsmB
    assert(!fsmAB.accepts(""))
    assert(!fsmAB.accepts("a"))
    assert(!fsmAB.accepts("b"))
    assert(!fsmAB.accepts("aa"))
    assert(!fsmAB.accepts("bb"))
    assert(fsmAB.accepts("ab"))
    assert(!fsmAB.accepts("ba"))
    assert(!fsmAB.accepts("aaa"))
    assert(!fsmAB.accepts("bbb"))
  }

  "Concatenate with Epsilon" should "have no defect" in {
    val fsmA = createFsmA
    val epsA = Fsm.epsilonFsm(Set('a', 'b'))

    val concatAeA = Fsm.concatenate(fsmA, epsA, fsmA)
    assert(!concatAeA.accepts(""))
    assert(!concatAeA.accepts("a"))
    assert(concatAeA.accepts("aa"))
    assert(!concatAeA.accepts("aaa"))

    val concatAeeA = Fsm.concatenate(fsmA, epsA, epsA, fsmA)
    assert(!concatAeeA.accepts(""))
    assert(!concatAeeA.accepts("a"))
    assert(concatAeeA.accepts("aa"))
    assert(!concatAeeA.accepts("aaa"))

    val concatEeAA = Fsm.concatenate(epsA, epsA, fsmA, fsmA)
    assert(!concatEeAA.accepts(""))
    assert(!concatEeAA.accepts("a"))
    assert(concatEeAA.accepts("aa"))
    assert(!concatEeAA.accepts("aaa"))
  }

  /**
    * original greenery test named: "test_addbug"
    */
  "Concatenate [bc]*c" should "work" in {
    val fsm1 = Fsm(
      Set('a', 'b', 'c', Fsm.anythingElse),
      Set(0, 1),
      1,
      Set(1),
      Map(
        0 -> Map(Fsm.anythingElse -> 0, 'a' -> 0, 'b' -> 0, 'c' -> 0),
        1 -> Map(Fsm.anythingElse -> 0, 'a' -> 0, 'b' -> 1, 'c' -> 1)
      )
    )

    assert(fsm1.accepts(""))

    val fsm2 = Fsm(
      Set('a', 'b', 'c', Fsm.anythingElse),
      Set(0, 1, 2),
      1,
      Set(0),
      Map(
        0 -> Map(Fsm.anythingElse -> 2, 'a' -> 2, 'b' -> 2, 'c' -> 2),
        1 -> Map(Fsm.anythingElse -> 2, 'a' -> 2, 'b' -> 2, 'c' -> 0),
        2 -> Map(Fsm.anythingElse -> 2, 'a' -> 2, 'b' -> 2, 'c' -> 2)
      )
    )

    assert(fsm2.accepts("c"))

    val fsm12 = fsm1 + fsm2
    assert(fsm12.accepts("c"))
  }

  /**
    * Thanks to sparse maps it should now be possible to compute the union of FSMs
    * disagreeing alphabets!
    */
  "Disagreeing alphabets" should "have valid unions" in {
    val fsmA = Fsm(Set('a'), Set(0, 1), 0, Set(1), Map(0 -> Map('a' -> 1)))
    val fsmB = Fsm(Set('b'), Set(0, 1), 0, Set(1), Map(0 -> Map('b' -> 1)))

    assert((fsmA | fsmB).accepts("a"))
    assert((fsmA | fsmB).accepts("b"))

    assert((fsmA & fsmB).isEmpty)

    assert((fsmA + fsmB).accepts("ab"))

    assert((fsmA ^ fsmB).accepts("a"))
    assert((fsmA ^ fsmB).accepts("b"))
  }

  "Star" should "pass basic check" in {
    val fsmA  = createFsmA
    val starA = fsmA.star

    assert(starA.accepts(""))
    assert(starA.accepts("a"))
    assert(!starA.accepts("b"))
    assert(starA.accepts("aaaaaaaaa"))
  }

  /**
    * This is (a*ba)*. Naively connecting the final states to the initial state
    * gives the incorrect result here.
    */
  "Star" should "pass advanced check" in {
    val fsmS = Fsm(
      Set('a', 'b'),
      Set(0, 1, 2, _ob),
      0,
      Set(2),
      Map(
        0   -> Map('a' -> 0, 'b'   -> 1),
        1   -> Map('a' -> 2, 'b'   -> _ob),
        2   -> Map('a' -> _ob, 'b' -> _ob),
        _ob -> Map('a' -> _ob, 'b' -> _ob)
      )
    ).star

    assert(fsmS.alphabet == Set('a', 'b'))
    assert(fsmS.accepts(""))

    assert(!fsmS.accepts("a"))
    assert(!fsmS.accepts("b"))
    assert(!fsmS.accepts("aa"))
    assert(!fsmS.accepts("aabb"))

    assert(fsmS.accepts("ba"))
    assert(fsmS.accepts("aba"))
    assert(fsmS.accepts("aaba"))
    assert(fsmS.accepts("abababa"))
  }

  "Fsm (ab*)*" should "work properly (greenery bug 28)" in {
    val abStar =
      Fsm(Set('a', 'b'), Set(0, 1), 0, Set(1), Map(0 -> Map('a' -> 1), 1 -> Map('b' -> 1)))

    assert(abStar.accepts("a"))
    assert(!abStar.accepts("b"))
    assert(abStar.accepts("ab"))
    assert(abStar.accepts("abb"))

    val abstarstar = abStar.star
    assert(abstarstar.accepts("a"))
    assert(!abstarstar.accepts("b"))
    assert(abstarstar.accepts("ab"))
    assert(!abstarstar.accepts("bb"))
  }

  "Derive" should "pass basic check" in {
    val fsmA = createFsmA

    assert(fsmA.derive("a").get == Fsm.epsilonFsm(Set('a', 'b')))
    assert(fsmA.derive("b").get == Fsm.nullFsm(Set('a', 'b')))

    assert(fsmA.derive("c").isFailure)

    assert(fsmA.derive("a").get == Fsm.epsilonFsm(Set('a', 'b')))
    assert(fsmA.derive("b").get == Fsm.nullFsm(Set('a', 'b')))
    assert((fsmA.star - Fsm.epsilonFsm(Set('a', 'b'))).derive("a").get == fsmA.star)
    assert((fsmA * 3).derive("a").get == fsmA * 2)
  }

  "Multiply" should "fail on multiplier <0" in {
    assertThrows[IllegalArgumentException](createFsmA * -1)
  }

  "Multiply" should "correctly multiply by 0" in {
    val zeroA = createFsmA * 0
    assert(zeroA.accepts(""))
    assert(!zeroA.accepts("a"))
  }

  "Multiply" should "correctly multiply by 1" in {
    val fsmA = createFsmA * 1
    assert(!fsmA.accepts(""))
    assert(fsmA.accepts("a"))
    assert(!fsmA.accepts("aa"))
  }

  "Multiply" should "correctly multiply by 2" in {
    val twoA = createFsmA * 2
    assert(!twoA.accepts(""))
    assert(!twoA.accepts("a"))
    assert(twoA.accepts("aa"))
    assert(!twoA.accepts("aaa"))
  }

  "Multiply" should "correctly multiply by 7" in {
    val sevenA = createFsmA * 7
    assert(!sevenA.accepts("aaaaaa"))
    assert(sevenA.accepts("aaaaaaa"))
    assert(!sevenA.accepts("aaaaaaaa"))
  }

  "Multiply" should "correctly applied to unions" in {
    val fsmAB  = createFsmA + createFsmB
    val fsmOpt = Fsm.epsilonFsm(createFsmA.alphabet) | fsmAB
    //fsmOpt now accepts (ab)?

    assert(fsmOpt.accepts(""))
    assert(!fsmOpt.accepts("a"))
    assert(!fsmOpt.accepts("b"))
    assert(fsmOpt.accepts("ab"))
    assert(!fsmOpt.accepts("aa"))

    val fsmOpt2 = fsmOpt * 2
    //fsmOpt2 accepts (ab)?(ab)?

    assert(fsmOpt2.accepts(""))
    assert(!fsmOpt2.accepts("a"))
    assert(!fsmOpt2.accepts("b"))
    assert(!fsmOpt2.accepts("aa"))
    assert(!fsmOpt2.accepts("bb"))
    assert(!fsmOpt2.accepts("ba"))
    assert(!fsmOpt2.accepts("aba"))
    assert(!fsmOpt2.accepts("abba"))
    assert(!fsmOpt2.accepts("bab"))
    assert(!fsmOpt2.accepts("baba"))

    assert(fsmOpt2.accepts("abab"))
    assert(fsmOpt2.accepts("ab"))
  }

  "Intersection" should "produce correct FSM" in {
    val fsmA = createFsmA
    val fsmB = createFsmB

    val fsmAB = fsmA & fsmB

    assert(!fsmAB.accepts(""))
    assert(!fsmAB.accepts("a"))
    assert(!fsmAB.accepts("b"))
  }

  "Union with Null" should "pass basic check" in {
    val fsmA = createFsmA | Fsm.nullFsm(Set('a', 'b'))
    assert(!fsmA.accepts(""))
    assert(fsmA.accepts("a"))
    assert(!fsmA.accepts("aa"))
    assert(!fsmA.accepts("b"))
  }

  "Union" should "produce correct FSM" in {
    val fsmA = createFsmA
    val fsmB = createFsmB

    val fsmAB = fsmA | fsmB

    assert(!fsmAB.accepts(""))
    assert(fsmAB.accepts("a"))
    assert(fsmAB.accepts("b"))
    assert(!fsmAB.accepts("aa"))
    assert(!fsmAB.accepts("ab"))
    assert(!fsmAB.accepts("ba"))
    assert(!fsmAB.accepts("bb"))
  }

  "Difference" should "work" in {
    val fsmAorB = Fsm(
      Set('a', 'b'),
      Set(0, 1, _ob),
      0,
      Set(1),
      Map(
        0   -> Map('a' -> 1, 'b'   -> 1),
        1   -> Map('a' -> _ob, 'b' -> _ob),
        _ob -> Map('a' -> _ob, 'b' -> _ob)
      )
    )

    val fsmA = createFsmA
    val fsmB = createFsmB

    assert((fsmA ^ fsmA).strings.isEmpty)
    assert((fsmB ^ fsmB).strings.isEmpty)
    assert((fsmA ^ fsmB).strings.toList == "a" :: "b" :: Nil)
    assert((fsmAorB ^ fsmA).strings.toList == "b" :: Nil)
    assert((fsmAorB ^ fsmB).strings.toList == "a" :: Nil)
  }

  "String generator" should "generate all strings" in {
    val fsmBr = createBrzozowski
    //fsmBr now (a|b)*a(a|b)
    val check = "aa" :: "ab" :: "aaa" :: "aab" :: "baa" :: "bab" :: "aaaa" :: Nil
    assert(fsmBr.strings.take(7).toList == check)
  }

  /**
    * FSM accepts no strings but has 3 states, needs only 1
    */
  "Reduce" should "eliminate unused states" in {
    val fsm3 = Fsm(
      Set('a'),
      Set(0, 1, 2),
      0,
      Set(1),
      Map(
        0 -> Map('a' -> 2),
        1 -> Map('a' -> 2),
        2 -> Map('a' -> 2)
      )
    )

    val fsm1 = fsm3.reduced
    assert(fsm1.states.size == 1)
  }

  /**
    * This is "0*1" in heavy disguise. crawl should resolve this duplication
    * Notice how states 2 and 3 behave identically. When resolved together,
    * states 1 and 2&3 also behave identically, so they, too should be resolved
    * (this is impossible to spot before 2 and 3 have been combined).
    * Finally, the oblivion state should be omitted.
    */
  "Crawl reduction" should "resolve duplication" in {
    val mergedFsm = Fsm(
      Set('a', 'b'),
      Set(1, 2, 3, 4, _ob),
      1,
      Set(4),
      Map(
        1   -> Map('a' -> 2, 'b'   -> 4),
        2   -> Map('a' -> 3, 'b'   -> 4),
        3   -> Map('a' -> 3, 'b'   -> 4),
        4   -> Map('a' -> _ob, 'b' -> _ob),
        _ob -> Map('a' -> _ob, 'b' -> _ob)
      )
    )

    val reducedFsm = mergedFsm.reduced

    assert(reducedFsm.states.size == 2)
  }

  "Equivalent" should "pass basic check" in {
    val fsmA = createFsmA
    val fsmB = createFsmB

    val fsmAB = fsmA | fsmB
    val fsmBA = fsmB | fsmA

    assert(fsmAB.equivalent(fsmBA))
    //ensure proper == overload
    assert(fsmAB == fsmBA)
  }

  /**
    * Binary numbers divisible by 3.
    * Disallows the empty string
    * Allows "0" on its own, but not leading zeroes.
    */
  "Binary Div 3" should "work" in {
    val _init = -2
    val _zero = -1
    //states 0 1 and 2 are for division results
    val fsm3 = Fsm(
      Set('0', '1'),
      Set(_init, _zero, 0, 1, 2, _ob),
      _init,
      Set(_zero, 0),
      Map(
        _init -> Map('0' -> _zero, '1' -> 1),
        _zero -> Map('0' -> _ob, '1'   -> _ob),
        0     -> Map('0' -> 0, '1'     -> 1),
        1     -> Map('0' -> 2, '1'     -> 0),
        2     -> Map('0' -> 1, '1'     -> 2),
        _ob   -> Map('0' -> _ob, '1'   -> _ob)
      )
    )

    assert(!fsm3.accepts(""))
    assert(fsm3.accepts("0"))
    assert(!fsm3.accepts("1"))
    assert(!fsm3.accepts("00"))
    assert(!fsm3.accepts("01"))
    assert(!fsm3.accepts("10"))

    assert(fsm3.accepts("11"))

    assert(!fsm3.accepts("000"))
    assert(!fsm3.accepts("001"))
    assert(!fsm3.accepts("010"))
    //3, but we don't allow leading zeroes
    assert(!fsm3.accepts("011"))
    assert(!fsm3.accepts("100"))
    assert(!fsm3.accepts("101"))

    assert(fsm3.accepts("110"))

    assert(!fsm3.accepts("111"))

    assert(!fsm3.accepts("0000"))
    assert(!fsm3.accepts("0001"))
    assert(!fsm3.accepts("0010"))
    assert(!fsm3.accepts("0011"))
    assert(!fsm3.accepts("0100"))
    assert(!fsm3.accepts("0101"))
    assert(!fsm3.accepts("0110"))
    assert(!fsm3.accepts("0111"))
    assert(!fsm3.accepts("1000"))

    assert(fsm3.accepts("1001"))
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
    assert(abc.star.states.size == 3)
    assert((abc * 3).states.size == 10)
    assert((abc | abc).states.size == 4)
    assert((abc & abc).states.size == 4)
    assert((abc ^ abc).states.size == 1)
  }

  /**
    * You may now omit a transition, or even an entire state, from the map.
    * This affects every usage of `fsm.transitions`.
    */
  "Dead states" should "be allowed" in {
    val fsm = Fsm(
      Set('/', '*', Fsm.anythingElse),
      Set(0, 1, 2, 3, 4, 5),
      0,
      Set(4),
      Map(
        0 -> Map('/' -> 1),
        1 -> Map('*' -> 2),
        2 -> Map('/' -> 2, Fsm.anythingElse -> 2, '*' -> 3),
        3 -> Map('/' -> 4, Fsm.anythingElse -> 2, '*' -> 3)
      )
    )

    assert(fsm.strings.take(1).toList == "/**/" :: Nil)

    assert(fsm.isLive(3))
    assert(fsm.isLive(4))
    assert(!fsm.isLive(5))

    assert(fsm.accepts("/* whatever */"))
    assert(!fsm.accepts("** whatever */"))

    val but = fsm.everythingBut
    assert(!but.accepts("/* whatever */"))
    //deliberately seek oblivion
    assert(but.accepts("*"))
  }

  "Fsm's" should "be have properties similar to List[String]" in {
    val fsmA = createFsmA
    val fsmB = createFsmB

    assert(fsmA.length.contains(1))
    assert(((fsmA | fsmB) * 4).length.contains(16))
    assert(fsmA.star.length.isEmpty)

    assert(Fsm.union(fsmA, fsmB) == fsmA.union(fsmB))
    assert(Fsm.union().length.contains(0))
    assert(Fsm.intersection(fsmA, fsmB) == fsmA.intersection(fsmB))
  }

  /**
    * In general, `a & b & c` is equivalent to
    * `EVERYTHING & a & b & c` where `EVERYTHING` is an FSM accepting every
    * possible string. Similarly `a` is equivalent to `EVERYTHING & a`, and the
    * intersection of no sets at all is... `EVERYTHING`.
    * However, since we compute the union of alphabets, and there are no
    * alphabets, the union is the empty set. So the only string which `EVERYTHING`
    * actually recognises is the empty string, [] (or "" if you prefer).
    */
  "Logical operations" should "behave as expected" in {
    val fsmNone = Fsm.intersection()
    assert(fsmNone.length.contains(1))
    assert(fsmNone.strings.toList == "" :: Nil)

    val fsmA = createFsmA
    val fsmB = createFsmB

    assert((fsmA | fsmB).difference(fsmA) == fsmB)
    assert((fsmA | fsmB) - fsmA - fsmB == Fsm.nullFsm(Set('a', 'b')))
    assert(fsmA.isDisjoint(fsmB))
    assert(fsmA <= (fsmA | fsmB))
    assert(fsmA < (fsmA | fsmB))
    assert(fsmA != (fsmA | fsmB))
    assert((fsmA | fsmB) > fsmA)
    assert((fsmA | fsmB) >= fsmB)
  }

  "Invalid Fsm" should "fail if alphabet null" in {
    assertThrows[IllegalArgumentException] {
      ignore { Fsm(null, null, 0, null, null) }
    }
  }

  "Invalid Fsm" should "fail if initial state is not a state" in {
    assertThrows[IllegalArgumentException] {
      ignore { Fsm(Set('a'), Set(0), 1, Set(0), Map()) }
    }
  }

  "Invalid Fsm" should "fail if final state is not a state" in {
    assertThrows[IllegalArgumentException] {
      ignore { Fsm(Set('a'), Set(1), 1, Set(2), Map()) }
    }
  }

  "Invalid Fsm" should "fail if unexpected transition state detected" in {
    assertThrows[IllegalArgumentException] {
      ignore { Fsm(Set('a'), Set(1, 2), 1, Set(2), Map(1 -> Map('a' -> 3))) }
    }
  }
}
