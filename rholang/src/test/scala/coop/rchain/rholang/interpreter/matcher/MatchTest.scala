package coop.rchain.rholang.interpreter.matcher

import cats.{Eval => _}
import com.google.protobuf.ByteString
import coop.rchain.models.Channel.ChannelInstance._
import coop.rchain.models.Connective.ConnectiveInstance._
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance._
import coop.rchain.models.Var.WildcardMsg
import coop.rchain.models._
import coop.rchain.models.rholang.sort.Sortable
import coop.rchain.rholang.interpreter.PrettyPrinter
import coop.rchain.rholang.interpreter.matcher.OptionalFreeMapWithCost.toOptionalFreeMapWithCostOps
import org.scalatest._
import scalapb.GeneratedMessage

import scala.collection.immutable.BitSet

class VarMatcherSpec extends FlatSpec with Matchers {
  import SpatialMatcher._
  import coop.rchain.models.rholang.implicits._

  private val printer = PrettyPrinter()

  def assertSpatialMatch[T <: GeneratedMessage, P <: GeneratedMessage](
      target: T,
      pattern: P,
      expectedCaptures: Option[FreeMap])(implicit sm: SpatialMatcher[T, P],
                                         ts: Sortable[T],
                                         ps: Sortable[P]): Assertion = {
    println(explainMatch(target, pattern, expectedCaptures))
    assertSorted(target, "target")
    assertSorted(pattern, "pattern")
    expectedCaptures.foreach(
      _.values.foreach((v: Par) => assertSorted(v, "expected captured term")))
    val result: Option[FreeMap] = spatialMatch(target, pattern).runWithCost._2.map(_._1)
    assert(prettyCaptures(result) == prettyCaptures(expectedCaptures))
    assert(result == expectedCaptures)
  }

  private def explainMatch[P <: GeneratedMessage, T <: GeneratedMessage](
      target: T,
      pattern: P,
      expectedCaptures: Option[FreeMap]): String = {
    val targetString       = printer.buildString(target)
    val patternString      = printer.buildString(pattern)
    val capturesStringsMap = prettyCaptures(expectedCaptures)

    s"""
       |     Matching:  $patternString
       |           to:  $targetString
       | should yield:  $capturesStringsMap
       |""".stripMargin
  }

  private def prettyCaptures[T <: GeneratedMessage, P <: GeneratedMessage](
      expectedCaptures: Option[FreeMap]): Option[Map[Int, String]] = {
    expectedCaptures.map(_.map(c => (c._1, printer.buildString(c._2))))
  }

  private def assertSorted[T <: GeneratedMessage](term: T, termName: String)(
      implicit ts: Sortable[T]): Assertion = {
    val sortedTerm = Sortable[T].sortMatch(term).term
    val clue       = s"Invalid test case - ${termName} is not sorted"
    assert(printer.buildString(term) == printer.buildString(sortedTerm), clue)
    assert(term == sortedTerm, clue)
  }

  val wc = Wildcard(Var.WildcardMsg())
  "Matching ground with var" should "work" in {
    val target: Par  = GInt(7)
    val pattern: Par = EVar(FreeVar(0))
    assertSpatialMatch(target, pattern, Some(Map[Int, Par](0 -> GInt(7))))
  }
  "Matching var with var" should "fail" in {
    val target: Par  = EVar(BoundVar(0))
    val pattern: Par = EVar(FreeVar(0))
    assertSpatialMatch(target, pattern, None)
  }
  "Matching lists of grounds with lists of vars" should "work" in {
    val target: Par = EList(List[Par](GString("add"), GInt(7), GInt(8)), BitSet())
    val pattern: Par =
      EList(List[Par](GString("add"), EVar(FreeVar(0)), EVar(FreeVar(1))), BitSet())
        .withConnectiveUsed(true)
    assertSpatialMatch(target, pattern, Some(Map[Int, Par](0 -> GInt(7), 1 -> GInt(8))))
  }

  "Matching 2 lists in parallel" should "work" in {
    // The second pattern will be checked first because of prepend.
    // It matches both targets, but the first pattern only matches one of the lists.
    val target: Par = EList(List(GInt(7), GInt(9)), BitSet())
      .prepend(EList(List(GInt(7), GInt(8)), BitSet()), 0)
    val pattern: Par = EList(List(EVar(FreeVar(0)).withConnectiveUsed(true), GInt(9)), BitSet())
      .withConnectiveUsed(true)
      .prepend(EList(List(GInt(7), EVar(FreeVar(1)).withConnectiveUsed(true)), BitSet())
        .withConnectiveUsed(true), depth = 1)
    assertSpatialMatch(target, pattern, Some(Map[Int, Par](0 -> GInt(7), 1 -> GInt(8))))
  }

  "Matching a send's channel" should "work" in {
    val target: Send =
      Send(Quote(GPrivateBuilder("unforgeable")), List(GInt(7), GInt(8)), false, BitSet())
    val pattern: Send =
      Send(ChanVar(FreeVar(0)), List(EVar(wc), GInt(8)), false, BitSet(), true)
    val expectedResult = Some(Map[Int, Par](0 -> GPrivateBuilder("unforgeable")))
    assertSpatialMatch(target, pattern, expectedResult)
    val targetPar: Par  = target
    val patternPar: Par = pattern
    assertSpatialMatch(targetPar, patternPar, expectedResult)
  }

  "Matching a send's body" should "work" in {
    val target: Send =
      Send(Quote(GPrivateBuilder("unforgeable")), List(GInt(7), GInt(8)), false, BitSet())
    val pattern: Send  = Send(ChanVar(wc), List(EVar(FreeVar(0)), GInt(8)), false, BitSet(), true)
    val expectedResult = Some(Map[Int, Par](0 -> GInt(7)))
    assertSpatialMatch(target, pattern, expectedResult)
    val targetPar: Par  = target
    val patternPar: Par = pattern
    assertSpatialMatch(targetPar, patternPar, expectedResult)
  }
  "Matching a send" should "require arity matching in" in {
    val target: Send =
      Send(Quote(GPrivateBuilder("unforgeable")), List(GInt(7), GInt(8), GInt(9)), false, BitSet())
    val pattern: Send = Send(ChanVar(wc), List(EVar(FreeVar(0)), EVar(wc)), false, BitSet(), true)
    assertSpatialMatch(target, pattern, None)
  }

  "Matching extras with free variable" should "work" in {
    val target: Par  = GInt(9).prepend(GInt(8), depth = 0).prepend(GInt(7), depth = 0)
    val pattern: Par = EVar(FreeVar(0)).prepend(GInt(8), depth = 1)
    assertSpatialMatch(target, pattern, Some(Map[Int, Par](0 -> GInt(9).prepend(GInt(7), depth = 0))))
  }
  "Matching extras with wildcard" should "work" in {
    val target: Par  = GInt(9).prepend(GInt(8), depth = 0).prepend(GInt(7), depth = 0)
    val pattern: Par = EVar(Wildcard(Var.WildcardMsg())).prepend(GInt(8), depth = 1)
    assertSpatialMatch(target, pattern, Some(Map.empty[Int, Par]))
  }
  "Matching extras with wildcard and free variable" should "capture in the free variable" in {
    val target: Par  = GInt(9).prepend(GInt(8), depth = 0).prepend(GInt(7), depth = 0)
    val pattern: Par =
      EVar(Wildcard(Var.WildcardMsg())).prepend(EVar(FreeVar(0)), depth = 1).prepend(GInt(8), depth = 1)
    assertSpatialMatch(target, pattern, Some(Map[Int, Par](0 -> GInt(9).prepend(GInt(7), depth = 0))))
  }
  "Matching send with free variable in channel and variable position" should "capture both values" in {
    val sendTarget: Par =
      Send(Quote(GPrivateBuilder("zero")), List(GInt(7), GPrivateBuilder("one")), false, BitSet())
    val pattern: Par =
      Send(ChanVar(FreeVar(0)), List(GInt(7), EVar(FreeVar(1))), false, BitSet(), true)
        .withConnectiveUsed(true)
    val expectedResult = Some(Map[Int, Par](0 -> GPrivateBuilder("zero"), 1 -> GPrivateBuilder("one")))
    assertSpatialMatch(sendTarget, pattern, expectedResult)
    val targetPar: Par  = sendTarget
    val patternPar: Par = pattern
    assertSpatialMatch(targetPar, patternPar, expectedResult)
  }

  "Matching a receive with a free variable in the channel and a free variable in the body" should "capture for both variables." in {
    val target: Receive = Receive(
      List(
        ReceiveBind(List(Quote(EVar(FreeVar(0))), Quote(EVar(FreeVar(1)))), Quote(GInt(7))),
        ReceiveBind(List(Quote(EVar(FreeVar(0))), Quote(EVar(FreeVar(1)))), Quote(GInt(8)))
      ),
      Send(Quote(GPrivateBuilder("unforgeable")), List(GInt(9), GInt(10)), false, BitSet()),
      false,
      4
    )
    val pattern: Receive = Receive(
      List(
        ReceiveBind(List(Quote(EVar(FreeVar(0))), Quote(EVar(FreeVar(1)))), Quote(GInt(7))),
        ReceiveBind(List(Quote(EVar(FreeVar(0))), Quote(EVar(FreeVar(1)))), ChanVar(FreeVar(0)))
      ),
      EVar(FreeVar(1)),
      false,
      4,
      connectiveUsed = true
    )
    val expectedResult =
      Some(
        Map[Int, Par](
          0 -> GInt(8),
          1 -> Send(Quote(GPrivateBuilder("unforgeable")), List(GInt(9), GInt(10)), false, BitSet())))
    assertSpatialMatch(target, pattern, expectedResult)
    val targetPar: Par  = target
    val patternPar: Par = pattern
    assertSpatialMatch(targetPar, patternPar, expectedResult)
  }

  "Matching an eval with no free variables" should "Succeed, but not capture anything." in {
    val target: Expr   = EEvalBody(ChanVar(BoundVar(0)))
    val pattern: Expr  = EEvalBody(ChanVar(BoundVar(0)))
    val expectedResult = Some(Map.empty[Int, Par])
    assertSpatialMatch(target, pattern, expectedResult)
    val targetPar: Par  = target
    val patternPar: Par = pattern
    assertSpatialMatch(targetPar, patternPar, expectedResult)
  }

  "Matching between New's" should "match the bodies if the new count is the same" in {
    val target: New =
      New(2,
          Par()
            .prepend(Send(ChanVar(BoundVar(0)), Seq(GInt(43)), false, locallyFree = BitSet(0)))
            .prepend(Send(Quote(GInt(7)), Seq(GInt(42)), false)))
    val pattern: New =
      New(2,
          Par()
            .prepend(Send(Quote(GInt(7)), Seq(EVar(FreeVar(0))), false).withConnectiveUsed(true))
            .prepend(EVar(Wildcard(WildcardMsg())), 1))

    val expectedResult = Some(Map[Int, Par](0 -> GInt(42)))
    assertSpatialMatch(target, pattern, expectedResult)
    val targetPar: Par  = target
    val patternPar: Par = pattern
    assertSpatialMatch(targetPar, patternPar, expectedResult)
  }

  "Matching between matches " should "require equality of cases, but match targets and inside bodies." in {
    val target: Match =
      Match(
        EList(Seq(GInt(4), GInt(20))),
        Seq(
          MatchCase(EList(Seq(EVar(FreeVar(0)), EVar(FreeVar(1)))),
                    Send(Quote(EVar(BoundVar(1))),
                         Seq[Par](EVar(BoundVar(0))),
                         false,
                         locallyFree = BitSet(0, 1))),
          MatchCase(EVar(Wildcard(WildcardMsg())), Par())
        )
      )
    val pattern: Match =
      Match(
        EVar(FreeVar(0)),
        Seq(MatchCase(EList(Seq(EVar(FreeVar(0)), EVar(FreeVar(1)))),
                      EVar(Wildcard(WildcardMsg()))),
            MatchCase(EVar(Wildcard(WildcardMsg())), EVar(FreeVar(1)))),
        connectiveUsed = true
      )

    val expectedResult = (Some(Map[Int, Par](0 -> EList(Seq(GInt(4), GInt(20))), 1 -> Par())))
    assertSpatialMatch(target, pattern, expectedResult)

    val targetPar: Par  = target
    val patternPar: Par = pattern
    assertSpatialMatch(targetPar, patternPar, expectedResult)
  }

  "Matching a list with a remainder" should "capture the remainder." in {
    val target: Expr   = EList(Seq(GInt(1), GInt(2)))
    val pattern: Expr  = EList(Seq(GInt(1)), remainder = Some(FreeVar(0)), connectiveUsed = true)
    val expectedResult = (Some(Map[Int, Par](0 -> EList(Seq(GInt(2))))))
    assertSpatialMatch(target, pattern, expectedResult)

    val targetPar: Par  = target
    val patternPar: Par = pattern
    assertSpatialMatch(targetPar, patternPar, expectedResult)
  }

  "Matching a whole list with a remainder" should "capture the list." in {
    // for (@[…a] <- @0) { … } | @0!([1,2,3])
    val target: Expr   = EList(Seq(GInt(1), GInt(2), GInt(3)))
    val pattern: Expr  = EList(remainder = Some(FreeVar(0)), connectiveUsed = true)
    val expectedResult = Some(Map[Int, Par](0 -> target))
    assertSpatialMatch(target, pattern, expectedResult)
    val targetPar: Par  = target
    val patternPar: Par = pattern
    assertSpatialMatch(targetPar, patternPar, expectedResult)
  }

  "Matching inside bundles" should "not be possible" in {
    val target: Bundle = Bundle(
      Par()
        .prepend(Send(Quote(GPrivateBuilder("0")), Seq(GInt(43)), persistent = false))
        .prepend(Send(Quote(GInt(7)), Seq(GInt(42)), persistent = false)))
    val pattern: Bundle = Bundle(
      Par()
        .prepend(Send(Quote(GInt(7)), Seq(EVar(FreeVar(0))), persistent = false))
        .prepend(EVar(Wildcard(WildcardMsg())), 1))

    assertSpatialMatch(target, pattern, None)
  }

  /** Example:
    *
    * bundle { @7!(42) | @"0"!(43) } match {
    *   w => …
    * }
    *
    * here we match on the entire bundle, not its components.
    */
  it should "be possible to match on entire bundle" in {
    val target: Bundle = Bundle(
      Par()
        .prepend(Send(Quote(GPrivateBuilder("0")), Seq(GInt(43)), persistent = false))
        .prepend(Send(Quote(GInt(7)), Seq(GInt(42)), persistent = false)))

    val pattern: Par = EVar(FreeVar(0))

    val expectedResult = Some(Map[Int, Par](0 -> target))
    assertSpatialMatch(Par(bundles = Seq(target)), pattern, expectedResult)
    val targetPar: Par  = target
    val patternPar: Par = pattern
    assertSpatialMatch(targetPar, patternPar, expectedResult)
  }

  "Matching a single and" should "match both sides" in {
    // @7!(8)
    val target: Par     = Send(Quote(GInt(7)), Seq(GInt(8)), persistent = false)
    val failTarget: Par = Send(Quote(GInt(7)), Seq(GInt(9)), persistent = false)
    // @7!(x) /\ @y!(8)
    val pattern: Connective = Connective(
      ConnAndBody(ConnectiveBody(Seq(
        Send(Quote(GInt(7)), Seq(EVar(FreeVar(0))), persistent = false, connectiveUsed = true),
        Send(ChanVar(FreeVar(1)), Seq(GInt(8)), persistent = false, connectiveUsed = true)
      ))))

    val expectedResult = Some(Map[Int, Par](0 -> GInt(8), 1 -> GInt(7)))
    assertSpatialMatch(target, pattern, expectedResult)
    val patternPar: Par = pattern
    assertSpatialMatch(target, patternPar, expectedResult)

    assertSpatialMatch(failTarget, pattern, None)
    assertSpatialMatch(failTarget, patternPar, None)
  }

  "Matching a single or" should "match some side" in {
    // @7!(8)
    val target: Par     = Send(Quote(GInt(7)), Seq(GInt(8)), persistent = false)
    val failTarget: Par = Send(Quote(GInt(7)), Seq(GInt(9)), persistent = false)
    // @9!(x) \/ @x!(8)
    val pattern: Connective = Connective(
      ConnOrBody(ConnectiveBody(Seq(
        Send(Quote(GInt(9)), Seq(EVar(FreeVar(0))), persistent = false, connectiveUsed = true),
        Send(ChanVar(FreeVar(0)), Seq(GInt(8)), persistent = false, connectiveUsed = true)
      ))))

    val expectedResult = Some(Map.empty[Int, Par])
    assertSpatialMatch(target, pattern, expectedResult)
    val patternPar: Par = pattern
    assertSpatialMatch(target, patternPar, expectedResult)

    assertSpatialMatch(failTarget, pattern, None)
    assertSpatialMatch(failTarget, patternPar, None)
  }

  "Matching negation" should "work" in {
    // @1!(2) | @2!(3) | @3!(4)
    val target: Par = Par().addSends(
      Send(Quote(GInt(1)), Seq(GInt(2)), persistent = false),
      Send(Quote(GInt(2)), Seq(GInt(3)), persistent = false),
      Send(Quote(GInt(3)), Seq(GInt(4)), persistent = false)
    )
    // ~Nil
    val pattern: Connective = Connective(ConnNotBody(Par()))

    val expectedResult = Some(Map.empty[Int, Par])
    assertSpatialMatch(target, pattern, expectedResult)

    val patternPar: Par = pattern
    assertSpatialMatch(target, patternPar, expectedResult)

    // ~Nil | ~Nil
    val doublePatternPar = patternPar.addConnectives(pattern)
    assertSpatialMatch(target, doublePatternPar, expectedResult)

    // ~Nil | ~Nil | ~Nil
    val triplePatternPar = doublePatternPar.addConnectives(pattern)
    assertSpatialMatch(target, triplePatternPar, expectedResult)

    // ~Nil | ~Nil | ~Nil | ~Nil
    // Fails because there is no way to split 3 sends into 4 non nil terms.
    val quadruplePatternPar = triplePatternPar.addConnectives(pattern)
    assertSpatialMatch(target, quadruplePatternPar, None)
  }

  "Matching a complicated connective" should "work" in {
    // @1!(6) | @2!(7) | @3!(8)
    val target: Par = Par().addSends(
      Send(Quote(GInt(1)), Seq(GInt(6)), persistent = false),
      Send(Quote(GInt(2)), Seq(GInt(7)), persistent = false),
      Send(Quote(GInt(3)), Seq(GInt(8)), persistent = false)
    )

    val failTarget: Par = Par().addSends(
      Send(Quote(GInt(1)), Seq(GInt(6)), persistent = false),
      Send(Quote(GInt(2)), Seq(GInt(9)), persistent = false),
      Send(Quote(GInt(3)), Seq(GInt(8)), persistent = false)
    )

    // ~Nil
    val nonNullConn: Connective = Connective(ConnNotBody(Par()))
    val nonNull: Par            = nonNullConn

    // ~{ ~Nil | ~Nil }
    val singleFactor: Connective = Connective(
      ConnNotBody(
        Par()
          .addConnectives(nonNullConn, nonNullConn)
          .withConnectiveUsed(true)))
    // x /\ y!(7)
    val capture: Connective =
      Connective(
        ConnAndBody(ConnectiveBody(
          Seq(EVar(FreeVar(0)), Send(ChanVar(FreeVar(1)), Seq(GInt(7))).withConnectiveUsed(true)))))
    // ~{ ~Nil | ~Nil } & ~Nil
    val prime: Connective = Connective(
      ConnAndBody(
        ConnectiveBody(Seq(nonNull, Par().addConnectives(singleFactor).withConnectiveUsed(true)))))
    // x!(7) \/ x!(8)
    val alternative: Connective = Connective(
      ConnOrBody(
        ConnectiveBody(Seq(Send(ChanVar(FreeVar(0)), Seq(GInt(7))).withConnectiveUsed(true),
                           Send(ChanVar(FreeVar(0)), Seq(GInt(8))).withConnectiveUsed(true)))))
    // x /\ y!(7) | ~{ ~Nil | ~Nil } & ~Nil | x!(7) \/ x!(8)
    val pattern: Par   = Par().addConnectives(capture, prime, alternative).withConnectiveUsed(true)
    val expectedResult = Some(Map[Int, Par](0 -> Send(Quote(GInt(2)), Seq(GInt(7))), 1 -> GInt(2)))
    assertSpatialMatch(target, pattern, expectedResult)

    assertSpatialMatch(failTarget, pattern, None)
  }

  "Matching a target with var ref and a pattern with a var ref" should "ignore locallyFree" in {
    val target: Par = New(
      bindCount = 1,
      p = Par(
        receives = List(Receive(
          binds = List(
            ReceiveBind(
              patterns = List(Quote(
                Connective(VarRefBody(VarRef(0, 1))).withLocallyFree(BitSet(0)))),
              source = Quote(Par()))),
          body = Par(),
          persistent = false,
          bindCount = 0,
          locallyFree = BitSet(0))),
        locallyFree = BitSet(0)))
    val pattern: Par = New(
      bindCount = 1,
      p = Par(
        receives = List(Receive(
          binds = List(
            ReceiveBind(
              patterns = List(Quote(
                Connective(VarRefBody(VarRef(0, 1))))),
              source = Quote(Par()))),
          body = Par(),
          persistent = false,
          bindCount = 0))))
    assertSpatialMatch(target, pattern, Some(Map.empty[Int, Par]))
  }

  "Matching ++" should "work" in {
    // "abc" ++ "def"
    val target = Expr(EPlusPlusBody(EPlusPlus(GString("abc"), GString("def"))))
    // x ++ y
    val pattern        = Expr(EPlusPlusBody(EPlusPlus(EVar(FreeVar(0)), EVar(FreeVar(1)))))
    val expectedResult = Some(Map[Int, Par](0 -> GString("abc"), 1 -> GString("def")))
    assertSpatialMatch(target, pattern, expectedResult)
  }

  "Matching %%" should "work" in {
    val map =
      EMapBody(ParMap(List[(Par, Par)]((GString("name"), GString("a"))), false, BitSet()))
    // "${name}" %% {"name" : "a"}
    val target = Expr(EPercentPercentBody(EPercentPercent(GString("${name}"), map)))
    // x %% y
    val pattern        = Expr(EPercentPercentBody(EPercentPercent(EVar(FreeVar(0)), EVar(FreeVar(1)))))
    val expectedResult = Some(Map[Int, Par](0 -> GString("${name}"), 1 -> map))
    assertSpatialMatch(target, pattern, expectedResult)
  }

  "Matching --" should "work" in {
    val lhsSet = ESetBody(ParSet(List[Par](GInt(1), GInt(2), GInt(3))))
    val rhsSet = ESetBody(ParSet(List[Par](GInt(1), GInt(2))))
    // "${1, 2, 3}" -- {1, 2}
    val target = Expr(EMinusMinusBody(EMinusMinus(lhsSet, rhsSet)))
    // x -- y
    val pattern        = Expr(EMinusMinusBody(EMinusMinus(EVar(FreeVar(0)), EVar(FreeVar(1)))))
    val expectedResult = Some(Map[Int, Par](0 -> lhsSet, 1 -> rhsSet))
    assertSpatialMatch(target, pattern, expectedResult)
  }

  "Matching Bool" should "work" in {
    val successTarget: Par = GBool(false)
    val failTarget: Par = GString("Fail")
    val pattern: Connective = Connective(ConnBool(true))

    assertSpatialMatch(successTarget, pattern, Some(Map.empty))
    assertSpatialMatch(failTarget, pattern, None)
  }

  "Matching Int" should "work" in {
    val successTarget: Par = GInt(7)
    val failTarget: Par = GString("Fail")
    val pattern: Connective = Connective(ConnInt(true))

    assertSpatialMatch(successTarget, pattern, Some(Map.empty))
    assertSpatialMatch(failTarget, pattern, None)
  }

  "Matching String" should "work" in {
    val successTarget: Par = GString("Match me!")
    val failTarget: Par = GInt(42)
    val pattern: Connective = Connective(ConnString(true))

    assertSpatialMatch(successTarget, pattern, Some(Map.empty))
    assertSpatialMatch(failTarget, pattern, None)
  }

  "Matching Uri" should "work" in {
    val successTarget: Par = GUri("rho:io:stdout")
    val failTarget: Par = GString("Fail")
    val pattern: Connective = Connective(ConnUri(true))

    assertSpatialMatch(successTarget, pattern, Some(Map.empty))
    assertSpatialMatch(failTarget, pattern, None)
  }

  "Matching ByteArray" should "work" in {
    val successTarget: Par = GByteArray(ByteString.copyFrom(Array[Byte](74, 75)))
    val failTarget: Par = GString("Fail")
    val pattern: Connective = Connective(ConnByteArray(true))

    assertSpatialMatch(successTarget, pattern, Some(Map.empty))
    assertSpatialMatch(failTarget, pattern, None)
  }
}
