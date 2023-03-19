package coop.rchain.rholang.interpreter.matcher

import cats.effect._
import cats.mtl.implicits._
import cats.{Eval => _}
import com.google.protobuf.ByteString
import coop.rchain.catscontrib.MonadError_._
import coop.rchain.models.Connective.ConnectiveInstance._
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance._
import coop.rchain.models.Var.WildcardMsg
import coop.rchain.models._
import coop.rchain.models.rholang.sorter.Sortable
import coop.rchain.rholang.interpreter._
import monix.eval.{Eval, Task}
import monix.execution.Scheduler.Implicits.global
import org.scalactic.TripleEqualsSupport
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.concurrent.TimeLimits

import scala.collection.immutable.BitSet
import scala.concurrent.duration._

class VarMatcherSpec extends AnyFlatSpec with Matchers with TimeLimits with TripleEqualsSupport {
  import SpatialMatcher._
  import coop.rchain.models.rholang.implicits._

  private val printer = PrettyPrinter()

  type F[A] = MatcherMonadT[Task, A]

  def assertSpatialMatch(
      target: Par,
      pattern: Par,
      expectedCaptures: Option[FreeMap]
  ): Assertion = {

    println(explainMatch(target, pattern, expectedCaptures))
    assertSorted(target, "target")
    assertSorted(pattern, "pattern")
    expectedCaptures.foreach(
      _.values.foreach((v: Par) => assertSorted(v, "expected captured term"))
    )

    implicit val matcherMonadError = implicitly[Sync[F]]
    (for {
      maybeResultWithCost <- runFirst(spatialMatch[F, Par, Par](target, pattern))
      result              = maybeResultWithCost.map(_._1)
      _                   = assert(prettyCaptures(result) == prettyCaptures(expectedCaptures))
    } yield (assert(result === expectedCaptures))).runSyncUnsafe(5.seconds)
  }

  private def explainMatch(
      target: Par,
      pattern: Par,
      expectedCaptures: Option[FreeMap]
  ): String = {

    def truncate(s: String): String = if (s.length > 120) s.substring(0, 120) + "..." else s

    val targetString       = truncate(printer.buildString(target))
    val patternString      = truncate(printer.buildString(pattern))
    val capturesStringsMap = truncate(prettyCaptures(expectedCaptures).toString)

    s"""
       |     Matching:  $patternString
       |           to:  $targetString
       | should yield:  $capturesStringsMap
       |""".stripMargin
  }

  private def prettyCaptures(
      expectedCaptures: Option[FreeMap]
  ): Option[Map[Int, String]] =
    expectedCaptures.map(_.map(c => (c._1, printer.buildString(c._2))))

  private def assertSorted(term: Par, termName: String): Assertion = {
    val sortedTerm = Sortable[Par].sortMatch[Eval](term).value.term
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
  "Matching bound var with free var" should "fail" in {
    val target: Par  = EVar(BoundVar(0))
    val pattern: Par = EVar(FreeVar(0))
    assertSpatialMatch(target, pattern, None)
  }
  "Matching bound var with a wildcard" should "succeed" in {
    val target: Par  = EVar(BoundVar(0))
    val pattern: Par = EVar(Wildcard(WildcardMsg()))
    assertSpatialMatch(target, pattern, Some(Map()))
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
      .prepend(
        EList(List(GInt(7), EVar(FreeVar(1)).withConnectiveUsed(true)), BitSet())
          .withConnectiveUsed(true),
        depth = 1
      )
    assertSpatialMatch(target, pattern, Some(Map[Int, Par](0 -> GInt(7), 1 -> GInt(8))))
  }

  "Matching huge list of targets with no patterns and a reminder" should "better be quick" in {
    //This is a very common case in rspace that can be handled in linear time, yet was quadratic for a short while
    val target: Par  = Par(exprs = Seq.fill(1000)(GInt(1): Expr))
    val pattern: Par = EVar(FreeVar(0))
    assertSpatialMatch(target, pattern, Some(Map[Int, Par](0 -> target)))
  }

  "Matching a send's channel" should "work" in {
    val target: Send =
      Send(GPrivateBuilder("unforgeable"), List(GInt(7), GInt(8)), false, BitSet())
    val pattern: Send =
      Send(EVar(FreeVar(0)), List(EVar(wc), GInt(8)), false, BitSet(), true)
    val expectedResult = Some(Map[Int, Par](0 -> GPrivateBuilder("unforgeable")))
    assertSpatialMatch(target, pattern, expectedResult)
    val targetPar: Par  = target
    val patternPar: Par = pattern
    assertSpatialMatch(targetPar, patternPar, expectedResult)
  }

  "Matching a send's body" should "work" in {
    val target: Send =
      Send(GPrivateBuilder("unforgeable"), List(GInt(7), GInt(8)), false, BitSet())
    val pattern: Send  = Send(EVar(wc), List(EVar(FreeVar(0)), GInt(8)), false, BitSet(), true)
    val expectedResult = Some(Map[Int, Par](0 -> GInt(7)))
    assertSpatialMatch(target, pattern, expectedResult)
    val targetPar: Par  = target
    val patternPar: Par = pattern
    assertSpatialMatch(targetPar, patternPar, expectedResult)
  }
  "Matching a send" should "require arity matching in" in {
    val target: Send =
      Send(GPrivateBuilder("unforgeable"), List(GInt(7), GInt(8), GInt(9)), false, BitSet())
    val pattern: Send = Send(EVar(wc), List(EVar(FreeVar(0)), EVar(wc)), false, BitSet(), true)
    assertSpatialMatch(target, pattern, None)
  }

  "Matching extras with free variable" should "work" in {
    val target: Par  = GInt(9).prepend(GInt(8), depth = 0).prepend(GInt(7), depth = 0)
    val pattern: Par = EVar(FreeVar(0)).prepend(GInt(8), depth = 1)
    assertSpatialMatch(
      target,
      pattern,
      Some(Map[Int, Par](0 -> GInt(9).prepend(GInt(7), depth = 0)))
    )
  }

  "Matching a singleton list" should "work" in {
    val target: Expr   = EList(Seq(GInt(1)))
    val pattern: Expr  = EList(Seq(EVar(FreeVar(0))), connectiveUsed = true)
    val expectedResult = Some(Map[Int, Par](0 -> GInt(1)))
    assertSpatialMatch(target, pattern, expectedResult)
    assertSpatialMatch(target: Par, pattern: Par, expectedResult)
  }

  "Matching patterns to equal targets" should "work" in {
    //     Matching:  free0 | free1
    //           to:  1 | 1
    // should yield:  Some(Map(0 -> 1 | 1))
    val target: Par = GInt(1).prepend(GInt(1), depth = 0)
    val pattern: Par =
      EVar(FreeVar(1)).prepend(EVar(FreeVar(0)), depth = 0).withConnectiveUsed(true)
    val expectedResult = Some(Map[Int, Par](0 -> target))
    assertSpatialMatch(target, pattern, expectedResult)
  }

  "Matching multiple reminders" should "assign captures to remainders greedily" in {
    //     Matching:  free0 | free1 | _
    //           to:  1 | 2 | 3
    // should yield:  Some(Map(0 -> 1 | 2 | 3))
    val target: Par = GInt(3).prepend(GInt(2), depth = 0).prepend(GInt(1), depth = 0)
    val pattern: Par = EVar(Wildcard(WildcardMsg()))
      .prepend(EVar(FreeVar(1)), depth = 0)
      .prepend(EVar(FreeVar(0)), depth = 0)
    val expectedResult = Some(Map[Int, Par](0 -> target))
    assertSpatialMatch(target, pattern, expectedResult)
  }

  "Matching that requires revision of prior matches" should "work" in {
    //     Matching:  [1, [free0]] | [free1, [2]]
    //           to:  [1, [2]] | [1, [3]]
    // should yield:  Some(Map(0 -> 3, 1 -> 1))
    val p1 = EList(
      Seq(GInt(1), EList(Seq(EVar(FreeVar(0))), connectiveUsed = true)),
      connectiveUsed = true
    )
    val p2 = EList(
      Seq(EVar(FreeVar(1)), EList(Seq(GInt(2)))),
      connectiveUsed = true
    )
    val t1 = EList(Seq(GInt(1), EList(Seq(GInt(2)))))
    val t2 = EList(Seq(GInt(1), EList(Seq(GInt(3)))))

    val target         = t2.prepend(t1, depth = 0)
    val pattern        = p2.prepend(p1, depth = 0)
    val expectedResult = Some(Map[Int, Par](0 -> GInt(3), 1 -> GInt(1)))
    assertSpatialMatch(target, pattern, expectedResult)
  }

  it should "work with remainders" in {
    //     Matching:  [1, [...free0]] | [free1, [2]]
    //           to:  [1, [2]] | [1, [3]]
    // should yield:  Some(Map(0 -> 3, 1 -> 1))
    val p1 = EList(
      Seq(GInt(1), EList(remainder = FreeVar(0), connectiveUsed = true)),
      connectiveUsed = true
    )
    val p2 = EList(
      Seq(EVar(FreeVar(1)), EList(Seq(GInt(2)))),
      connectiveUsed = true
    )
    val t1 = EList(Seq(GInt(1), EList(Seq(GInt(2)))))
    val t2 = EList(Seq(GInt(1), EList(Seq(GInt(3)))))

    val target         = t2.prepend(t1, depth = 0)
    val pattern        = p2.prepend(p1, depth = 0)
    val expectedResult = Some(Map[Int, Par](0 -> EList(Seq(GInt(3))), 1 -> GInt(1)))
    assertSpatialMatch(target, pattern, expectedResult)
  }

  it should "work despite having other terms in the same list" in {
    //     Matching:  [1, [free0, 2]] | [free1, [2, 2]]
    //           to:  [1, [2, 2]] | [1, [3, 2]]
    // should yield:  Some(Map(0 -> 3, 1 -> 1))
    val p1 = EList(
      Seq(GInt(1), EList(Seq(EVar(FreeVar(0)), GInt(2)), connectiveUsed = true)),
      connectiveUsed = true
    )
    val p2 = EList(
      Seq(EVar(FreeVar(1)), EList(Seq(GInt(2), GInt(2)))),
      connectiveUsed = true
    )
    val t1 = EList(Seq(GInt(1), EList(Seq(GInt(2), GInt(2)))))
    val t2 = EList(Seq(GInt(1), EList(Seq(GInt(3), GInt(2)))))

    val target         = t2.prepend(t1, depth = 0)
    val pattern        = p2.prepend(p1, depth = 0)
    val expectedResult = Some(Map[Int, Par](0 -> GInt(3), 1 -> GInt(1)))
    assertSpatialMatch(target, pattern, expectedResult)
  }

  it should "work with remainders and wildcards" in {
    //     Matching:  [_] | free0
    //           to:  [1] | [x0]
    // should yield:  Some(Map(0 -> [1]))
    val p1 = EList(Seq(EVar(Wildcard(Var.WildcardMsg()))), connectiveUsed = true)
    val p2 = EVar(FreeVar(0))
    val t1 = EList(Seq(GInt(1)))
    val t2 = EList(Seq(EVar(BoundVar(0))), locallyFree = BitSet(0))

    val target         = t2.prepend(t1, depth = 0)
    val pattern        = p2.prepend(p1, depth = 0)
    val expectedResult = Some(Map[Int, Par](0 -> EList(Seq(GInt(1)))))
    assertSpatialMatch(target, pattern, expectedResult)
  }

  "Matching extras with wildcard" should "work" in {
    val target: Par  = GInt(9).prepend(GInt(8), depth = 0).prepend(GInt(7), depth = 0)
    val pattern: Par = EVar(Wildcard(Var.WildcardMsg())).prepend(GInt(8), depth = 1)
    assertSpatialMatch(target, pattern, Some(Map.empty[Int, Par]))
  }
  "Matching extras with wildcard and free variable" should "capture in the free variable" in {
    val target: Par = GInt(9).prepend(GInt(8), depth = 0).prepend(GInt(7), depth = 0)
    val pattern: Par =
      EVar(Wildcard(Var.WildcardMsg()))
        .prepend(EVar(FreeVar(0)), depth = 1)
        .prepend(GInt(8), depth = 1)
    assertSpatialMatch(
      target,
      pattern,
      Some(Map[Int, Par](0 -> GInt(9).prepend(GInt(7), depth = 0)))
    )
  }
  "Matching send with free variable in channel and variable position" should "capture both values" in {
    val sendTarget: Par =
      Send(GPrivateBuilder("zero"), List(GInt(7), GPrivateBuilder("one")), false, BitSet())
    val pattern: Par =
      Send(EVar(FreeVar(0)), List(GInt(7), EVar(FreeVar(1))), false, BitSet(), true)
        .withConnectiveUsed(true)
    val expectedResult =
      Some(Map[Int, Par](0 -> GPrivateBuilder("zero"), 1 -> GPrivateBuilder("one")))
    assertSpatialMatch(sendTarget, pattern, expectedResult)
    val targetPar: Par  = sendTarget
    val patternPar: Par = pattern
    assertSpatialMatch(targetPar, patternPar, expectedResult)
  }

  "Matching a receive with a free variable in the channel and a free variable in the body" should "capture for both variables." in {
    val target: Receive = Receive(
      List(
        ReceiveBind(List(EVar(FreeVar(0)), EVar(FreeVar(1))), GInt(7)),
        ReceiveBind(List(EVar(FreeVar(0)), EVar(FreeVar(1))), GInt(8))
      ),
      Send(GPrivateBuilder("unforgeable"), List(GInt(9), GInt(10)), false, BitSet()),
      false,
      false,
      4
    )
    val pattern: Receive = Receive(
      List(
        ReceiveBind(List(EVar(FreeVar(0)), EVar(FreeVar(1))), GInt(7)),
        ReceiveBind(List(EVar(FreeVar(0)), EVar(FreeVar(1))), EVar(FreeVar(0)))
      ),
      EVar(FreeVar(1)),
      false,
      false,
      4,
      connectiveUsed = true
    )
    val expectedResult =
      Some(
        Map[Int, Par](
          0 -> GInt(8),
          1 -> Send(GPrivateBuilder("unforgeable"), List(GInt(9), GInt(10)), false, BitSet())
        )
      )
    assertSpatialMatch(target, pattern, expectedResult)
    val targetPar: Par  = target
    val patternPar: Par = pattern
    assertSpatialMatch(targetPar, patternPar, expectedResult)
  }

  "Matching an eval with no free variables" should "Succeed, but not capture anything." in {
    val target: Expr   = EVar(BoundVar(0))
    val pattern: Expr  = EVar(BoundVar(0))
    val expectedResult = Some(Map.empty[Int, Par])
    assertSpatialMatch(target, pattern, expectedResult)
    val targetPar: Par  = target
    val patternPar: Par = pattern
    assertSpatialMatch(targetPar, patternPar, expectedResult)
  }

  "Matching between New's" should "match the bodies if the new count is the same" in {
    val target: New =
      New(
        2,
        Par()
          .prepend(Send(EVar(BoundVar(0)), Seq(GInt(43)), false, locallyFree = BitSet(0)))
          .prepend(Send(GInt(7), Seq(GInt(42)), false))
      )
    val pattern: New =
      New(
        2,
        Par()
          .prepend(Send(GInt(7), Seq(EVar(FreeVar(0))), false).withConnectiveUsed(true))
          .prepend(EVar(Wildcard(WildcardMsg())), 1)
      )

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
          MatchCase(
            EList(Seq(EVar(FreeVar(0)), EVar(FreeVar(1)))),
            Send(
              EVar(BoundVar(1)),
              Seq[Par](EVar(BoundVar(0))),
              false,
              locallyFree = BitSet(0, 1)
            )
          ),
          MatchCase(EVar(Wildcard(WildcardMsg())), Par())
        )
      )
    val pattern: Match =
      Match(
        EVar(FreeVar(0)),
        Seq(
          MatchCase(EList(Seq(EVar(FreeVar(0)), EVar(FreeVar(1)))), EVar(Wildcard(WildcardMsg()))),
          MatchCase(EVar(Wildcard(WildcardMsg())), EVar(FreeVar(1)))
        ),
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

  "Matching sets" should "work for concrete sets" in {
    val target: Expr = ParSet(Seq(GInt(1), GInt(2), GInt(3)))
    //matcher expects terms in canonical form and there's only one for any set
    val pattern: Expr  = target
    val expectedResult = Some(Map[Int, Par]())
    assertSpatialMatch(target, pattern, expectedResult)

    val targetPar: Par  = target
    val patternPar: Par = pattern
    assertSpatialMatch(targetPar, patternPar, expectedResult)
  }

  it should "work with free variables and wildcards" in {
    val target: Expr = ParSet(Seq(GInt(1), GInt(2), GInt(3), GInt(4), GInt(5)))
    val pattern: Expr =
      ParSet(
        Seq(GInt(2), GInt(5), EVar(FreeVar(0)), EVar(FreeVar(1)), EVar(Wildcard(WildcardMsg())))
      )
    //the captures and their order are somewhat arbitrary and could potentially by changed
    val expectedResult = Some(Map[Int, Par](0 -> GInt(4), 1 -> GInt(3)))
    assertSpatialMatch(target, pattern, expectedResult)

    val targetPar: Par  = target
    val patternPar: Par = pattern
    assertSpatialMatch(targetPar, patternPar, expectedResult)

    val nonMatchingPattern: Expr =
      ParSet(Seq(GInt(2), GInt(5), EVar(FreeVar(0)), EVar(Wildcard(WildcardMsg()))))
    assertSpatialMatch(target, nonMatchingPattern, None)
  }

  it should "work with wildcard remainders" in {
    val targetElements = Seq[Par](GInt(1), GInt(2), GInt(3), GInt(4), GInt(5))
    val target: Expr   = ParSet(targetElements)
    val pattern: Expr =
      ParSet(Seq(GInt(1), GInt(4)), remainder = Wildcard(WildcardMsg()))
    val expectedResult = Some(Map[Int, Par]())
    assertSpatialMatch(target, pattern, expectedResult)

    val targetPar: Par  = target
    val patternPar: Par = pattern
    assertSpatialMatch(targetPar, patternPar, expectedResult)

    val allElementsAndWildcard: Expr = ParSet(targetElements, remainder = Wildcard(WildcardMsg()))
    assertSpatialMatch(target, allElementsAndWildcard, Some(Map[Int, Par]()))

    val justWildcard: Expr = ParSet(Seq(), remainder = Wildcard(WildcardMsg()))
    assertSpatialMatch(target, justWildcard, Some(Map[Int, Par]()))
  }

  it should "work with var remainders" in {
    val targetElements = Seq[Par](GInt(1), GInt(2), GInt(3), GInt(4), GInt(5))
    val target: Expr   = ParSet(targetElements)
    val pattern: Expr =
      ParSet(
        Seq(GInt(1), GInt(4), EVar(FreeVar(0))),
        remainder = Var(FreeVar(1))
      )
    //the captures and their order are somewhat arbitrary and could potentially by changed
    val expectedResult = Some(Map[Int, Par](0 -> GInt(2), 1 -> ParSet(Seq(GInt(3), GInt(5)))))
    assertSpatialMatch(target, pattern, expectedResult)

    val targetPar: Par  = target
    val patternPar: Par = pattern
    assertSpatialMatch(targetPar, patternPar, expectedResult)

    val allElementsAndRemainder: Expr = ParSet(targetElements, remainder = Var(FreeVar(0)))
    assertSpatialMatch(target, allElementsAndRemainder, Some(Map[Int, Par](0 -> ParSet(Seq()))))

    val justRemainder: Expr = ParSet(Seq(), remainder = Var(FreeVar(0)))
    assertSpatialMatch(target, justRemainder, Some(Map[Int, Par](0 -> ParSet(targetElements))))
  }

  "Matching maps" should "work for concrete maps" in {
    val target: Expr = ParMap(Seq[(Par, Par)]((GInt(1), GInt(2)), (GInt(3), GInt(4))))
    //matcher expects terms in canonical form and there's only one for any MAP
    val pattern: Expr  = target
    val expectedResult = Some(Map[Int, Par]())
    assertSpatialMatch(target, pattern, expectedResult)

    val targetPar: Par  = target
    val patternPar: Par = pattern
    assertSpatialMatch(targetPar, patternPar, expectedResult)
  }

  it should "work with free variables and wildcards" in {
    val target: Expr =
      ParMap(Seq[(Par, Par)]((GInt(1), GInt(2)), (GInt(3), GInt(4)), (GInt(5), GInt(6))))
    val pattern: Expr =
      ParMap(
        Seq[(Par, Par)](
          (GInt(1), EVar(FreeVar(1))),
          (GInt(3), GInt(4)),
          (EVar(FreeVar(0)), EVar(Wildcard(WildcardMsg())))
        ),
        connectiveUsed = true,
        locallyFree = BitSet(0, 1),
        remainder = None
      )
    val expectedResult = Some(Map[Int, Par](0 -> GInt(5), 1 -> GInt(2)))
    assertSpatialMatch(target, pattern, expectedResult)

    val targetPar: Par  = target
    val patternPar: Par = pattern
    assertSpatialMatch(targetPar, patternPar, expectedResult)

    val nonMatchingPattern: Expr =
      ParMap(
        Seq[(Par, Par)](
          (GInt(3), EVar(FreeVar(1))),
          (EVar(FreeVar(0)), EVar(Wildcard(WildcardMsg()))),
          (EVar(Wildcard(WildcardMsg())), GInt(4))
        ),
        connectiveUsed = true,
        locallyFree = BitSet(0, 1),
        remainder = None
      )
    assertSpatialMatch(target, nonMatchingPattern, None)
  }

  it should "work with wildcard remainders" in {
    val targetElements = Seq[(Par, Par)]((GInt(1), GInt(2)), (GInt(3), GInt(4)), (GInt(5), GInt(6)))
    val target: Expr   = ParMap(targetElements)
    val pattern: Expr =
      ParMap(
        Seq[(Par, Par)]((GInt(3), GInt(4))),
        connectiveUsed = true,
        locallyFree = BitSet(),
        remainder = Wildcard(WildcardMsg())
      )
    val expectedResult = Some(Map[Int, Par]())
    assertSpatialMatch(target, pattern, expectedResult)

    val targetPar: Par  = target
    val patternPar: Par = pattern
    assertSpatialMatch(targetPar, patternPar, expectedResult)

    val allElementsAndWildcard: Expr = ParMap(
      targetElements,
      connectiveUsed = true,
      locallyFree = BitSet(),
      remainder = Wildcard(WildcardMsg())
    )
    assertSpatialMatch(target, allElementsAndWildcard, Some(Map[Int, Par]()))

    val justWildcard: Expr = ParMap(
      Seq(),
      connectiveUsed = true,
      locallyFree = BitSet(),
      remainder = Wildcard(WildcardMsg())
    )
    assertSpatialMatch(target, justWildcard, Some(Map[Int, Par]()))
  }

  it should "work with var remainders" in {
    val targetElements = Seq[(Par, Par)]((GInt(1), GInt(2)), (GInt(3), GInt(4)), (GInt(5), GInt(6)))
    val target: Expr   = ParMap(targetElements)
    val pattern: Expr =
      ParMap(
        Seq[(Par, Par)]((EVar(FreeVar(0)), GInt(4))),
        connectiveUsed = true,
        locallyFree = BitSet(),
        remainder = FreeVar(1)
      )
    val expectedResult = Some(
      Map[Int, Par](
        0 -> GInt(3),
        1 -> ParMap(Seq[(Par, Par)]((GInt(1), GInt(2)), (GInt(5), GInt(6))))
      )
    )
    assertSpatialMatch(target, pattern, expectedResult)

    val targetPar: Par  = target
    val patternPar: Par = pattern
    assertSpatialMatch(targetPar, patternPar, expectedResult)

    val allElementsAndRemainder: Expr =
      ParMap(targetElements, connectiveUsed = true, locallyFree = BitSet(), remainder = FreeVar(0))
    assertSpatialMatch(target, allElementsAndRemainder, Some(Map[Int, Par](0 -> ParMap(Seq.empty))))

    val justRemainder: Expr =
      ParMap(Seq(), connectiveUsed = true, locallyFree = BitSet(), remainder = FreeVar(0))
    assertSpatialMatch(target, justRemainder, Some(Map[Int, Par](0 -> ParMap(targetElements))))
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
        .prepend(Send(GPrivateBuilder("0"), Seq(GInt(43)), persistent = false))
        .prepend(Send(GInt(7), Seq(GInt(42)), persistent = false))
    )
    val pattern: Bundle = Bundle(
      Par()
        .prepend(Send(GInt(7), Seq(EVar(FreeVar(0))), persistent = false))
        .prepend(EVar(Wildcard(WildcardMsg())), 1)
    )

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
        .prepend(Send(GPrivateBuilder("0"), Seq(GInt(43)), persistent = false))
        .prepend(Send(GInt(7), Seq(GInt(42)), persistent = false))
    )

    val pattern: Par = EVar(FreeVar(0))

    val expectedResult = Some(Map[Int, Par](0 -> target))
    assertSpatialMatch(Par(bundles = Seq(target)), pattern, expectedResult)
    val targetPar: Par  = target
    val patternPar: Par = pattern
    assertSpatialMatch(targetPar, patternPar, expectedResult)
  }

  "Matching a single and" should "match both sides" in {
    // @7!(8)
    val target: Par     = Send(GInt(7), Seq(GInt(8)), persistent = false)
    val failTarget: Par = Send(GInt(7), Seq(GInt(9)), persistent = false)
    // @7!(x) /\ @y!(8)
    val pattern: Connective = Connective(
      ConnAndBody(
        ConnectiveBody(
          Seq(
            Send(GInt(7), Seq(EVar(FreeVar(0))), persistent = false, connectiveUsed = true),
            Send(EVar(FreeVar(1)), Seq(GInt(8)), persistent = false, connectiveUsed = true)
          )
        )
      )
    )

    val expectedResult = Some(Map[Int, Par](0 -> GInt(8), 1 -> GInt(7)))
    assertSpatialMatch(target, pattern, expectedResult)
    val patternPar: Par = pattern
    assertSpatialMatch(target, patternPar, expectedResult)

    assertSpatialMatch(failTarget, pattern, None)
    assertSpatialMatch(failTarget, patternPar, None)
  }

  "Matching a single or" should "match some side" in {
    // @7!(8)
    val target: Par     = Send(GInt(7), Seq(GInt(8)), persistent = false)
    val failTarget: Par = Send(GInt(7), Seq(GInt(9)), persistent = false)
    // @9!(x) \/ @x!(8)
    val pattern: Connective = Connective(
      ConnOrBody(
        ConnectiveBody(
          Seq(
            Send(GInt(9), Seq(EVar(FreeVar(0))), persistent = false, connectiveUsed = true),
            Send(EVar(FreeVar(0)), Seq(GInt(8)), persistent = false, connectiveUsed = true)
          )
        )
      )
    )

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
      Send(GInt(1), Seq(GInt(2)), persistent = false),
      Send(GInt(2), Seq(GInt(3)), persistent = false),
      Send(GInt(3), Seq(GInt(4)), persistent = false)
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
      Send(GInt(1), Seq(GInt(6)), persistent = false),
      Send(GInt(2), Seq(GInt(7)), persistent = false),
      Send(GInt(3), Seq(GInt(8)), persistent = false)
    )

    val failTarget: Par = Par().addSends(
      Send(GInt(1), Seq(GInt(6)), persistent = false),
      Send(GInt(2), Seq(GInt(9)), persistent = false),
      Send(GInt(3), Seq(GInt(8)), persistent = false)
    )

    // ~Nil
    val nonNullConn: Connective = Connective(ConnNotBody(Par()))
    val nonNull: Par            = nonNullConn

    // ~{ ~Nil | ~Nil }
    val singleFactor: Connective = Connective(
      ConnNotBody(
        Par()
          .addConnectives(nonNullConn, nonNullConn)
          .withConnectiveUsed(true)
      )
    )
    // x /\ y!(7)
    val capture: Connective =
      Connective(
        ConnAndBody(
          ConnectiveBody(
            Seq(EVar(FreeVar(0)), Send(EVar(FreeVar(1)), Seq(GInt(7))).withConnectiveUsed(true))
          )
        )
      )
    // ~{ ~Nil | ~Nil } & ~Nil
    val prime: Connective = Connective(
      ConnAndBody(
        ConnectiveBody(Seq(nonNull, Par().addConnectives(singleFactor).withConnectiveUsed(true)))
      )
    )
    // x!(7) \/ x!(8)
    val alternative: Connective = Connective(
      ConnOrBody(
        ConnectiveBody(
          Seq(
            Send(EVar(FreeVar(0)), Seq(GInt(7))).withConnectiveUsed(true),
            Send(EVar(FreeVar(0)), Seq(GInt(8))).withConnectiveUsed(true)
          )
        )
      )
    )
    // x /\ y!(7) | ~{ ~Nil | ~Nil } & ~Nil | x!(7) \/ x!(8)
    val pattern: Par   = Par().addConnectives(capture, prime, alternative).withConnectiveUsed(true)
    val expectedResult = Some(Map[Int, Par](0 -> Send(GInt(2), Seq(GInt(7))), 1 -> GInt(2)))
    assertSpatialMatch(target, pattern, expectedResult)

    assertSpatialMatch(failTarget, pattern, None)
  }

  "Matching a target with var ref and a pattern with a var ref" should "ignore locallyFree" in {
    val target: Par = New(
      bindCount = 1,
      p = Par(
        receives = List(
          Receive(
            binds = List(
              ReceiveBind(
                patterns = List(
                  Par().prepend(Connective(VarRefBody(VarRef(0, 1))), 1)
                ),
                source = Par()
              )
            ),
            body = Par(),
            persistent = false,
            bindCount = 0,
            locallyFree = BitSet(0)
          )
        ),
        locallyFree = BitSet(0)
      )
    )
    val pattern: Par = New(
      bindCount = 1,
      p = Par(
        receives = List(
          Receive(
            binds = List(
              ReceiveBind(
                patterns = List(Connective(VarRefBody(VarRef(0, 1)))),
                source = Par()
              )
            ),
            body = Par(),
            persistent = false,
            bindCount = 0
          )
        )
      )
    )
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
      EMapBody(ParMap(List[(Par, Par)]((GString("name"), GString("a")))))
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
    val successTarget: Par  = GBool(false)
    val failTarget: Par     = GString("Fail")
    val pattern: Connective = Connective(ConnBool(true))

    assertSpatialMatch(successTarget, pattern, Some(Map.empty))
    assertSpatialMatch(failTarget, pattern, None)
  }

  "Matching Int" should "work" in {
    val successTarget: Par  = GInt(7)
    val failTarget: Par     = GString("Fail")
    val pattern: Connective = Connective(ConnInt(true))

    assertSpatialMatch(successTarget, pattern, Some(Map.empty))
    assertSpatialMatch(failTarget, pattern, None)
  }

  "Matching BigInt" should "work" in {
    val successTarget: Par  = GBigInt(BigInt("-9999999999999999999999999999999999999999999999"))
    val failTarget: Par     = GInt(12)
    val pattern: Connective = Connective(ConnBigInt(true))

    assertSpatialMatch(successTarget, pattern, Some(Map.empty))
    assertSpatialMatch(failTarget, pattern, None)
  }

  "Matching String" should "work" in {
    val successTarget: Par  = GString("Match me!")
    val failTarget: Par     = GInt(42)
    val pattern: Connective = Connective(ConnString(true))

    assertSpatialMatch(successTarget, pattern, Some(Map.empty))
    assertSpatialMatch(failTarget, pattern, None)
  }

  "Matching Uri" should "work" in {
    val successTarget: Par  = GUri("rho:io:stdout")
    val failTarget: Par     = GString("Fail")
    val pattern: Connective = Connective(ConnUri(true))

    assertSpatialMatch(successTarget, pattern, Some(Map.empty))
    assertSpatialMatch(failTarget, pattern, None)
  }

  "Matching ByteArray" should "work" in {
    val successTarget: Par  = GByteArray(ByteString.copyFrom(Array[Byte](74, 75)))
    val failTarget: Par     = GString("Fail")
    val pattern: Connective = Connective(ConnByteArray(true))

    assertSpatialMatch(successTarget, pattern, Some(Map.empty))
    assertSpatialMatch(failTarget, pattern, None)
  }

}
