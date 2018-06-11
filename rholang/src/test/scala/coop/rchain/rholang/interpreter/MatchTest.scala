package coop.rchain.rholang.interpreter

import cats.{Eval => _, _}
import cats.data._
import cats.implicits._

import coop.rchain.models.Channel.ChannelInstance._
import coop.rchain.models.Connective.ConnectiveInstance._
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance._
import coop.rchain.models.Var.WildcardMsg
import coop.rchain.models._
import coop.rchain.rholang.syntax.rholang_mercury.Absyn.{
  Bundle => AbsynBundle,
  Ground => AbsynGround,
  Send => AbsynSend,
  _
}

import org.scalatest._

import scala.collection.immutable.BitSet

class VarMatcherSpec extends FlatSpec with Matchers {
  import coop.rchain.models.rholang.implicits._
  import SpatialMatcher._
  val wc = Wildcard(Var.WildcardMsg())
  "Matching ground with var" should "work" in {
    val target: Par  = GInt(7)
    val pattern: Par = EVar(FreeVar(0))
    val result       = spatialMatch(target, pattern).runS(emptyMap)
    result should be(Some(Map[Int, Par](0 -> GInt(7))))
  }
  "Matching var with var" should "fail" in {
    val target: Par  = EVar(BoundVar(0))
    val pattern: Par = EVar(FreeVar(0))
    val result       = spatialMatch(target, pattern).runS(emptyMap)
    result should be(None)
  }
  "Matching lists of grounds with lists of vars" should "work" in {
    val target: Par = EList(List[Par](GString("add"), GInt(7), GInt(8)), BitSet())
    val pattern: Par =
      EList(List[Par](GString("add"), EVar(FreeVar(0)), EVar(FreeVar(1))), BitSet())
        .withConnectiveUsed(true)
    val result = spatialMatch(target, pattern).runS(emptyMap)
    result should be(Some(Map[Int, Par](0 -> GInt(7), 1 -> GInt(8))))
  }

  "Matching 2 lists in parallel" should "work" in {
    // The second pattern will be checked first because of prepend.
    // It matches both targets, but the first pattern only matches one of the lists.
    val target: Par = EList(List(GInt(7), GInt(8)), BitSet())
      .prepend(EList(List(GInt(7), GInt(9)), BitSet()))
    val pattern: Par = EList(List(EVar(FreeVar(0)).withConnectiveUsed(true), GInt(8)), BitSet())
      .withConnectiveUsed(true)
      .prepend(EList(List(GInt(7), EVar(FreeVar(1)).withConnectiveUsed(true)), BitSet())
        .withConnectiveUsed(true))
    val result = spatialMatch(target, pattern).runS(emptyMap)
    result should be(Some(Map[Int, Par](0 -> GInt(7), 1 -> GInt(9))))
  }

  "Matching a send's channel" should "work" in {
    val target: Send =
      Send(Quote(GPrivate("unforgeable")), List(GInt(7), GInt(8)), false, BitSet())
    val pattern: Send =
      Send(ChanVar(FreeVar(0)), List(EVar(wc), GInt(8)), false, BitSet(), true)
    val expectedResult = Some(Map[Int, Par](0 -> GPrivate("unforgeable")))
    val result         = spatialMatch(target, pattern).runS(emptyMap)
    result should be(expectedResult)
    val targetPar: Par  = target
    val patternPar: Par = pattern
    val parResult       = spatialMatch(targetPar, patternPar).runS(emptyMap)
    parResult should be(expectedResult)
  }

  "Matching a send's body" should "work" in {
    val target: Send =
      Send(Quote(GPrivate("unforgeable")), List(GInt(7), GInt(8)), false, BitSet())
    val pattern: Send  = Send(ChanVar(wc), List(EVar(FreeVar(0)), GInt(8)), false, BitSet(), true)
    val expectedResult = Some(Map[Int, Par](0 -> GInt(7)))
    val result =
      spatialMatch(target, pattern).runS(emptyMap)
    result should be(expectedResult)
    val targetPar: Par  = target
    val patternPar: Par = pattern
    val parResult       = spatialMatch(targetPar, patternPar).runS(emptyMap)
    parResult should be(expectedResult)
  }
  "Matching a send" should "require arity matching in" in {
    val target: Send =
      Send(Quote(GPrivate("unforgeable")), List(GInt(7), GInt(8), GInt(9)), false, BitSet())
    val pattern: Send = Send(ChanVar(wc), List(EVar(FreeVar(0)), EVar(wc)), false, BitSet(), true)
    val result        = spatialMatch(target, pattern).runS(emptyMap)
    result should be(None)
  }

  "Matching extras with free variable" should "work" in {
    val target: Par  = GInt(7).prepend(GInt(8)).prepend(GInt(9))
    val pattern: Par = GInt(8).prepend(EVar(FreeVar(0)))
    val result       = spatialMatch(target, pattern).runS(emptyMap)
    result should be(Some(Map[Int, Par](0 -> GInt(9).prepend(GInt(7)))))
  }
  "Matching extras with wildcard" should "work" in {
    val target: Par  = GInt(7).prepend(GInt(8)).prepend(GInt(9))
    val pattern: Par = GInt(8).prepend(EVar(Wildcard(Var.WildcardMsg())))
    val result       = spatialMatch(target, pattern).runS(emptyMap)
    result should be(Some(Map.empty[Int, Par]))
  }
  "Matching extras with wildcard and free variable" should "capture in the free variable" in {
    val target: Par  = GInt(7).prepend(GInt(8)).prepend(GInt(9))
    val pattern: Par = GInt(8).prepend(EVar(Wildcard(Var.WildcardMsg()))).prepend(EVar(FreeVar(0)))
    val result       = spatialMatch(target, pattern).runS(emptyMap)
    result should be(Some(Map[Int, Par](0 -> GInt(9).prepend(GInt(7)))))
  }
  "Matching send with free variable in channel and variable position" should "capture both values" in {
    val sendTarget: Par =
      Send(Quote(GPrivate("zero")), List(GInt(7), GPrivate("one")), false, BitSet())
    val pattern: Par =
      Send(ChanVar(FreeVar(0)), List(GInt(7), EVar(FreeVar(1))), false, BitSet(), true)
        .withConnectiveUsed(true)
    val expectedResult = Some(Map[Int, Par](0 -> GPrivate("zero"), 1 -> GPrivate("one")))
    val result         = spatialMatch(sendTarget, pattern).runS(emptyMap)
    result should be(expectedResult)
    val targetPar: Par  = sendTarget
    val patternPar: Par = pattern
    val parResult       = spatialMatch(targetPar, patternPar).runS(emptyMap)
    parResult should be(expectedResult)
  }

  "Matching a receive with a free variable in the channel and a free variable in the body" should "capture for both variables." in {
    val target: Receive = Receive(
      List(
        ReceiveBind(List(Quote(EVar(FreeVar(0))), Quote(EVar(FreeVar(1)))), Quote(GInt(7))),
        ReceiveBind(List(Quote(EVar(FreeVar(0))), Quote(EVar(FreeVar(1)))), Quote(GInt(8)))
      ),
      Send(Quote(GPrivate("unforgeable")), List(GInt(9), GInt(10)), false, BitSet()),
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
          1 -> Send(Quote(GPrivate("unforgeable")), List(GInt(9), GInt(10)), false, BitSet())))
    val result = spatialMatch(target, pattern).runS(emptyMap)
    result should be(expectedResult)
    val targetPar: Par  = target
    val patternPar: Par = pattern
    val parResult       = spatialMatch(targetPar, patternPar).runS(emptyMap)
    parResult should be(expectedResult)
  }

  "Matching an eval with no free variables" should "Succeed, but not capture anything." in {
    val target: Expr   = EEvalBody(ChanVar(BoundVar(0)))
    val pattern: Expr  = EEvalBody(ChanVar(BoundVar(0)))
    val expectedResult = Some(Map.empty[Int, Par])
    val result         = spatialMatch(target, pattern).runS(emptyMap)
    result should be(expectedResult)
    val targetPar: Par  = target
    val patternPar: Par = pattern
    val parResult       = spatialMatch(targetPar, patternPar).runS(emptyMap)
    parResult should be(expectedResult)
  }

  "Matching between New's" should "match the bodies if the new count is the same" in {
    val target: New =
      New(2,
          Par()
            .prepend(Send(Quote(GInt(7)), Seq(GInt(42)), false))
            .prepend(Send(ChanVar(BoundVar(0)), Seq(GInt(43)), false, locallyFree = BitSet(0))))
    val pattern: New =
      New(2,
          Par()
            .prepend(Send(Quote(GInt(7)), Seq(EVar(FreeVar(0))), false).withConnectiveUsed(true))
            .prepend(EVar(Wildcard(WildcardMsg()))))

    val expectedResult = Some(Map[Int, Par](0 -> GInt(42)))
    val result         = spatialMatch(target, pattern).runS(emptyMap)
    result should be(expectedResult)
    val targetPar: Par  = target
    val patternPar: Par = pattern
    val parResult       = spatialMatch(targetPar, patternPar).runS(emptyMap)
    parResult should be(expectedResult)
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
    val result = spatialMatch(target, pattern).runS(emptyMap)

    val expectedResult = (Some(Map[Int, Par](0 -> EList(Seq(GInt(4), GInt(20))), 1 -> Par())))
    result should be(expectedResult)
    val targetPar: Par  = target
    val patternPar: Par = pattern
    val parResult       = spatialMatch(targetPar, patternPar).runS(emptyMap)
    parResult should be(expectedResult)
  }

  "Matching a list with a remainder" should "capture the remainder." in {
    val target: Expr   = EList(Seq(GInt(1), GInt(2)))
    val pattern: Expr  = EList(Seq(GInt(1)), remainder = Some(FreeVar(0)), connectiveUsed = true)
    val result         = spatialMatch(target, pattern).runS(emptyMap)
    val expectedResult = (Some(Map[Int, Par](0 -> EList(Seq(GInt(2))))))
    result should be(expectedResult)
    val targetPar: Par  = target
    val patternPar: Par = pattern
    val parResult       = spatialMatch(targetPar, patternPar).runS(emptyMap)
    parResult should be(expectedResult)
  }

  "Matching inside bundles" should "not be possible" in {
    val target: Bundle = Bundle(
      Par()
        .prepend(Send(Quote(GInt(7)), Seq(GInt(42)), persistent = false))
        .prepend(Send(Quote(GPrivate("0")), Seq(GInt(43)), persistent = false)))
    val pattern: Bundle = Bundle(
      Par()
        .prepend(Send(Quote(GInt(7)), Seq(EVar(FreeVar(0))), persistent = false))
        .prepend(EVar(Wildcard(WildcardMsg()))))

    val result = spatialMatch(target, pattern).runS(emptyMap)
    result should be(None)
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
        .prepend(Send(Quote(GInt(7)), Seq(GInt(42)), persistent = false))
        .prepend(Send(Quote(GPrivate("0")), Seq(GInt(43)), persistent = false)))

    val pattern: Par = EVar(FreeVar(0))

    val result         = spatialMatch(Par(bundles = Seq(target)), pattern).runS(emptyMap)
    val expectedResult = (Some(Map[Int, Par](0 -> target)))
    result should be(expectedResult)
    val targetPar: Par  = target
    val patternPar: Par = pattern
    val parResult       = spatialMatch(targetPar, patternPar).runS(emptyMap)
    parResult should be(expectedResult)
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
    val result         = spatialMatch(target, pattern).runS(emptyMap)

    result should be(expectedResult)
    val patternPar: Par = pattern
    val parResult       = spatialMatch(target, patternPar).runS(emptyMap)
    parResult should be(expectedResult)

    val failResult    = spatialMatch(failTarget, pattern).runS(emptyMap)
    val failParResult = spatialMatch(failTarget, patternPar).runS(emptyMap)
    failResult should be(None)
    failParResult should be(None)
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
    val result         = spatialMatch(target, pattern).runS(emptyMap)

    result should be(expectedResult)
    val patternPar: Par = pattern
    val parResult       = spatialMatch(target, patternPar).runS(emptyMap)
    parResult should be(expectedResult)

    val failResult    = spatialMatch(failTarget, pattern).runS(emptyMap)
    val failParResult = spatialMatch(failTarget, patternPar).runS(emptyMap)
    failResult should be(None)
    failParResult should be(None)
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
    val result         = spatialMatch(target, pattern).runS(emptyMap)

    result should be(expectedResult)

    val patternPar: Par = pattern
    val parResult       = spatialMatch(target, patternPar).runS(emptyMap)
    parResult should be(expectedResult)

    // ~Nil | ~Nil
    val doublePatternPar = patternPar.addConnectives(pattern)
    val doubleResult     = spatialMatch(target, doublePatternPar).runS(emptyMap)
    doubleResult should be(expectedResult)

    // ~Nil | ~Nil | ~Nil
    val triplePatternPar = doublePatternPar.addConnectives(pattern)
    val tripleResult     = spatialMatch(target, triplePatternPar).runS(emptyMap)
    tripleResult should be(expectedResult)

    // ~Nil | ~Nil | ~Nil | ~Nil
    // Fails because there is no way to split 3 sends into 4 non nil terms.
    val quadruplePatternPar = triplePatternPar.addConnectives(pattern)
    val quadrupleResult     = spatialMatch(target, quadruplePatternPar).runS(emptyMap)
    quadrupleResult should be(None)
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
    // ~{ ~Nil | ~Nil } & ~Nil
    val prime: Connective = Connective(
      ConnAndBody(
        ConnectiveBody(Seq(nonNull, Par().addConnectives(singleFactor).withConnectiveUsed(true)))))
    // x /\ y!(7)
    val capture: Connective =
      Connective(
        ConnAndBody(ConnectiveBody(
          Seq(EVar(FreeVar(0)), Send(ChanVar(FreeVar(1)), Seq(GInt(7))).withConnectiveUsed(true)))))
    // x!(7) \/ x!(8)
    val alternative: Connective = Connective(
      ConnOrBody(
        ConnectiveBody(Seq(Send(ChanVar(FreeVar(0)), Seq(GInt(7))).withConnectiveUsed(true),
                           Send(ChanVar(FreeVar(0)), Seq(GInt(8))).withConnectiveUsed(true)))))
    // ~{ ~Nil | ~Nil } & ~Nil | x /\ y!(7) | x!(7) \/ x!(8)
    val pattern: Par   = Par().addConnectives(prime, capture, alternative).withConnectiveUsed(true)
    val expectedResult = Some(Map[Int, Par](0 -> Send(Quote(GInt(2)), Seq(GInt(7))), 1 -> GInt(2)))
    val result         = spatialMatch(target, pattern).runS(emptyMap)
    result should be(expectedResult)

    val failResult = spatialMatch(failTarget, pattern).runS(emptyMap)
    failResult should be(None)
  }
}
