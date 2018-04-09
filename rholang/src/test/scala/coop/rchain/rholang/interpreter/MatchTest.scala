package coop.rchain.rholang.interpreter

import cats.{Eval => _, _}
import cats.data._
import cats.implicits._

import coop.rchain.models.Channel.ChannelInstance._
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance._
import coop.rchain.models.Var.WildcardMsg
import coop.rchain.models._
import coop.rchain.rholang.syntax.rholang_mercury.Absyn.{
  Ground => AbsynGround,
  Send => AbsynSend,
  _
}

import org.scalatest._

import scala.collection.immutable.BitSet

class VarMatcherSpec extends FlatSpec with Matchers {
  import implicits._
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
    val target: Par = EList(List[Par](GString("add"), GInt(7), GInt(8)), 0, BitSet())
    val pattern: Par =
      EList(List[Par](GString("add"), EVar(FreeVar(0)), EVar(FreeVar(1))), 2, BitSet())
    val result = spatialMatch(target, pattern).runS(emptyMap)
    result should be(Some(Map[Int, Par](0 -> GInt(7), 1 -> GInt(8))))
  }

  "Matching 2 lists in parallel" should "work" in {
    // The second pattern will be checked first because of prepend.
    // It matches both targets, but the first pattern only matches one of the lists.
    val target: Par = EList(List(GInt(7), GInt(8)), 0, BitSet())
      .prepend(EList(List(GInt(7), GInt(9)), 0, BitSet()))
    val pattern: Par = EList(List(EVar(FreeVar(0)), GInt(8)), 1, BitSet())
      .prepend(EList(List(GInt(7), EVar(FreeVar(1))), 1, BitSet()))
    val result = spatialMatch(target, pattern).runS(emptyMap)
    result should be(Some(Map[Int, Par](0 -> GInt(7), 1 -> GInt(9))))
  }

  "Matching a send's channel" should "work" in {
    val target: Send =
      Send(Quote(GPrivate("unforgeable")), List(GInt(7), GInt(8)), false, 0, BitSet())
    val pattern: Send =
      Send(ChanVar(FreeVar(0)), List(EVar(wc), GInt(8)), false, 1, BitSet())
    val result = spatialMatch(target, pattern).runS(emptyMap)
    result should be(Some(Map[Int, Par](0 -> GPrivate("unforgeable"))))
  }

  "Matching a send's body" should "work" in {
    val target: Send =
      Send(Quote(GPrivate("unforgeable")), List(GInt(7), GInt(8)), false, 0, BitSet())
    val pattern: Send = Send(ChanVar(wc), List(EVar(FreeVar(0)), GInt(8)), false, 1, BitSet())
    val result =
      spatialMatch(target, pattern).runS(emptyMap)
    result should be(Some(Map[Int, Par](0 -> GInt(7))))
  }
  "Matching a send" should "require arity matching in" in {
    val target: Send =
      Send(Quote(GPrivate("unforgeable")), List(GInt(7), GInt(8), GInt(9)), false, 0, BitSet())
    val pattern: Send = Send(ChanVar(wc), List(EVar(FreeVar(0)), EVar(wc)), false, 1, BitSet())
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
      Send(Quote(GPrivate("zero")), List(GInt(7), GPrivate("one")), false, 0, BitSet())
    val pattern: Par =
      Send(ChanVar(FreeVar(0)), List(GInt(7), EVar(FreeVar(1))), false, 2, BitSet())
    val result = spatialMatch(sendTarget, pattern).runS(emptyMap)
    result should be(Some(Map[Int, Par](0 -> GPrivate("zero"), 1 -> GPrivate("one"))))
  }

  "Matching a receive with a free variable in the channel and a free variable in the body" should "capture for both variables." in {
    val target: Receive = Receive(
      List(
        ReceiveBind(List(Quote(EVar(FreeVar(0))), Quote(EVar(FreeVar(1)))), Quote(GInt(7))),
        ReceiveBind(List(Quote(EVar(FreeVar(0))), Quote(EVar(FreeVar(1)))), Quote(GInt(8)))
      ),
      Send(Quote(GPrivate("unforgeable")), List(GInt(9), GInt(10)), false, 0, BitSet()),
      false,
      4,
      2
    )
    val pattern: Receive = Receive(
      List(
        ReceiveBind(List(Quote(EVar(FreeVar(0))), Quote(EVar(FreeVar(1)))), Quote(GInt(7))),
        ReceiveBind(List(Quote(EVar(FreeVar(0))), Quote(EVar(FreeVar(1)))), ChanVar(FreeVar(0)))
      ),
      EVar(FreeVar(1)),
      false,
      4,
      2
    )
    val result = spatialMatch(target, pattern).runS(emptyMap)
    result should be(
      Some(
        Map[Int, Par](
          0 -> GInt(8),
          1 -> Send(Quote(GPrivate("unforgeable")), List(GInt(9), GInt(10)), false, 0, BitSet()))))
  }

  "Matching an eval with no free variables" should "Succeed, but not capture anything." in {
    val target: Eval  = Eval(ChanVar(BoundVar(0)))
    val pattern: Eval = Eval(ChanVar(BoundVar(0)))
    val result        = spatialMatch(target, pattern).runS(emptyMap)
    result should be(Some(Map.empty[Int, Par]))
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
            .prepend(Send(Quote(GInt(7)), Seq(EVar(FreeVar(0))), false, freeCount = 1))
            .prepend(EVar(Wildcard(WildcardMsg()))))
    val result = spatialMatch(target, pattern).runS(emptyMap)

    result should be(Some(Map[Int, Par](0 -> GInt(42))))
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
            MatchCase(EVar(Wildcard(WildcardMsg())), EVar(FreeVar(1))))
      )
    val result = spatialMatch(target, pattern).runS(emptyMap)

    result should be(Some(Map[Int, Par](0 -> EList(Seq(GInt(4), GInt(20))), 1 -> Par())))
  }
}
