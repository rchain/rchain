package coop.rchain.rholang.interpreter

import coop.rchain.models.Channel.ChannelInstance._
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar, Wildcard}
import coop.rchain.models._
import org.scalatest._
import implicits._

import scala.collection.immutable.BitSet

class ScoredTermSpec extends FlatSpec with Matchers {
  "ScoredTerm" should "Sort so that shorter nodes come first" in {
    val unsortedTerms =
      Seq(ScoredTerm("foo", Leaves(1, 2, 2, 3)), ScoredTerm("bar", Leaves(1, 2, 2)))
    val sortedTerms = Seq(ScoredTerm("bar", Leaves(1, 2, 2)), ScoredTerm("foo", Leaves(1, 2, 2, 3)))
    unsortedTerms.sorted should be(sortedTerms)
  }
  "ScoredTerm" should "Sort so that smaller leafs stay first" in {
    val unsortedTerms = Seq(ScoredTerm("foo", Leaf(1)), ScoredTerm("bar", Leaf(2)))
    val sortedTerms   = Seq(ScoredTerm("foo", Leaf(1)), ScoredTerm("bar", Leaf(2)))
    unsortedTerms.sorted should be(sortedTerms)
  }
  "ScoredTerm" should "Sort so that smaller leafs are put first" in {
    val unsortedTerms = Seq(ScoredTerm("foo", Leaf(2)), ScoredTerm("bar", Leaf(1)))
    val sortedTerms   = Seq(ScoredTerm("bar", Leaf(1)), ScoredTerm("foo", Leaf(2)))
    unsortedTerms.sorted should be(sortedTerms)
  }
  "ScoredTerm" should "Sort so that smaller nodes are put first" in {
    val unsortedTerms = Seq(ScoredTerm("foo", Node(Seq(Leaves(1, 2), Leaves(2, 2)))),
                            ScoredTerm("bar", Node(Seq(Leaves(1, 1), Leaves(2, 2)))))
    val sortedTerms = Seq(ScoredTerm("bar", Node(Seq(Leaves(1, 1), Leaves(2, 2)))),
                          ScoredTerm("foo", Node(Seq(Leaves(1, 2), Leaves(2, 2)))))
    unsortedTerms.sorted should be(sortedTerms)
  }
}

class ReceiveSortMatcherSpec extends FlatSpec with Matchers {
  val emptyMap = DebruijnLevelMap[VarSort]()
  val p        = Par()
  "Binds" should "Presort based on their channel and then pattern" in {
    val binds: List[Tuple3[List[Channel], Channel, DebruijnLevelMap[VarSort]]] =
      List(
        (
          List(Quote(GInt(2))),
          Quote(GInt(3)),
          emptyMap
        ),
        (
          List(Quote(GInt(3))),
          Quote(GInt(2)),
          emptyMap
        ),
        (
          List(Quote(GInt(1))),
          Quote(GInt(3)),
          emptyMap
        )
      )
    val sortedBinds: List[Tuple3[List[Channel], Channel, DebruijnLevelMap[VarSort]]] =
      List(
        (
          List(Quote(GInt(3))),
          Quote(GInt(2)),
          emptyMap
        ),
        (
          List(Quote(GInt(1))),
          Quote(GInt(3)),
          emptyMap
        ),
        (
          List(Quote(GInt(2))),
          Quote(GInt(3)),
          emptyMap
        )
      )
    val result = ReceiveSortMatcher.preSortBinds(binds)
    result should be(sortedBinds)
  }
}

class VarSortMatcherSpec extends FlatSpec with Matchers {
  val p = Par()
  "Different kinds of variables" should "bin separately" in {
    val parVars = p.copy(
      exprs = List(EVar(BoundVar(2)),
                   EVar(Wildcard(Var.WildcardMsg())),
                   EVar(BoundVar(1)),
                   EVar(FreeVar(0)),
                   EVar(FreeVar(2)),
                   EVar(BoundVar(0)),
                   EVar(FreeVar(1))),
      freeCount = 4,
      locallyFree = BitSet(0, 1, 2)
    )
    val sortedParVars: Option[Par] = p.copy(
      exprs = List(EVar(BoundVar(0)),
                   EVar(BoundVar(1)),
                   EVar(BoundVar(2)),
                   EVar(FreeVar(0)),
                   EVar(FreeVar(1)),
                   EVar(FreeVar(2)),
                   EVar(Wildcard(Var.WildcardMsg()))),
      freeCount = 4,
      locallyFree = BitSet(0, 1, 2)
    )
    val result = ParSortMatcher.sortMatch(parVars)
    result.term should be(sortedParVars)
  }
}

class ParSortMatcherSpec extends FlatSpec with Matchers {
  val p = Par()
  "Par" should "Sort so that smaller integers come first" in {
    val parGround =
      p.copy(exprs = List(GInt(2), GInt(1), GInt(-1), GInt(-2), GInt(0)))
    val sortedParGround: Option[Par] =
      p.copy(exprs = List(GInt(-2), GInt(-1), GInt(0), GInt(1), GInt(2)))
    val result = ParSortMatcher.sortMatch(parGround)
    result.term should be(sortedParGround)
  }

  "Par" should "Sort in order of boolean, int, string, uri" in {
    val parGround =
      p.copy(
        exprs = List(GUri("https://www.rchain.coop/"), GInt(47), GString("Hello"), GBool(true)))
    val sortedParGround: Option[Par] =
      p.copy(
        exprs = List(GBool(true), GInt(47), GString("Hello"), GUri("https://www.rchain.coop/")))
    val result = ParSortMatcher.sortMatch(parGround)
    result.term should be(sortedParGround)
  }

  "Par" should "Sort and deduplicate sets insides" in {
    val parGround =
      ESet(
        List(
          GInt(2),
          GInt(1),
          ESet(List(GInt(1), GInt(2)), freeCount = 0, locallyFree = BitSet()),
          ESet(List(GInt(1), GInt(1)), freeCount = 0, locallyFree = BitSet())
        ),
        freeCount = 0,
        locallyFree = BitSet()
      )
    val sortedParGround: Option[Par] =
      ESet(
        List(
          GInt(1),
          GInt(2),
          ESet(List(GInt(1)), freeCount = 0, locallyFree = BitSet()),
          ESet(List(GInt(1), GInt(2)), freeCount = 0, locallyFree = BitSet())
        ),
        freeCount = 0,
        locallyFree = BitSet()
      )
    val result = ParSortMatcher.sortMatch(parGround)
    result.term should be(sortedParGround)
  }

  "Par" should "Sort map insides by key and last write should win" in {
    val parGround =
      EMap(
        List(
          KeyValuePair(GInt(2),
                       ESet(List(GInt(2), GInt(1)), freeCount = 0, locallyFree = BitSet())),
          KeyValuePair(GInt(2), GInt(1)),
          KeyValuePair(GInt(1), GInt(1))
        ),
        freeCount = 0,
        locallyFree = BitSet()
      )
    val sortedParGround: Option[Par] =
      EMap(List(KeyValuePair(GInt(1), GInt(1)), KeyValuePair(GInt(2), GInt(1))),
           freeCount = 0,
           locallyFree = BitSet())
    val result = ParSortMatcher.sortMatch(parGround)
    result.term should be(sortedParGround)
  }

  "Par" should "Keep order when adding numbers" in {
    val parExpr: Option[Par] =
      EPlus(EPlus(GInt(1), GInt(3)), GInt(2))
    val result = ParSortMatcher.sortMatch(parExpr)
    result.term should be(parExpr)
  }

  "Par" should "Sort according to PEMDAS" in {
    val parExpr =
      p.copy(
        exprs = List(EMinus(GInt(4), GInt(3)),
                     EDiv(GInt(1), GInt(5)),
                     EPlus(GInt(1), GInt(3)),
                     EMult(GInt(6), GInt(3))))
    val sortedParExpr: Option[Par] =
      p.copy(
        exprs = List(
          EMult(GInt(6), GInt(3)),
          EDiv(GInt(1), GInt(5)),
          EPlus(GInt(1), GInt(3)),
          EMinus(GInt(4), GInt(3))
        ))
    val result = ParSortMatcher.sortMatch(parExpr)
    result.term should be(sortedParExpr)
  }

  "Par" should "Sort comparisons in order of LT, LTE, GT, GTE, EQ, NEQ" in {
    val parExpr =
      p.copy(
        exprs = List(
          EEq(GInt(4), GInt(3)),
          ENeq(GInt(1), GInt(5)),
          ELt(GInt(1), GInt(5)),
          EGt(GBool(false), GBool(true)),
          ELte(GInt(1), GInt(5)),
          EGte(GBool(false), GBool(true))
        ))
    val sortedParExpr: Option[Par] =
      p.copy(
        exprs = List(
          ELt(GInt(1), GInt(5)),
          ELte(GInt(1), GInt(5)),
          EGt(GBool(false), GBool(true)),
          EGte(GBool(false), GBool(true)),
          EEq(GInt(4), GInt(3)),
          ENeq(GInt(1), GInt(5))
        ))
    val result = ParSortMatcher.sortMatch(parExpr)
    result.term should be(sortedParExpr)
  }

  "Par" should "Sort Sends based on their persistence, channel, data" in {
    val parExpr =
      p.copy(
        sends = List(
          Send(Quote(GInt(5)), List(GInt(3)), false, 0, BitSet()),
          Send(Quote(GInt(5)), List(GInt(3)), true, 0, BitSet()),
          Send(Quote(GInt(4)), List(GInt(2)), false, 0, BitSet()),
          Send(Quote(GInt(5)), List(GInt(2)), false, 0, BitSet())
        ))
    val sortedParExpr: Option[Par] =
      p.copy(
        sends = List(
          Send(Quote(GInt(4)), List(GInt(2)), false, 0, BitSet()),
          Send(Quote(GInt(5)), List(GInt(2)), false, 0, BitSet()),
          Send(Quote(GInt(5)), List(GInt(3)), false, 0, BitSet()),
          Send(Quote(GInt(5)), List(GInt(3)), true, 0, BitSet())
        ))
    val result = ParSortMatcher.sortMatch(parExpr)
    result.term should be(sortedParExpr)
  }

  "Par" should "Sort Receives based on persistence, channels, patterns and then body" in {
    val parExpr =
      p.copy(
        receives = List(
          Receive(List(ReceiveBind(List(Quote(GInt(1))), Quote(GInt(3)))),
                  Par(),
                  false,
                  0,
                  0,
                  BitSet()),
          Receive(
            List(ReceiveBind(List(Quote(GInt(0))), Quote(GInt(3)))),
            EVar(BoundVar(0)),
            false,
            0,
            0,
            BitSet()
          ),
          Receive(List(ReceiveBind(List(Quote(GInt(0))), Quote(GInt(3)))),
                  Par(),
                  false,
                  0,
                  0,
                  BitSet()),
          Receive(List(ReceiveBind(List(Quote(GInt(0))), Quote(GInt(3)))),
                  Par(),
                  true,
                  0,
                  0,
                  BitSet()),
          Receive(List(ReceiveBind(List(Quote(GInt(100))), Quote(GInt(2)))),
                  Par(),
                  false,
                  0,
                  0,
                  BitSet())
        ))
    val sortedParExpr: Option[Par] =
      p.copy(
        receives = List(
          Receive(List(ReceiveBind(List(Quote(GInt(100))), Quote(GInt(2)))),
                  Par(),
                  false,
                  0,
                  0,
                  BitSet()),
          Receive(List(ReceiveBind(List(Quote(GInt(0))), Quote(GInt(3)))),
                  Par(),
                  false,
                  0,
                  0,
                  BitSet()),
          Receive(
            List(ReceiveBind(List(Quote(GInt(0))), Quote(GInt(3)))),
            EVar(BoundVar(0)),
            false,
            0,
            0,
            BitSet()
          ),
          Receive(List(ReceiveBind(List(Quote(GInt(1))), Quote(GInt(3)))),
                  Par(),
                  false,
                  0,
                  0,
                  BitSet()),
          Receive(List(ReceiveBind(List(Quote(GInt(0))), Quote(GInt(3)))),
                  Par(),
                  true,
                  0,
                  0,
                  BitSet())
        ))
    val result = ParSortMatcher.sortMatch(parExpr)
    result.term should be(sortedParExpr)
  }

  "Par" should "Sort Match based on their value and then cases" in {
    val parMatch =
      p.copy(
        matches = List(
          Match(GInt(5),
                List(MatchCase(GInt(5), GInt(5)), MatchCase(GInt(4), GInt(4))),
                0,
                BitSet()),
          Match(GBool(true),
                List(MatchCase(GInt(5), GInt(5)), MatchCase(GInt(4), GInt(4))),
                0,
                BitSet()),
          Match(GBool(true),
                List(MatchCase(GInt(4), GInt(4)), MatchCase(GInt(3), GInt(3))),
                0,
                BitSet())
        ))
    val sortedParMatch: Option[Par] =
      p.copy(
        matches = List(
          Match(GBool(true),
                List(MatchCase(GInt(4), GInt(4)), MatchCase(GInt(3), GInt(3))),
                0,
                BitSet()),
          Match(GBool(true),
                List(MatchCase(GInt(5), GInt(5)), MatchCase(GInt(4), GInt(4))),
                0,
                BitSet()),
          Match(GInt(5),
                List(MatchCase(GInt(5), GInt(5)), MatchCase(GInt(4), GInt(4))),
                0,
                BitSet())
        ))
    val result = ParSortMatcher.sortMatch(parMatch)
    result.term should be(sortedParMatch)
  }

  "Par" should "Sort EVars based on their type and then levels" in {
    val parGround =
      p.copy(exprs = List(EVar(FreeVar(2)), EVar(FreeVar(1)), EVar(BoundVar(2)), EVar(BoundVar(1))))
    val sortedParGround: Option[Par] =
      p.copy(exprs = List(EVar(BoundVar(1)), EVar(BoundVar(2)), EVar(FreeVar(1)), EVar(FreeVar(2))))
    val result = ParSortMatcher.sortMatch(parGround)
    result.term should be(sortedParGround)
  }

  "Par" should "Sort exprs in order of ground, vars, arithmetic, comparisons, logical" in {
    val parExpr =
      p.copy(
        exprs = List(
          EEq(GInt(4), GInt(3)),
          EDiv(GInt(1), GInt(5)),
          EVar(BoundVar(1)),
          EOr(GBool(false), GBool(true)),
          GInt(1)
        ))
    val sortedParExpr: Option[Par] =
      p.copy(
        exprs = List(
          GInt(1),
          EVar(BoundVar(1)),
          EDiv(GInt(1), GInt(5)),
          EEq(GInt(4), GInt(3)),
          EOr(GBool(false), GBool(true))
        ))
    val result = ParSortMatcher.sortMatch(parExpr)
    result.term should be(sortedParExpr)
  }
}
