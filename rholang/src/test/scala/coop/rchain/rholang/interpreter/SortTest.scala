package coop.rchain.rholang.interpreter

import org.scalatest._

class ScoredTermSpec extends FlatSpec with Matchers {
  "ScoredTerm" should "Sort so that shorter nodes come first" in {
    val unsortedTerms = Seq(ScoredTerm("foo", Leaves(1,2,2,3)), ScoredTerm("bar", Leaves(1,2,2)))
    val sortedTerms = Seq(ScoredTerm("bar", Leaves(1,2,2)), ScoredTerm("foo", Leaves(1,2,2,3)))
    unsortedTerms.sorted should be (sortedTerms)
  }
  "ScoredTerm" should "Sort so that smaller leafs stay first" in {
    val unsortedTerms = Seq(ScoredTerm("foo", Leaf(1)), ScoredTerm("bar", Leaf(2)))
    val sortedTerms = Seq(ScoredTerm("foo", Leaf(1)), ScoredTerm("bar", Leaf(2)))
    unsortedTerms.sorted should be (sortedTerms)
  }
  "ScoredTerm" should "Sort so that smaller leafs are put first" in {
    val unsortedTerms = Seq(ScoredTerm("foo", Leaf(2)), ScoredTerm("bar", Leaf(1)))
    val sortedTerms = Seq(ScoredTerm("bar", Leaf(1)), ScoredTerm("foo", Leaf(2)))
    unsortedTerms.sorted should be (sortedTerms)
  }
  "ScoredTerm" should "Sort so that smaller nodes are put first" in {
    val unsortedTerms = Seq(ScoredTerm("foo", Node(Seq(Leaves(1,2), Leaves(2,2)))),
      ScoredTerm("bar", Node(Seq(Leaves(1,1), Leaves(2,2)))))
    val sortedTerms = Seq(ScoredTerm("bar", Node(Seq(Leaves(1,1), Leaves(2,2)))),
      ScoredTerm("foo", Node(Seq(Leaves(1,2), Leaves(2,2)))))
    unsortedTerms.sorted should be (sortedTerms)
  }
}

class ReceiveSortMatcherSpec extends FlatSpec with Matchers {
  val emptyMap = DebruijnLevelMap[VarSort]()
  val p = Par()
  "Binds" should "Presort based on their channel and then pattern" in {
    val binds =
      List(
        (
          List(
            Quote(p.copy(exprs=List(GInt(2))))),
          Quote(p.copy(exprs=List(GInt(3)))),
          emptyMap
        ),
        (
          List(
            Quote(p.copy(exprs=List(GInt(3))))),
          Quote(p.copy(exprs=List(GInt(2)))),
          emptyMap
        ),
        (
          List(
            Quote(p.copy(exprs=List(GInt(1))))),
          Quote(p.copy(exprs=List(GInt(3)))),
          emptyMap
        )
      )
    val sortedBinds =
      List(
        (
          List(
            Quote(p.copy(exprs=List(GInt(3))))),
          Quote(p.copy(exprs=List(GInt(2)))),
          emptyMap
        ),
        (
          List(
            Quote(p.copy(exprs=List(GInt(1))))),
          Quote(p.copy(exprs=List(GInt(3)))),
          emptyMap
        ),
        (
          List(
            Quote(p.copy(exprs=List(GInt(2))))),
          Quote(p.copy(exprs=List(GInt(3)))),
          emptyMap
        )
      )
    val result = ReceiveSortMatcher.preSortBinds(binds)
    result should be (sortedBinds)
  }
}

class ParSortMatcherSpec extends FlatSpec with Matchers {
  val p = Par()
  "Par" should "Sort so that smaller integers come first" in {
    val parGround =
      p.copy(exprs=
        List(
          GInt(2),
          GInt(1),
          GInt(-1),
          GInt(-2),
          GInt(0)))
    val sortedParGround =
      p.copy(exprs=
        List(
          GInt(-2),
          GInt(-1),
          GInt(0),
          GInt(1),
          GInt(2)))
    val result = ParSortMatcher.sortMatch(parGround)
    result.term should be (sortedParGround)
  }

  "Par" should "Sort in order of boolean, int, string, uri, private" in {
    val parGround =
      p.copy(exprs=
        List(
          GUri("https://www.rchain.coop/"),
          GInt(47),
          GString("Hello"),
          GBool(true),
          GPrivate("foo")))
    val sortedParGround =
      p.copy(exprs=
        List(
          GBool(true),
          GInt(47),
          GString("Hello"),
          GUri("https://www.rchain.coop/"),
          GPrivate("foo")))
    val result = ParSortMatcher.sortMatch(parGround)
    result.term should be (sortedParGround)
  }

  "Par" should "Sort sets insides" in {
    val parGround =
      p.copy(exprs=
        List(
          ESet(
            List(
              p.copy(exprs=List(GInt(2))),
              p.copy(exprs=List(GInt(1))),
              p.copy(exprs=List(
                ESet(
                  List(
                    p.copy(exprs=List(GInt(1))),
                    p.copy(exprs=List(GInt(2))))))),
              p.copy(exprs=List(
                ESet(
                  List(
                    p.copy(exprs=List(GInt(1))),
                    p.copy(exprs=List(GInt(1)))))))))))
    val sortedParGround =
      p.copy(exprs=
        List(
          ESet(
            List(
              p.copy(exprs=List(GInt(1))),
              p.copy(exprs=List(GInt(2))),
              p.copy(exprs=List(
                ESet(
                  List(
                    p.copy(exprs=List(GInt(1))))))),
              p.copy(exprs=List(
                ESet(
                  List(
                    p.copy(exprs=List(GInt(1))),
                    p.copy(exprs=List(GInt(2)))))))))))
    val result = ParSortMatcher.sortMatch(parGround)
    result.term should be (sortedParGround)
  }

  "Par" should "Deduplicate sets insides" in {
    val parGround =
      p.copy(exprs=
        List(
          ESet(
            List(
              p.copy(exprs=List(GInt(2))),
              p.copy(exprs=List(
                ESet(
                  List(
                    p.copy(exprs=List(GInt(1))),
                    p.copy(exprs=List(GInt(1))))))),
              p.copy(exprs=List(GInt(2))),
              p.copy(exprs=List(
                ESet(
                  List(
                    p.copy(exprs=List(GInt(1))),
                    p.copy(exprs=List(GInt(1)))))))))))
    val deduplicatedParGround =
      p.copy(exprs=
        List(
          ESet(
            List(
              p.copy(exprs=List(GInt(2))),
              p.copy(exprs=List(
                ESet(
                  List(
                    p.copy(exprs=List(GInt(1)))))))))))
    val result = ParSortMatcher.sortMatch(parGround)
    result.term should be (deduplicatedParGround)
  }

  "Par" should "Sort map insides by key and last write should win" in {
    val parGround =
      p.copy(exprs=
        List(
          EMap(
            List(
              (
                p.copy(exprs=List(GInt(2))),
                p.copy(exprs=List(
                  ESet(
                    List(
                      p.copy(exprs=List(GInt(2))),
                      p.copy(exprs=List(GInt(1)))))))),
              (
                p.copy(exprs=List(GInt(2))),
                p.copy(exprs=List(GInt(1)))),
              (
                p.copy(exprs=List(GInt(1))),
                p.copy(exprs=List(GInt(1))))))))
    val sortedParGround =
      p.copy(exprs=
        List(
          EMap(
            List(
              (
                p.copy(exprs=List(GInt(1))),
                p.copy(exprs=List(GInt(1)))),
              (
                p.copy(exprs=List(GInt(2))),
                p.copy(exprs=List(GInt(1))))))))
    val result = ParSortMatcher.sortMatch(parGround)
    result.term should be (sortedParGround)
  }

  "Par" should "Keep order when adding numbers" in {
    val parExpr =
      p.copy(exprs=
        List(
          EPlus(
            p.copy(exprs=
              List(
                EPlus(
                  p.copy(exprs=
                    List(GInt(1))),
                  p.copy(exprs=
                    List(GInt(3)))))),
            p.copy(exprs=
              List(GInt(2))))))
    val result = ParSortMatcher.sortMatch(parExpr)
    result.term should be (parExpr)
  }

  "Par" should "Sort according to PEMDAS" in {
    val parExpr =
      p.copy(exprs=
        List(
          EMinus(
            p.copy(exprs=
              List(GInt(4))),
            p.copy(exprs=
              List(GInt(3)))),
          EDiv(
            p.copy(exprs=
              List(GInt(1))),
            p.copy(exprs=
              List(GInt(5)))),
          EPlus(
            p.copy(exprs=
              List(GInt(1))),
            p.copy(exprs=
              List(GInt(3)))),
          EMult(
            p.copy(exprs=
              List(GInt(6))),
            p.copy(exprs=
              List(GInt(3))))))
    val sortedParExpr =
      p.copy(exprs=
        List(
          EMult(
            p.copy(exprs=
              List(GInt(6))),
            p.copy(exprs=
              List(GInt(3)))),
          EDiv(
            p.copy(exprs=
              List(GInt(1))),
            p.copy(exprs=
              List(GInt(5)))),
          EPlus(
            p.copy(exprs=
              List(GInt(1))),
            p.copy(exprs=
              List(GInt(3)))),
          EMinus(
            p.copy(exprs=
              List(GInt(4))),
            p.copy(exprs=
              List(GInt(3))))))
    val result = ParSortMatcher.sortMatch(parExpr)
    result.term should be (sortedParExpr)
  }

  "Par" should "Sort comparisons in order of LT, LTE, GT, GTE, EQ, NEQ" in {
    val parExpr =
      p.copy(exprs=
        List(
          EEq(
            p.copy(exprs=
              List(GInt(4))),
            p.copy(exprs=
              List(GInt(3)))),
          ENeq(
            p.copy(exprs=
              List(GInt(1))),
            p.copy(exprs=
              List(GInt(5)))),
          ELt(
            p.copy(exprs=
              List(GInt(1))),
            p.copy(exprs=
              List(GInt(5)))),
          EGt(
            p.copy(exprs=
              List(GBool(false))),
            p.copy(exprs=
              List(GBool(true)))),
          ELte(
            p.copy(exprs=
              List(GInt(1))),
            p.copy(exprs=
              List(GInt(5)))),
          EGte(
            p.copy(exprs=
              List(GBool(false))),
            p.copy(exprs=
              List(GBool(true))))))
    val sortedParExpr =
      p.copy(exprs=
        List(
          ELt(
            p.copy(exprs=
              List(GInt(1))),
            p.copy(exprs=
              List(GInt(5)))),
          ELte(
            p.copy(exprs=
              List(GInt(1))),
            p.copy(exprs=
              List(GInt(5)))),
          EGt(
            p.copy(exprs=
              List(GBool(false))),
            p.copy(exprs=
              List(GBool(true)))),
          EGte(
            p.copy(exprs=
              List(GBool(false))),
            p.copy(exprs=
              List(GBool(true)))),
          EEq(
            p.copy(exprs=
              List(GInt(4))),
            p.copy(exprs=
              List(GInt(3)))),
          ENeq(
            p.copy(exprs=
              List(GInt(1))),
            p.copy(exprs=
              List(GInt(5))))))
    val result = ParSortMatcher.sortMatch(parExpr)
    result.term should be (sortedParExpr)
  }

  "Par" should "Sort Sends based on their persistence, channel, data" in {
    val parExpr =
      p.copy(sends=
        List(
          Send(
            Quote(p.copy(exprs=List(GInt(5)))),
            List(p.copy(exprs=List(GInt(3)))),
            false),
          Send(
            Quote(p.copy(exprs=List(GInt(5)))),
            List(p.copy(exprs=List(GInt(3)))),
            true),
          Send(
            Quote(p.copy(exprs=List(GInt(4)))),
            List(p.copy(exprs=List(GInt(2)))),
            false),
          Send(
            Quote(p.copy(exprs=List(GInt(5)))),
            List(p.copy(exprs=List(GInt(2)))),
            false)))
    val sortedParExpr =
      p.copy(sends=
        List(
          Send(
            Quote(p.copy(exprs=List(GInt(4)))),
            List(p.copy(exprs=List(GInt(2)))),
            false),
          Send(
            Quote(p.copy(exprs=List(GInt(5)))),
            List(p.copy(exprs=List(GInt(2)))),
            false),
          Send(
            Quote(p.copy(exprs=List(GInt(5)))),
            List(p.copy(exprs=List(GInt(3)))),
            false),
          Send(
            Quote(p.copy(exprs=List(GInt(5)))),
            List(p.copy(exprs=List(GInt(3)))),
            true)))
    val result = ParSortMatcher.sortMatch(parExpr)
    result.term should be (sortedParExpr)
  }

  "Par" should "Sort Receives based on persistence, channels, patterns and then body" in {
    val parExpr =
      p.copy(receives=
        List(
          Receive(
            List(
              (
                List(
                  Quote(p.copy(exprs=List(GInt(1))))),
                Quote(p.copy(exprs=List(GInt(3)))))),
            Par(), false, 0),
          Receive(
            List(
              (
                List(
                  Quote(p.copy(exprs=List(GInt(0))))),
                Quote(p.copy(exprs=List(GInt(3)))))),
            p.copy(exprs = List(EVar(BoundVar(0)))), false, 0),
          Receive(
            List(
              (
                List(
                  Quote(p.copy(exprs=List(GInt(0))))),
                Quote(p.copy(exprs=List(GInt(3)))))),
            Par(), false, 0),
          Receive(
            List(
              (
                List(
                  Quote(p.copy(exprs=List(GInt(0))))),
                Quote(p.copy(exprs=List(GInt(3)))))),
            Par(), true, 0),
          Receive(
            List(
              (
                List(
                  Quote(p.copy(exprs=List(GInt(100))))),
                Quote(p.copy(exprs=List(GInt(2)))))),
            Par(), false, 0)))
    val sortedParExpr =
      p.copy(receives=
        List(
          Receive(
            List(
              (
                List(
                  Quote(p.copy(exprs=List(GInt(100))))),
                Quote(p.copy(exprs=List(GInt(2)))))),
            Par(), false, 0),
          Receive(
            List(
              (
                List(
                  Quote(p.copy(exprs=List(GInt(0))))),
                Quote(p.copy(exprs=List(GInt(3)))))),
            Par(), false, 0),
          Receive(
            List(
              (
                List(
                  Quote(p.copy(exprs=List(GInt(0))))),
                Quote(p.copy(exprs=List(GInt(3)))))),
            p.copy(exprs = List(EVar(BoundVar(0)))), false, 0),
          Receive(
            List(
              (
                List(
                  Quote(p.copy(exprs=List(GInt(1))))),
                Quote(p.copy(exprs=List(GInt(3)))))),
            Par(), false, 0),
          Receive(
            List(
              (
                List(
                  Quote(p.copy(exprs=List(GInt(0))))),
                Quote(p.copy(exprs=List(GInt(3)))))),
            Par(), true, 0)))
    val result = ParSortMatcher.sortMatch(parExpr)
    result.term should be (sortedParExpr)
  }

  "Par" should "Sort Match based on their value and then cases" in {
    val parMatch =
      p.copy(matches=
        List(
          Match(
            GInt(5),
            List((GInt(5), GInt(5)),
                 (GInt(4), GInt(4)))),
          Match(
            GBool(true),
            List((GInt(5), GInt(5)),
                 (GInt(4), GInt(4)))),
          Match(
            GBool(true),
            List((GInt(4), GInt(4)),
                 (GInt(3), GInt(3))))
          ))
    val sortedParMatch =
      p.copy(matches=
        List(
          Match(
            GBool(true),
            List((GInt(4), GInt(4)),
              (GInt(3), GInt(3)))),
          Match(
            GBool(true),
            List((GInt(5), GInt(5)),
              (GInt(4), GInt(4)))),
          Match(
            GInt(5),
            List((GInt(5), GInt(5)),
              (GInt(4), GInt(4))))
        ))
    val result = ParSortMatcher.sortMatch(parMatch)
    result.term should be (sortedParMatch)
  }

  "Par" should "Sort EVars based on their type and then levels" in {
    val parGround =
      p.copy(exprs=
        List(
          EVar(FreeVar(level=2)),
          EVar(FreeVar(level=1)),
          EVar(BoundVar(level=2)),
          EVar(BoundVar(level=1))))
    val sortedParGround =
      p.copy(exprs=
        List(
          EVar(BoundVar(level=1)),
          EVar(BoundVar(level=2)),
          EVar(FreeVar(level=1)),
          EVar(FreeVar(level=2))))
    val result = ParSortMatcher.sortMatch(parGround)
    result.term should be (sortedParGround)
  }

  "Par" should "Sort exprss in order of ground, vars, arithmetic, comparisons, logical" in {
    val parExpr =
      p.copy(exprs=
        List(
          EEq(
            p.copy(exprs=
              List(GInt(4))),
            p.copy(exprs=
              List(GInt(3)))),
          EDiv(
            p.copy(exprs=
              List(GInt(1))),
            p.copy(exprs=
              List(GInt(5)))),
          EVar(BoundVar(level=1)),
          EOr(
            p.copy(exprs=
              List(GBool(false))),
            p.copy(exprs=
              List(GBool(true)))),
          GInt(1)))
    val sortedParExpr =
      p.copy(exprs=
        List(
          GInt(1),
          EVar(BoundVar(level=1)),
          EDiv(
            p.copy(exprs=
              List(GInt(1))),
            p.copy(exprs=
              List(GInt(5)))),
          EEq(
            p.copy(exprs=
              List(GInt(4))),
            p.copy(exprs=
              List(GInt(3)))),
          EOr(
            p.copy(exprs=
              List(GBool(false))),
            p.copy(exprs=
              List(GBool(true))))))
    val result = ParSortMatcher.sortMatch(parExpr)
    result.term should be (sortedParExpr)
  }
}
