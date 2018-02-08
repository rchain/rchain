package coop.rchain.rholang.interpreter

import coop.rchain.rholang.intepreter._
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


class ParSortMatcherSpec extends FlatSpec with Matchers {
  val p = Par()
  "Par" should "Sort so that smaller integers come first" in {
    val parGround =
      p.copy(expr=
        List(
          GInt(2),
          GInt(1),
          GInt(-1),
          GInt(-2),
          GInt(0)))
    val sortedParGround =
      p.copy(expr=
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
      p.copy(expr=
        List(
          GUri("https://www.rchain.coop/"),
          GInt(47),
          GString("Hello"),
          GBool(true),
          GPrivate("foo")))
    val sortedParGround =
      p.copy(expr=
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
      p.copy(expr=
        List(
          ESet(
            List(
              p.copy(expr=List(GInt(2))),
              p.copy(expr=List(GInt(1))),
              p.copy(expr=List(
                ESet(
                  List(
                    p.copy(expr=List(GInt(1))),
                    p.copy(expr=List(GInt(2))))))),
              p.copy(expr=List(
                ESet(
                  List(
                    p.copy(expr=List(GInt(1))),
                    p.copy(expr=List(GInt(1)))))))))))
    val sortedParGround =
      p.copy(expr=
        List(
          ESet(
            List(
              p.copy(expr=List(GInt(1))),
              p.copy(expr=List(GInt(2))),
              p.copy(expr=List(
                ESet(
                  List(
                    p.copy(expr=List(GInt(1))),
                    p.copy(expr=List(GInt(1))))))),
              p.copy(expr=List(
                ESet(
                  List(
                    p.copy(expr=List(GInt(1))),
                    p.copy(expr=List(GInt(2)))))))))))
    val result = ParSortMatcher.sortMatch(parGround)
    result.term should be (sortedParGround)
  }


  "Par" should "Sort map insides" in {
    val parGround =
      p.copy(expr=
        List(
          EMap(
            List(
              (
                p.copy(expr=List(GInt(2))),
                p.copy(expr=List(GInt(1)))),
              (
                p.copy(expr=List(GInt(2))),
                p.copy(expr=List(
                  ESet(
                    List(
                      p.copy(expr=List(GInt(2))),
                      p.copy(expr=List(GInt(1)))))))),
              (
                p.copy(expr=List(GInt(1))),
                p.copy(expr=List(GInt(1))))))))
    val sortedParGround =
      p.copy(expr=
        List(
          EMap(
            List(
              (
                p.copy(expr=List(GInt(1))),
                p.copy(expr=List(GInt(1)))),
              (
                p.copy(expr=List(GInt(2))),
                p.copy(expr=List(GInt(1)))),
              (
                p.copy(expr=List(GInt(2))),
                p.copy(expr=List(
                  ESet(
                    List(
                      p.copy(expr=List(GInt(1))),
                      p.copy(expr=List(GInt(2))))))))))))
    val result = ParSortMatcher.sortMatch(parGround)
    result.term should be (sortedParGround)
  }

  "Par" should "Keep order when adding numbers" in {
    val parExpr =
      p.copy(expr=
        List(
          EPlus(
            p.copy(expr=
              List(
                EPlus(
                  p.copy(expr=
                    List(GInt(1))),
                  p.copy(expr=
                    List(GInt(3)))))),
            p.copy(expr=
              List(GInt(2))))))
    val result = ParSortMatcher.sortMatch(parExpr)
    result.term should be (parExpr)
  }

  "Par" should "Sort according to PEMDAS" in {
    val parExpr =
      p.copy(expr=
        List(
          EMinus(
            p.copy(expr=
              List(GInt(4))),
            p.copy(expr=
              List(GInt(3)))),
          EDiv(
            p.copy(expr=
              List(GInt(1))),
            p.copy(expr=
              List(GInt(5)))),
          EPlus(
            p.copy(expr=
              List(GInt(1))),
            p.copy(expr=
              List(GInt(3)))),
          EMult(
            p.copy(expr=
              List(GInt(6))),
            p.copy(expr=
              List(GInt(3))))))
    var sortedParExpr =
      p.copy(expr=
        List(
          EMult(
            p.copy(expr=
              List(GInt(6))),
            p.copy(expr=
              List(GInt(3)))),
          EDiv(
            p.copy(expr=
              List(GInt(1))),
            p.copy(expr=
              List(GInt(5)))),
          EPlus(
            p.copy(expr=
              List(GInt(1))),
            p.copy(expr=
              List(GInt(3)))),
          EMinus(
            p.copy(expr=
              List(GInt(4))),
            p.copy(expr=
              List(GInt(3))))))
    val result = ParSortMatcher.sortMatch(parExpr)
    result.term should be (sortedParExpr)
  }

  "Par" should "Sort comparisons in order of LT, LTE, GT, GTE, EQ, NEQ" in {
    val parExpr =
      p.copy(expr=
        List(
          EEq(
            p.copy(expr=
              List(GInt(4))),
            p.copy(expr=
              List(GInt(3)))),
          ENeq(
            p.copy(expr=
              List(GInt(1))),
            p.copy(expr=
              List(GInt(5)))),
          ELt(
            p.copy(expr=
              List(GInt(1))),
            p.copy(expr=
              List(GInt(5)))),
          EGt(
            p.copy(expr=
              List(GBool(false))),
            p.copy(expr=
              List(GBool(true)))),
          ELte(
            p.copy(expr=
              List(GInt(1))),
            p.copy(expr=
              List(GInt(5)))),
          EGte(
            p.copy(expr=
              List(GBool(false))),
            p.copy(expr=
              List(GBool(true))))))
    var sortedParExpr =
      p.copy(expr=
        List(
          ELt(
            p.copy(expr=
              List(GInt(1))),
            p.copy(expr=
              List(GInt(5)))),
          ELte(
            p.copy(expr=
              List(GInt(1))),
            p.copy(expr=
              List(GInt(5)))),
          EGt(
            p.copy(expr=
              List(GBool(false))),
            p.copy(expr=
              List(GBool(true)))),
          EGte(
            p.copy(expr=
              List(GBool(false))),
            p.copy(expr=
              List(GBool(true)))),
          EEq(
            p.copy(expr=
              List(GInt(4))),
            p.copy(expr=
              List(GInt(3)))),
          ENeq(
            p.copy(expr=
              List(GInt(1))),
            p.copy(expr=
              List(GInt(5))))))
    val result = ParSortMatcher.sortMatch(parExpr)
    result.term should be (sortedParExpr)
  }

  "Par" should "Sort Sends based on their channel and then data" in {
    val parExpr =
      p.copy(sends=
        List(
          Send(
            Quote(p.copy(expr=List(GInt(5)))),
            List(p.copy(expr=List(GInt(3))))),
          Send(
            Quote(p.copy(expr=List(GInt(4)))),
            List(p.copy(expr=List(GInt(2))))),
          Send(
            Quote(p.copy(expr=List(GInt(5)))),
            List(p.copy(expr=List(GInt(2)))))))
    val sortedParExpr =
      p.copy(sends=
        List(
          Send(
            Quote(p.copy(expr=List(GInt(4)))),
            List(p.copy(expr=List(GInt(2))))),
          Send(
            Quote(p.copy(expr=List(GInt(5)))),
            List(p.copy(expr=List(GInt(2))))),
          Send(
            Quote(p.copy(expr=List(GInt(5)))),
            List(p.copy(expr=List(GInt(3)))))))
    val result = ParSortMatcher.sortMatch(parExpr)
    result.term should be (sortedParExpr)
  }

  "Par" should "Sort Receives based on their channel and then pattern" in {
    val parExpr =
      p.copy(receives=
        List(
          Receive(
            List(
              (
                List(
                  Quote(p.copy(expr=List(GInt(1))))),
                Quote(p.copy(expr=List(GInt(3))))))),
          Receive(
            List(
              (
                List(
                  Quote(p.copy(expr=List(GInt(0))))),
                Quote(p.copy(expr=List(GInt(3))))))),
          Receive(
            List(
              (
                List(
                  Quote(p.copy(expr=List(GInt(100))))),
                Quote(p.copy(expr=List(GInt(2)))))))))
    val sortedParExpr =
      p.copy(receives=
        List(
          Receive(
            List(
              (
                List(
                  Quote(p.copy(expr=List(GInt(100))))),
                Quote(p.copy(expr=List(GInt(2))))))),
          Receive(
            List(
              (
                List(
                  Quote(p.copy(expr=List(GInt(0))))),
                Quote(p.copy(expr=List(GInt(3))))))),
          Receive(
            List(
              (
                List(
                  Quote(p.copy(expr=List(GInt(1))))),
                Quote(p.copy(expr=List(GInt(3)))))))))
    val result = ParSortMatcher.sortMatch(parExpr)
    result.term should be (sortedParExpr)
  }

  "Par" should "Sort EVars based on their type and then levels" in {
    val parGround =
      p.copy(expr=
        List(
          EVar(FreeVar(level=2)),
          EVar(FreeVar(level=1)),
          EVar(BoundVar(level=2)),
          EVar(BoundVar(level=1))))
    val sortedParGround =
      p.copy(expr=
        List(
          EVar(BoundVar(level=1)),
          EVar(BoundVar(level=2)),
          EVar(FreeVar(level=1)),
          EVar(FreeVar(level=2))))
    val result = ParSortMatcher.sortMatch(parGround)
    result.term should be (sortedParGround)
  }

  "Par" should "Sort exprs in order of ground, vars, arithmetic, comparisons, logical" in {
    val parExpr =
      p.copy(expr=
        List(
          EEq(
            p.copy(expr=
              List(GInt(4))),
            p.copy(expr=
              List(GInt(3)))),
          EDiv(
            p.copy(expr=
              List(GInt(1))),
            p.copy(expr=
              List(GInt(5)))),
          EVar(BoundVar(level=1)),
          EOr(
            p.copy(expr=
              List(GBool(false))),
            p.copy(expr=
              List(GBool(true)))),
          GInt(1)))
    var sortedParExpr =
      p.copy(expr=
        List(
          GInt(1),
          EVar(BoundVar(level=1)),
          EDiv(
            p.copy(expr=
              List(GInt(1))),
            p.copy(expr=
              List(GInt(5)))),
          EEq(
            p.copy(expr=
              List(GInt(4))),
            p.copy(expr=
              List(GInt(3)))),
          EOr(
            p.copy(expr=
              List(GBool(false))),
            p.copy(expr=
              List(GBool(true))))))
    val result = ParSortMatcher.sortMatch(parExpr)
    result.term should be (sortedParExpr)
  }
}