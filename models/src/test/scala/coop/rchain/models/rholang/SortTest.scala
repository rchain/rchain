package coop.rchain.models.rholang

import com.google.protobuf.ByteString
import coop.rchain.shared.Base16
import coop.rchain.models.Connective.ConnectiveInstance._
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar, Wildcard}
import coop.rchain.models.rholang.SortTest.sort
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.rholang.sorter._
import coop.rchain.models.testUtils.TestUtils.forAllSimilarA
import coop.rchain.models.{New, _}
import monix.eval.Coeval
import org.scalacheck.Arbitrary
import org.scalatest._
import org.scalatest.prop.PropertyChecks

import scala.collection.immutable.BitSet

object SortTest {
  def sort[T: Sortable](t: T) = Sortable[T].sortMatch[Coeval](t).value
}

class ScoredTermSpec extends FlatSpec with PropertyChecks with Matchers {

  behavior of "ScoredTerm"

  it should "sort so that shorter nodes come first" in {
    val unsortedTerms =
      Seq(ScoredTerm("foo", Leaves(1, 2, 2, 3)), ScoredTerm("bar", Leaves(1, 2, 2)))
    val sortedTerms = Seq(ScoredTerm("bar", Leaves(1, 2, 2)), ScoredTerm("foo", Leaves(1, 2, 2, 3)))
    unsortedTerms.sorted should be(sortedTerms)
  }
  it should "sort so that smaller leafs stay first" in {
    val unsortedTerms = Seq(ScoredTerm("foo", Leaf(1)), ScoredTerm("bar", Leaf(2)))
    val sortedTerms   = Seq(ScoredTerm("foo", Leaf(1)), ScoredTerm("bar", Leaf(2)))
    unsortedTerms.sorted should be(sortedTerms)
  }
  it should "sort so that smaller leafs are put first" in {
    val unsortedTerms = Seq(ScoredTerm("foo", Leaf(2)), ScoredTerm("bar", Leaf(1)))
    val sortedTerms   = Seq(ScoredTerm("bar", Leaf(1)), ScoredTerm("foo", Leaf(2)))
    unsortedTerms.sorted should be(sortedTerms)
  }
  it should "sort so that smaller nodes are put first" in {
    val unsortedTerms = Seq(
      ScoredTerm("foo", Node(Seq(Leaves(1, 2), Leaves(2, 2)))),
      ScoredTerm("bar", Node(Seq(Leaves(1, 1), Leaves(2, 2))))
    )
    val sortedTerms = Seq(
      ScoredTerm("bar", Node(Seq(Leaves(1, 1), Leaves(2, 2)))),
      ScoredTerm("foo", Node(Seq(Leaves(1, 2), Leaves(2, 2))))
    )
    unsortedTerms.sorted should be(sortedTerms)
  }
  it should "sort so that whenever scores differ then result terms have to differ and the other way around" in {
    def check[A: Sortable: Arbitrary]: Unit =
      forAllSimilarA[A] { (x, y) =>
        val xSorted = sort(x)
        val ySorted = sort(y)
        assert((xSorted.term == ySorted.term) == (xSorted.score == ySorted.score))
      }

    import coop.rchain.models.testImplicits._
    check[Bundle]
    check[Connective]
    check[Expr]
    check[Match]
    check[New]
    check[Par]
    check[Receive]
    check[Send]
    check[Var]
  }

  it should "sort so that whenever scores or result terms differ then the initial terms differ and the other way around" in {
    def check[A: Sortable: Arbitrary]: Unit =
      forAllSimilarA[A] { (x, y) =>
        val xSorted = sort(x)
        val ySorted = sort(y)
        if (xSorted.score != ySorted.score || xSorted.term != ySorted.term)
          assert(x != y)
        else
          assert(x == y)
      }

    import coop.rchain.models.testImplicits._
    check[Bundle]
    check[Connective]
    check[Expr]
    check[Match]
    check[New]
    check[Par]
    check[Receive]
    check[Send]
    check[Var]
  }
  it should "sort so that unequal terms have unequal scores and the other way around" in {
    def checkScoreEquality[A: Sortable: Arbitrary]: Unit =
      forAllSimilarA[A](
        (x, y) =>
          if (x != y)
            assert(sort(x).score != sort(y).score)
          else
            assert(sort(x).score == sort(y).score)
      )

    import coop.rchain.models.testImplicits._

    checkScoreEquality[Bundle]
    checkScoreEquality[Connective]
    checkScoreEquality[Expr]
    checkScoreEquality[Match]
    checkScoreEquality[New]
    checkScoreEquality[Par]
    checkScoreEquality[Receive]
    checkScoreEquality[Send]
    checkScoreEquality[Var]
  }
  it should "score in different ByteString should be unequal" in {
    // ci failing case https://github.com/rchain/rchain/runs/2268175723
    // a is Expr(GByteArray(<ByteString@65e14bdc size=1 contents="\200">))
    // b is Expr(GByteArray(<ByteString@2f125f4b size=1 contents="\331">))
    val a = Expr(GByteArray(ByteString.copyFrom(Base16.unsafeDecode("80"))))
    val b = Expr(GByteArray(ByteString.copyFrom(Base16.unsafeDecode("d9"))))
    assert(sort(a).score != sort(b).score)
  }
  it should "sort so that unequal New have unequal scores" in {
    val new1 = New(
      bindCount = 1,
      injections = Map(
        "" -> Par(
          bundles = Vector(
            Bundle(
              Par(),
              writeFlag = true
            )
          )
        )
      )
    )
    val new2 = New(bindCount = 1)
    assert(new1 != new2)
    assert(sort(new1).score != sort(new2).score)
  }
  it should "sort so that unequal EMethod have unequal scores" in {
    val method1 = Expr(EMethodBody(EMethod(connectiveUsed = true)))
    val method2 = Expr(EMethodBody(EMethod(connectiveUsed = false)))
    assert(sort(method1).score != sort(method2).score)
  }
  it should "sort so that unequal ParMap have unequal scores" in {
    val map1 = Expr(EMapBody(ParMap(Seq.empty, connectiveUsed = true, BitSet(), None)))
    val map2 = Expr(EMapBody(ParMap(Seq.empty, connectiveUsed = false, BitSet(), None)))
    assert(sort(map1).score != sort(map2).score)
    val map3 = Expr(EMapBody(ParMap(Seq.empty, connectiveUsed = true, BitSet(), None)))
    val map4 = Expr(EMapBody(ParMap(Seq.empty, connectiveUsed = true, BitSet(), Some(Var()))))
    assert(sort(map3).score != sort(map4).score)
  }
  it should "sort so that unequal ParSet have unequal scores" in {
    val set1 =
      Expr(ESetBody(ParSet(Seq.empty, connectiveUsed = true, Coeval.delay(BitSet()), None)))
    val set2 =
      Expr(ESetBody(ParSet(Seq.empty, connectiveUsed = false, Coeval.delay(BitSet()), None)))
    assert(sort(set1).score != sort(set2).score)
    val set3 =
      Expr(ESetBody(ParSet(Seq.empty, connectiveUsed = true, Coeval.delay(BitSet()), None)))
    val set4 =
      Expr(ESetBody(ParSet(Seq.empty, connectiveUsed = true, Coeval.delay(BitSet()), Some(Var()))))
    assert(sort(set3).score != sort(set4).score)
  }
  it should "sort so that unequal List have unequal scores" in {
    val list1 =
      Expr(EListBody(EList(Seq.empty, AlwaysEqual(BitSet()), connectiveUsed = true, None)))
    val list2 =
      Expr(EListBody(EList(Seq.empty, AlwaysEqual(BitSet()), connectiveUsed = false, None)))
    assert(sort(list1).score != sort(list2).score)
    val list3 =
      Expr(EListBody(EList(Seq.empty, AlwaysEqual(BitSet()), connectiveUsed = true, None)))
    val list4 =
      Expr(EListBody(EList(Seq.empty, AlwaysEqual(BitSet()), connectiveUsed = true, Some(Var()))))
    assert(sort(list3).score != sort(list4).score)
  }
}

class VarSortMatcherSpec extends FlatSpec with Matchers {
  "Different kinds of variables" should "bin separately" in {
    val parVars = Par(
      exprs = List(
        EVar(BoundVar(2)),
        EVar(Wildcard(Var.WildcardMsg())),
        EVar(BoundVar(1)),
        EVar(FreeVar(0)),
        EVar(FreeVar(2)),
        EVar(BoundVar(0)),
        EVar(FreeVar(1))
      ),
      locallyFree = BitSet(0, 1, 2),
      connectiveUsed = true
    )
    val sortedParVars = Par(
      exprs = List(
        EVar(BoundVar(0)),
        EVar(BoundVar(1)),
        EVar(BoundVar(2)),
        EVar(FreeVar(0)),
        EVar(FreeVar(1)),
        EVar(FreeVar(2)),
        EVar(Wildcard(Var.WildcardMsg()))
      ),
      locallyFree = BitSet(0, 1, 2),
      connectiveUsed = true
    )
    val result = sort(parVars)
    result.term should be(sortedParVars)
  }
}

class ParSortMatcherSpec extends FlatSpec with Matchers {

  behavior of "Par"

  it should "sort so that smaller integers come first" in {
    val parGround =
      Par(exprs = List(GInt(2), GInt(1), GInt(-1), GInt(-2), GInt(0)))
    val sortedParGround: Par =
      Par(exprs = List(GInt(-2), GInt(-1), GInt(0), GInt(1), GInt(2)))
    val result = sort(parGround)
    result.term should be(sortedParGround)
  }

  it should "sort in order of boolean, int, string, uri" in {
    val parGround =
      Par(exprs = List(GUri("https://www.rchain.coop/"), GInt(47), GString("Hello"), GBool(true)))
    val sortedParGround =
      Par(exprs = List(GBool(true), GInt(47), GString("Hello"), GUri("https://www.rchain.coop/")))
    val result = sort(parGround)
    result.term should be(sortedParGround)
  }

  it should "sort and deduplicate sets insides" in {
    val parGround: Par =
      ParSet(
        Seq[Par](
          GInt(2),
          GInt(1),
          ParSet(Seq[Par](GInt(1), GInt(2))),
          ParSet(Seq[Par](GInt(1), GInt(1)))
        )
      )
    val sortedParGround: Par =
      ParSet(
        Seq[Par](
          GInt(1),
          GInt(2),
          ParSet(Seq[Par](GInt(1))),
          ParSet(Seq[Par](GInt(1), GInt(2)))
        )
      )
    val result = sort(parGround)
    result.term should be(sortedParGround)
  }

  it should "sort map insides by key and last write should win" in {
    val parGround: Par =
      ParMap(
        Seq[(Par, Par)](
          (GInt(2), ParSet(Seq[Par](GInt(2), GInt(1)))),
          (GInt(2), GInt(1)),
          (GInt(1), GInt(1))
        )
      )
    val sortedParGround: Par =
      ParMap(Seq[(Par, Par)]((GInt(1), GInt(1)), (GInt(2), GInt(1))))

    val result = sort(parGround)
    result.term should be(sortedParGround)
  }

  it should "use sorted subtrees and their scores in results" in {
    val s1  = Send(Par())
    val s2  = Send(Par(receives = List(Receive(List(), Par()))))
    val p21 = Par(sends = List(s2, s1))
    val p12 = Par(sends = List(s1, s2))

    assume(p12 == sort(p12).term)
    assume(p21 != sort(p21).term)
    assert(sort(p12).term == sort(p21).term)
    assert(sort(p12) == sort(p21)) //compare .score and all the other properties
  }

  it should "keep order when adding numbers" in {
    val parExpr: Par =
      EPlus(EPlus(GInt(1), GInt(3)), GInt(2))
    val result = sort(parExpr)
    result.term should be(parExpr)
  }

  it should "sort according to PEMDAS" in {
    val parExpr =
      Par(
        exprs = List(
          EMinus(GInt(4), GInt(3)),
          EDiv(GInt(1), GInt(5)),
          EPlus(GInt(1), GInt(3)),
          EMult(GInt(6), GInt(3))
        )
      )
    val sortedParExpr: Par =
      Par(
        exprs = List(
          EMult(GInt(6), GInt(3)),
          EDiv(GInt(1), GInt(5)),
          EPlus(GInt(1), GInt(3)),
          EMinus(GInt(4), GInt(3))
        )
      )
    val result = sort(parExpr)
    result.term should be(sortedParExpr)
  }

  it should "sort comparisons in order of LT, LTE, GT, GTE, EQ, NEQ" in {
    val parExpr =
      Par(
        exprs = List(
          EEq(GInt(4), GInt(3)),
          ENeq(GInt(1), GInt(5)),
          ELt(GInt(1), GInt(5)),
          EGt(GBool(false), GBool(true)),
          ELte(GInt(1), GInt(5)),
          EGte(GBool(false), GBool(true))
        )
      )
    val sortedParExpr =
      Par(
        exprs = List(
          ELt(GInt(1), GInt(5)),
          ELte(GInt(1), GInt(5)),
          EGt(GBool(false), GBool(true)),
          EGte(GBool(false), GBool(true)),
          EEq(GInt(4), GInt(3)),
          ENeq(GInt(1), GInt(5))
        )
      )
    val result = sort(parExpr)
    result.term should be(sortedParExpr)
  }

  it should "sort methods after other expressions" in {
    val parExpr =
      Par(
        exprs = List(
          EOr(EVar(BoundVar(0)), EVar(BoundVar(1))),
          EMethod("nth", EVar(BoundVar(2)), List(GInt(1)), locallyFree = BitSet(2)),
          EOr(EVar(BoundVar(3)), EVar(BoundVar(4)))
        )
      )
    val sortedParExpr =
      Par(
        exprs = List(
          EOr(EVar(BoundVar(0)), EVar(BoundVar(1))),
          EOr(EVar(BoundVar(3)), EVar(BoundVar(4))),
          EMethod("nth", EVar(BoundVar(2)), List(GInt(1)), locallyFree = BitSet(2))
        )
      )
    val result = sort(parExpr)
    result.term should be(sortedParExpr)
  }

  it should "sort methods based on methodName, target, and arguments" in {
    val parExpr =
      Par(
        exprs = List(
          EMethod("nth", EVar(BoundVar(2)), List(GInt(1)), locallyFree = BitSet(2)),
          EMethod("mth", EVar(BoundVar(2)), List(GInt(1)), locallyFree = BitSet(2)),
          EMethod("nth", EVar(BoundVar(2)), List(GInt(2), GInt(3)), locallyFree = BitSet(2)),
          EMethod("nth", EVar(BoundVar(2)), List(GInt(2)), locallyFree = BitSet(2))
        )
      )
    val sortedParExpr =
      Par(
        exprs = List(
          EMethod("mth", EVar(BoundVar(2)), List(GInt(1)), locallyFree = BitSet(2)),
          EMethod("nth", EVar(BoundVar(2)), List(GInt(1)), locallyFree = BitSet(2)),
          EMethod("nth", EVar(BoundVar(2)), List(GInt(2)), locallyFree = BitSet(2)),
          EMethod("nth", EVar(BoundVar(2)), List(GInt(2), GInt(3)), locallyFree = BitSet(2))
        )
      )
    val result = sort(parExpr)
    result.term should be(sortedParExpr)
  }

  it should "sort Sends based on their persistence, channel, data" in {
    val parExpr =
      Par(
        sends = List(
          Send(GInt(5), List(GInt(3)), false, BitSet()),
          Send(GInt(5), List(GInt(3)), true, BitSet()),
          Send(GInt(4), List(GInt(2)), false, BitSet()),
          Send(GInt(5), List(GInt(2)), false, BitSet())
        )
      )
    val sortedParExpr =
      Par(
        sends = List(
          Send(GInt(4), List(GInt(2)), false, BitSet()),
          Send(GInt(5), List(GInt(2)), false, BitSet()),
          Send(GInt(5), List(GInt(3)), false, BitSet()),
          Send(GInt(5), List(GInt(3)), true, BitSet())
        )
      )
    val result = sort(parExpr)
    result.term should be(sortedParExpr)
  }

  it should "sort Receives based on persistence, peek, channels, patterns and then body" in {
    val parExpr =
      Par(
        receives = List(
          Receive(
            List(ReceiveBind(List(GInt(1)), GInt(3))),
            Par(),
            false,
            false,
            0,
            BitSet()
          ),
          Receive(
            List(ReceiveBind(List(GInt(0)), GInt(3))),
            EVar(BoundVar(0)),
            false,
            false,
            0,
            BitSet()
          ),
          Receive(
            List(ReceiveBind(List(GInt(0)), GInt(3))),
            Par(),
            false,
            false,
            0,
            BitSet()
          ),
          Receive(
            List(ReceiveBind(List(GInt(0)), GInt(3))),
            Par(),
            true,
            false,
            0,
            BitSet()
          ),
          Receive(
            List(ReceiveBind(List(GInt(0)), GInt(3))),
            Par(),
            true,
            true,
            0,
            BitSet()
          ),
          Receive(
            List(ReceiveBind(List(GInt(100)), GInt(2))),
            Par(),
            false,
            false,
            0,
            BitSet()
          )
        )
      )
    val sortedParExpr =
      Par(
        receives = List(
          Receive(
            List(ReceiveBind(List(GInt(100)), GInt(2))),
            Par(),
            false,
            false,
            0,
            BitSet()
          ),
          Receive(
            List(ReceiveBind(List(GInt(0)), GInt(3))),
            Par(),
            false,
            false,
            0,
            BitSet()
          ),
          Receive(
            List(ReceiveBind(List(GInt(0)), GInt(3))),
            EVar(BoundVar(0)),
            false,
            false,
            0,
            BitSet()
          ),
          Receive(
            List(ReceiveBind(List(GInt(1)), GInt(3))),
            Par(),
            false,
            false,
            0,
            BitSet()
          ),
          Receive(List(ReceiveBind(List(GInt(0)), GInt(3))), Par(), true, false, 0, BitSet()),
          Receive(
            List(ReceiveBind(List(GInt(0)), GInt(3))),
            Par(),
            true,
            true,
            0,
            BitSet()
          )
        )
      )
    val result = sort(parExpr)
    result.term should be(sortedParExpr)
  }

  it should "sort Match based on their value and then cases" in {
    val parMatch =
      Par(
        matches = List(
          Match(GInt(5), List(MatchCase(GInt(5), GInt(5)), MatchCase(GInt(4), GInt(4))), BitSet()),
          Match(
            GBool(true),
            List(MatchCase(GInt(5), GInt(5)), MatchCase(GInt(4), GInt(4))),
            BitSet()
          ),
          Match(
            GBool(true),
            List(MatchCase(GInt(4), GInt(4)), MatchCase(GInt(3), GInt(3))),
            BitSet()
          )
        )
      )
    val sortedParMatch =
      Par(
        matches = List(
          Match(
            GBool(true),
            List(MatchCase(GInt(4), GInt(4)), MatchCase(GInt(3), GInt(3))),
            BitSet()
          ),
          Match(
            GBool(true),
            List(MatchCase(GInt(5), GInt(5)), MatchCase(GInt(4), GInt(4))),
            BitSet()
          ),
          Match(GInt(5), List(MatchCase(GInt(5), GInt(5)), MatchCase(GInt(4), GInt(4))), BitSet())
        )
      )
    val result = sort(parMatch)
    result.term should be(sortedParMatch)
  }

  it should "sort News based on bindCount, uri's and then body" in {
    val parNew =
      Par(
        news = List(
          New(bindCount = 2, p = Par()),
          New(
            bindCount = 2,
            uri = Vector("rho:io:stderr"),
            p = Par()
          ),
          New(bindCount = 2, uri = Vector("rho:io:stdout"), p = Par()),
          New(bindCount = 1, p = Par()),
          New(bindCount = 2, uri = Vector("rho:io:stdout"), p = GInt(7))
        )
      )
    val sortedParNew =
      Par(
        news = List(
          New(bindCount = 1, p = Par()),
          New(bindCount = 2, p = Par()),
          New(
            bindCount = 2,
            uri = Vector("rho:io:stderr"),
            p = Par()
          ),
          New(bindCount = 2, uri = Vector("rho:io:stdout"), p = Par()),
          New(bindCount = 2, uri = Vector("rho:io:stdout"), p = GInt(7))
        )
      )
    val result = sort(parNew)
    result.term should be(sortedParNew)
  }

  it should "sort uris in News" in {
    val parNew =
      Par(
        news = List(
          New(bindCount = 1, uri = Vector("rho:io:stdout", "rho:io:stderr"), p = Par())
        )
      )
    val sortedParNew =
      Par(
        news = List(
          New(bindCount = 1, uri = Vector("rho:io:stderr", "rho:io:stdout"), p = Par())
        )
      )
    val result = sort(parNew)
    result.term should be(sortedParNew)
  }

  it should "sort EVars based on their type and then levels" in {
    val parGround =
      Par(exprs = List(EVar(FreeVar(2)), EVar(FreeVar(1)), EVar(BoundVar(2)), EVar(BoundVar(1))))
    val sortedParGround =
      Par(exprs = List(EVar(BoundVar(1)), EVar(BoundVar(2)), EVar(FreeVar(1)), EVar(FreeVar(2))))
    val result = sort(parGround)
    result.term should be(sortedParGround)
  }

  it should "sort exprs in order of ground, vars, arithmetic, comparisons, logical" in {
    val parExpr =
      Par(
        exprs = List(
          EEq(GInt(4), GInt(3)),
          EDiv(GInt(1), GInt(5)),
          EVar(BoundVar(1)),
          EOr(GBool(false), GBool(true)),
          GInt(1)
        )
      )
    val sortedParExpr =
      Par(
        exprs = List(
          GInt(1),
          EVar(BoundVar(1)),
          EDiv(GInt(1), GInt(5)),
          EEq(GInt(4), GInt(3)),
          EOr(GBool(false), GBool(true))
        )
      )
    val result = sort(parExpr)
    result.term should be(sortedParExpr)
  }

  it should "sort expressions inside bundle" in {
    val parExpr =
      Par(
        exprs = List(
          EEq(GInt(4), GInt(3)),
          EDiv(GInt(1), GInt(5)),
          EVar(BoundVar(1)),
          EOr(GBool(false), GBool(true)),
          GInt(1)
        )
      )
    val sortedParExpr: Par =
      Par(
        exprs = List(
          GInt(1),
          EVar(BoundVar(1)),
          EDiv(GInt(1), GInt(5)),
          EEq(GInt(4), GInt(3)),
          EOr(GBool(false), GBool(true))
        )
      )

    val bundle = Bundle(parExpr)
    val result = sort(bundle)
    result.term should be(Bundle(sortedParExpr))
  }

  it should "sort expressions in nested bundles preserving polarities" in {
    val parExpr =
      Par(
        exprs = List(
          EEq(GInt(4), GInt(3)),
          EDiv(GInt(1), GInt(5)),
          EVar(BoundVar(1)),
          EOr(GBool(false), GBool(true)),
          GInt(1)
        )
      )
    val sortedParExpr: Par =
      Par(
        exprs = List(
          GInt(1),
          EVar(BoundVar(1)),
          EDiv(GInt(1), GInt(5)),
          EEq(GInt(4), GInt(3)),
          EOr(GBool(false), GBool(true))
        )
      )

    val nestedBundle = Bundle(
      Bundle(
        Bundle(parExpr, writeFlag = true, readFlag = false),
        writeFlag = false,
        readFlag = true
      )
    )
    val result = sort(nestedBundle)
    result.term should be(
      Bundle(
        Bundle(
          Bundle(sortedParExpr, writeFlag = true, readFlag = false),
          writeFlag = false,
          readFlag = true
        )
      )
    )
  }

  it should "sort logical connectives in \"not\", \"and\", \"or\" order" in {
    val parExpr =
      Par(
        connectives = List(
          Connective(
            ConnAndBody(
              ConnectiveBody(
                List(EVar(FreeVar(0)), Send(EVar(FreeVar(1)), List(EVar(FreeVar(2))), false))
              )
            )
          ),
          Connective(
            ConnOrBody(
              ConnectiveBody(
                List(
                  New(1, EVar(Wildcard(Var.WildcardMsg()))),
                  New(2, EVar(Wildcard(Var.WildcardMsg())))
                )
              )
            )
          ),
          Connective(ConnNotBody(Par()))
        ),
        connectiveUsed = true
      )
    val sortedParExpr: Par =
      Par(
        connectives = List(
          Connective(ConnNotBody(Par())),
          Connective(
            ConnAndBody(
              ConnectiveBody(
                List(EVar(FreeVar(0)), Send(EVar(FreeVar(1)), List(EVar(FreeVar(2))), false))
              )
            )
          ),
          Connective(
            ConnOrBody(
              ConnectiveBody(
                List(
                  New(1, EVar(Wildcard(Var.WildcardMsg()))),
                  New(2, EVar(Wildcard(Var.WildcardMsg())))
                )
              )
            )
          )
        ),
        connectiveUsed = true
      )
    val result = sort(parExpr)
    result.term should be(sortedParExpr)
  }

  it should "sort logical connectives in \"varref\", \"bool\", \"int\", \"string\", \"uri\", \"bytearray\" order" in {
    val parExpr =
      Par(
        connectives = List(
          Connective(ConnByteArray(true)),
          Connective(ConnUri(true)),
          Connective(ConnString(true)),
          Connective(ConnInt(true)),
          Connective(ConnBool(true)),
          Connective(
            VarRefBody(VarRef())
          )
        ),
        connectiveUsed = true
      )
    val sortedParExpr: Par =
      Par(
        connectives = List(
          Connective(
            VarRefBody(VarRef())
          ),
          Connective(ConnBool(true)),
          Connective(ConnInt(true)),
          Connective(ConnString(true)),
          Connective(ConnUri(true)),
          Connective(ConnByteArray(true))
        ),
        connectiveUsed = true
      )
    val result = sort(parExpr)
    result.term should be(sortedParExpr)
  }

  it should "sort based on the connectiveUsed flag" in {
    val expr = Expr(
      EMapBody(
        ParMap(
          SortedParMap(
            Map(
              Par() -> Par(
                exprs = Seq()
              ),
              Par(connectiveUsed = true) -> Par(exprs = Seq())
            )
          )
        )
      )
    )
    val result = sort(expr)
    result.term should be(expr)
  }
}
