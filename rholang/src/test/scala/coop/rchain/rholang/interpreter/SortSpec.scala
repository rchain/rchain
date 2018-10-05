package coop.rchain.rholang.interpreter
import coop.rchain.models.Var.VarInstance.FreeVar
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.rholang.sorter.Sortable
import org.scalatest.{FlatSpec, Matchers}
import coop.rchain.models.rholang.sorter.ScoredTerm
import monix.eval.Coeval

import scala.collection.immutable.BitSet

class SortSpec extends FlatSpec with Matchers {

  "GroundSortMatcher" should "discern sets with and without remainder" in {
    assertOrder[Expr](
      ParSet(Seq.empty),
      ParSet(Seq.empty, remainder = Some(FreeVar(0)))
    )
  }

  it should "discern maps with and without remainder" in {
    assertOrder[Expr](
      ParMap(Seq.empty),
      ParMap(
        Seq.empty,
        connectiveUsed = false,
        locallyFree = BitSet(),
        remainder = Some(Var(FreeVar(0)))
      )
    )
  }

  private def assertOrder[T: Sortable](smaller: T, bigger: T): Any = {
    val left: ScoredTerm[T]  = checkSortingAndScore(smaller)
    val right: ScoredTerm[T] = checkSortingAndScore(bigger)
    assert(Ordering[ScoredTerm[T]].compare(left, right) < 0)
  }

  def checkSortingAndScore[T: Sortable](term: T): ScoredTerm[T] = {
    val scored: ScoredTerm[T] = Sortable[T].sortMatch[Coeval](term).value
    assert(scored.term == term, "Either input term not sorted or sorting returned wrong results")
    scored
  }
}
