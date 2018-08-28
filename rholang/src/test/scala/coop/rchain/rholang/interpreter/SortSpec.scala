package coop.rchain.rholang.interpreter
import coop.rchain.models.Var.VarInstance.FreeVar
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.rholang.sort.Sortable
import org.scalatest.{FlatSpec, Matchers}
import coop.rchain.models.rholang.sort.ScoredTerm

class SortSpec extends FlatSpec with Matchers {

  "GroundSortMatcher" should "discern sets with and without remainder" in {
    assertOrder[Expr](
      ParSet(Seq.empty),
      ParSet(Seq.empty, remainder = Some(FreeVar(0)))
    )
  }

  private def assertOrder[T: Sortable](smaller: T, bigger: T): Any = {
    val left: ScoredTerm[T]  = Sortable[T].sortMatch(smaller)
    val right: ScoredTerm[T] = Sortable[T].sortMatch(bigger)
    assert(Ordering[ScoredTerm[T]].compare(left, right) < 0)
  }
}
