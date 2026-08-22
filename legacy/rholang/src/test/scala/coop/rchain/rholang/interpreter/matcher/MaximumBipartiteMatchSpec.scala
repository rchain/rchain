package coop.rchain.rholang.interpreter.matcher
import cats.data.Writer
import cats.{Id, Monad}
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MaximumBipartiteMatchSpec extends AnyFlatSpec with Matchers {

  val edges = Map(
    //  A - 1
    //  |   |
    //  2   B
    "A" -> Seq(1, 2),
    "B" -> Seq(1),
    // Full bipartite graph K(3,3)
    "D" -> Seq(4, 5),
    "E" -> Seq(4, 6),
    "F" -> Seq(5, 6)
  )

  type Edge = (String, Int)

  val mf: (String, Int) => Id[Option[Edge]] =
    (x: String, y: Int) => edges.get(x).flatMap(_.find(_ == y).map(_ => x -> y))

  "MaximumBipartiteMatch" should "find a maximum match in a bipartite (sub)graph" in {
    assertMatch(mf, Seq(), Seq(), Some(Seq()))
    assertMatch(mf, Seq(), Seq(1), Some(Seq()))
    assertMatch(mf, Seq("A"), Seq(), None)
    assertMatch(mf, Seq("C"), Seq(), None)
    assertMatch(mf, Seq(), Seq(3), Some(Seq()))
    assertMatch(mf, Seq("C"), Seq(3), None)
    assertMatch(mf, Seq("B"), Seq(3), None)
    assertMatch(mf, Seq("C"), Seq(2), None)
    assertMatch(mf, Seq("A"), Seq(1), Some(Seq(edge("A", 1))))
    assertMatch(mf, Seq("A"), Seq(2), Some(Seq(edge("A", 2))))
    assertMatch(mf, Seq("A"), Seq(1, 2), Some(Seq(edge("A", 1))))
    // format: off
    assertMatch(mf, Seq("A", "B"), Seq(1, 2), Some(Seq(
      edge("B", 1),
      edge("A", 2)
    )))
    assertMatch(mf, Seq("A", "B"), Seq(1, 2, 3), Some(Seq(
      edge("B", 1),
      edge("A", 2)
    )))
    assertMatch(mf, Seq("A", "B", "C"), Seq(1, 2), None)
    assertMatch(mf, Seq("D", "E", "F"), Seq(2, 4, 5, 6), Some(Seq(
      edge("E", 4),
      edge("D", 5),
      edge("F", 6)
    )))
    // format: on
  }

  it should "discern equal terms and/or patterns" in {
    assertMatch(mf, Seq("A", "A"), Seq(1), None)
    // format: off
    assertMatch(mf, Seq("A"), Seq(1, 1), Some(Seq(
      edge("A", 1)
    )))
    assertMatch(mf, Seq("A", "A"), Seq(1, 1), Some(Seq(
      edge("A", 1),
      edge("A", 1)
    )))
    assertMatch(mf, Seq("A", "A"), Seq(1, 2), Some(Seq(
      edge("A", 1),
      edge("A", 2)
    )))
    assertMatch(mf, Seq("A", "B"), Seq(1, 1), Some(Seq(
      edge("B", 1),
      edge("A", 1)
    )))
    assertMatch(mf, Seq("A", "A"), Seq(2, 1), Some(Seq(
      edge("A", 2),
      edge("A", 1)
    )))
    assertMatch(mf, Seq("B", "A"), Seq(1, 1), Some(Seq(
      edge("A", 1),
      edge("B", 1)
    )))
    // format: on
  }

  it should "retain effects caused by the matchFunction" in {
    import cats.syntax.all._

    val effectfulMf: (String, Int) => Writer[List[Edge], Option[Edge]] =
      (x, y) => Writer.tell(List(x -> y)).map(_ => mf(x, y))

    val mbm                   = MaximumBipartiteMatch(effectfulMf)
    val (edgesTested, result) = mbm.findMatches(Seq("D", "E", "F"), Seq(4, 5, 6)).run

    assert(result.contains(Seq(edge("E", 4), edge("D", 5), edge("F", 6))))
    assert(edgesTested == Seq(("D", 4), ("E", 4), ("D", 5), ("F", 4), ("F", 5), ("D", 6), ("F", 6)))
  }

  def assertMatch[P, T, R, F[_]: Monad](
      matchFunction: (P, T) => F[Option[R]],
      left: Seq[P],
      right: Seq[T],
      expectedMatch: Option[Seq[(T, P, R)]]
  ): Assertion = {
    val mbm = MaximumBipartiteMatch(matchFunction)
    assert(mbm.findMatches(left, right) === expectedMatch)
  }

  private def edge[P, T](from: P, to: T): (T, P, (P, T)) =
    (to, from, (from, to))
}
