package coop.rchain.models.rholang.sort

import coop.rchain.models.Par
import coop.rchain.models.rholang.implicits._

object ordering {
  implicit val parOrdering: Ordering[Par] = new Ordering[Par] {
    override def compare(x: Par, y: Par): Int =
      (for {
        xSorted <- ParSortMatcher.sortMatch(x)
        ySorted <- ParSortMatcher.sortMatch(y)
      } yield ScoredTerm.ordering.compare(xSorted, ySorted))
        .fold(th => throw new RuntimeException(th.getMessage), score => score)
  }
}
