package coop.rchain.models.rholang.sorter

import cats.effect.Sync
import cats.implicits._
import coop.rchain.models.New

private[sorter] object NewSortMatcher extends Sortable[New] {
  def sortMatch[F[_]: Sync](n: New): F[ScoredTerm[New]] =
    Sortable
      .sortMatch(n.p)
      .map {
        val uriScore =
          if (n.uri.nonEmpty)
            n.uri.map(Leaf.apply)
          else
            List(Leaf(Score.ABSENT))

        sortedPar =>
          ScoredTerm(
            New(
              bindCount = n.bindCount,
              p = sortedPar.term,
              uri = n.uri,
              injections = n.injections,
              locallyFree = n.locallyFree
            ),
            new Node(
              Leaf(Score.NEW) +: (Leaf(n.bindCount.toLong) +: uriScore :+ sortedPar.score)
            )
          )
      }
}
