package coop.rchain.models.rholang.sorter

import cats.effect.Sync
import cats.syntax.all._
import cats.instances.list._
import coop.rchain.models.{New, Par}

private[sorter] object NewSortMatcher extends Sortable[New] {
  def sortMatch[F[_]: Sync](n: New): F[ScoredTerm[New]] =
    for {
      sortedPar      <- Sortable.sortMatch(n.p)
      sortedUri      = n.uri.sorted
      uriScore       = if (sortedUri.nonEmpty) sortedUri.map(Leaf.apply) else List(Leaf(Score.ABSENT))
      injectionsList = n.injections.toList
      injectionsScore <- if (injectionsList.nonEmpty)
                          injectionsList.traverse {
                            case (k, v) =>
                              Sortable[Par]
                                .sortMatch[F](v)
                                .map(
                                  scoredTerm => Node(k, scoredTerm.score)
                                )
                          } else List(Leaf(Score.ABSENT)).pure[F]
    } yield ScoredTerm(
      New(
        bindCount = n.bindCount,
        p = sortedPar.term,
        uri = sortedUri,
        injections = n.injections,
        locallyFree = n.locallyFree
      ),
      new Node(
        Leaf(Score.NEW) +: ((Leaf(n.bindCount.toLong) +: uriScore) ++ injectionsScore :+ sortedPar.score)
      )
    )
}
