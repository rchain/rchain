package coop.rchain.models.rholang.sort

import cats.effect.Sync
import coop.rchain.models.Par
import cats.implicits._

private[sort] object ParSortMatcher extends Sortable[Par] {
  def sortMatch[F[_]: Sync](par: Par): F[ScoredTerm[Par]] =
    for {
      sends       <- par.sends.toList.map(s => Sortable.sortMatch(s)).sequence
      receives    <- par.receives.toList.map(r => Sortable.sortMatch(r)).sequence
      exprs       <- par.exprs.toList.map(e => Sortable.sortMatch(e)).sequence
      news        <- par.news.toList.map(n => Sortable.sortMatch(n)).sequence
      matches     <- par.matches.toList.map(m => Sortable.sortMatch(m)).sequence
      bundles     <- par.bundles.toList.map(b => Sortable.sortMatch(b)).sequence
      connectives <- par.connectives.toList.map(c => Sortable.sortMatch(c)).sequence
      ids         = par.ids.map(g => ScoredTerm(g, Node(Score.PRIVATE, Leaf(g.id)))).sorted
      sortedPar = Par(
        sends = sends.sorted.map(_.term),
        receives = receives.sorted.map(_.term),
        exprs = exprs.sorted.map(_.term),
        news = news.sorted.map(_.term),
        matches = matches.sorted.map(_.term),
        bundles = bundles.sorted.map(_.term),
        connectives = connectives.sorted.map(_.term),
        ids = ids.map(_.term),
        locallyFree = par.locallyFree,
        connectiveUsed = par.connectiveUsed
      )
      parScore = Node(
        Score.PAR,
        sends.map(_.score) ++ receives.map(_.score) ++ exprs.map(_.score) ++ news
          .map(_.score) ++ matches.map(_.score) ++ bundles.map(_.score) ++ ids
          .map(_.score) ++ connectives.map(_.score): _*
      )
    } yield ScoredTerm(sortedPar, parScore)
}
