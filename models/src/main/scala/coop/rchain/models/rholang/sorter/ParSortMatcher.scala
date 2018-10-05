package coop.rchain.models.rholang.sorter

import cats.effect.Sync
import coop.rchain.models._
import cats.implicits._

private[sorter] object ParSortMatcher extends Sortable[Par] {
  def sortMatch[F[_]: Sync](par: Par): F[ScoredTerm[Par]] =
    for {
      sends       <- par.sends.toList.traverse(Sortable[Send].sortMatch[F])
      receives    <- par.receives.toList.traverse(Sortable[Receive].sortMatch[F])
      exprs       <- par.exprs.toList.traverse(Sortable[Expr].sortMatch[F])
      news        <- par.news.toList.traverse(Sortable[New].sortMatch[F])
      matches     <- par.matches.toList.traverse(Sortable[Match].sortMatch[F])
      bundles     <- par.bundles.toList.traverse(Sortable[Bundle].sortMatch[F])
      connectives <- par.connectives.toList.traverse(Sortable[Connective].sortMatch[F])
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
