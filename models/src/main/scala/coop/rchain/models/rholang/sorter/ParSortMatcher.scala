package coop.rchain.models.rholang.sorter

import cats.effect.Sync
import coop.rchain.models._
import cats.implicits._

private[sorter] object ParSortMatcher extends Sortable[Par] {
  def sortMatch[F[_]: Sync](par: Par): F[ScoredTerm[Par]] =
    for {
      sends    <- par.sends.toList.traverse(Sortable[Send].sortMatch[F]).map(_.sorted)
      receives <- par.receives.toList.traverse(Sortable[Receive].sortMatch[F]).map(_.sorted)
      exprs    <- par.exprs.toList.traverse(Sortable[Expr].sortMatch[F]).map(_.sorted)
      news     <- par.news.toList.traverse(Sortable[New].sortMatch[F]).map(_.sorted)
      matches  <- par.matches.toList.traverse(Sortable[Match].sortMatch[F]).map(_.sorted)
      bundles  <- par.bundles.toList.traverse(Sortable[Bundle].sortMatch[F]).map(_.sorted)
      connectives <- par.connectives.toList
                      .traverse(Sortable[Connective].sortMatch[F])
                      .map(_.sorted)
      ids = par.ids.map(g => ScoredTerm(g, Node(Score.PRIVATE, Leaf(g.id)))).sorted
      sortedPar = Par(
        sends = sends.map(_.term),
        receives = receives.map(_.term),
        exprs = exprs.map(_.term),
        news = news.map(_.term),
        matches = matches.map(_.term),
        bundles = bundles.map(_.term),
        connectives = connectives.map(_.term),
        ids = ids.map(_.term),
        locallyFree = par.locallyFree,
        connectiveUsed = par.connectiveUsed
      )
      parScore = Node(
        Score.PAR,
        sends.map(_.score) ++
          receives.map(_.score) ++
          exprs.map(_.score) ++
          news.map(_.score) ++
          matches.map(_.score) ++
          bundles.map(_.score) ++
          connectives.map(_.score) ++
          ids.map(_.score): _*
      )
    } yield ScoredTerm(sortedPar, parScore)
}
