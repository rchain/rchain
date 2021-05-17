package coop.rchain.models.rholang.sorter

import cats.effect.Sync
import coop.rchain.models._
import cats.implicits._

private[sorter] object ParSortMatcher extends Sortable[Par] {
  def sortMatch[F[_]: Sync](par: Par): F[ScoredTerm[Par]] =
    for {
      sends    <- par.sends.traverse(Sortable[Send].sortMatch[F]).map(_.sorted)
      receives <- par.receives.traverse(Sortable[Receive].sortMatch[F]).map(_.sorted)
      exprs    <- par.exprs.traverse(Sortable[Expr].sortMatch[F]).map(_.sorted)
      news     <- par.news.traverse(Sortable[New].sortMatch[F]).map(_.sorted)
      matches  <- par.matches.traverse(Sortable[Match].sortMatch[F]).map(_.sorted)
      bundles  <- par.bundles.traverse(Sortable[Bundle].sortMatch[F]).map(_.sorted)
      connectives <- par.connectives
                      .traverse(Sortable[Connective].sortMatch[F])
                      .map(_.sorted)
      unforgeables <- par.unforgeables
                       .traverse(Sortable[GUnforgeable].sortMatch[F])
                       .map(_.sorted)
      sortedPar = Par(
        sends = sends.map(_.term),
        receives = receives.map(_.term),
        exprs = exprs.map(_.term),
        news = news.map(_.term),
        matches = matches.map(_.term),
        bundles = bundles.map(_.term),
        connectives = connectives.map(_.term),
        unforgeables = unforgeables.map(_.term),
        locallyFree = par.locallyFree,
        connectiveUsed = par.connectiveUsed
      )
      connectiveUsedScore = if (par.connectiveUsed) 1L else 0L
      parScore = Node(
        Score.PAR,
        sends.map(_.score) ++
          receives.map(_.score) ++
          exprs.map(_.score) ++
          news.map(_.score) ++
          matches.map(_.score) ++
          bundles.map(_.score) ++
          connectives.map(_.score) ++
          unforgeables.map(_.score) ++
          Seq(Leaf(connectiveUsedScore)): _*
      )
    } yield ScoredTerm(sortedPar, parScore)
}
