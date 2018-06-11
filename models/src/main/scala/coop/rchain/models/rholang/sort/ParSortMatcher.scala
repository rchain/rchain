package coop.rchain.models.rholang.sort

import coop.rchain.models.Par
import cats.implicits._

object ParSortMatcher {
  def sortMatch(parOption: Option[Par]): Either[Throwable, ScoredTerm[Par]] =
    parOption match {
      case Some(p) =>
        for {
          sends <- p.sends.toList.traverse(s => SendSortMatcher.sortMatch(s)).map(_.sorted)
          receives <- p.receives.toList
                       .traverse(r => ReceiveSortMatcher.sortMatch(r))
                       .map(_.sorted)
          exprs   <- p.exprs.toList.traverse(e => ExprSortMatcher.sortMatch(e)).map(_.sorted)
          news    <- p.news.toList.traverse(n => NewSortMatcher.sortMatch(n)).map(_.sorted)
          matches <- p.matches.toList.traverse(m => MatchSortMatcher.sortMatch(m)).map(_.sorted)
          bundles <- p.bundles.toList.traverse(b => BundleSortMatcher.sortMatch(b)).map(_.sorted)
          connectives <- p.connectives.toList
                          .traverse(c => ConnectiveSortMatcher.sortMatch(c))
                          .map(_.sorted)
          ids = p.ids.map(g => ScoredTerm(g, Node(Score.PRIVATE, Leaf(g.id)))).sorted
          sortedPar = Par(
            sends = sends.map(_.term),
            receives = receives.map(_.term),
            exprs = exprs.map(_.term),
            news = news.map(_.term),
            matches = matches.map(_.term),
            bundles = bundles.map(_.term),
            connectives = connectives.map(_.term),
            ids = ids.map(_.term),
            locallyFree = p.locallyFree,
            connectiveUsed = p.connectiveUsed
          )
          parScore = Node(
            Score.PAR,
            sends.map(_.score) ++ receives.map(_.score) ++
              exprs.map(_.score) ++ news.map(_.score) ++
              matches.map(_.score) ++ bundles.map(_.score) ++ ids.map(_.score) ++ connectives.map(
              _.score): _*
          )

        } yield ScoredTerm(sortedPar, parScore)

      case None => Left(new IllegalArgumentException("ParSortMatcher was passed None"))
    }
}
