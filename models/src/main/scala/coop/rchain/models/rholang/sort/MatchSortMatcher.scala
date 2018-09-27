package coop.rchain.models.rholang.sort

import cats.effect.Sync
import coop.rchain.models.{Expr, Match, MatchCase}
import cats.implicits._

private[sort] object MatchSortMatcher extends Sortable[Match] {
  def sortMatch[F[_]: Sync](m: Match): F[ScoredTerm[Match]] = {

    def sortCase(matchCase: MatchCase): F[ScoredTerm[MatchCase]] =
      for {
        sortedPattern <- Sortable.sortMatch(matchCase.pattern)
        sortedBody    <- Sortable.sortMatch(matchCase.source)
      } yield
        ScoredTerm(
          MatchCase(sortedPattern.term, sortedBody.term, matchCase.freeCount),
          Node(Seq(sortedPattern.score) ++ Seq(sortedBody.score))
        )
    for {
      sortedValue <- Sortable.sortMatch(m.target)
      scoredCases <- m.cases.toList.traverse(sortCase)
    } yield
      ScoredTerm(
        Match(sortedValue.term, scoredCases.map(_.term), m.locallyFree, m.connectiveUsed),
        Node(Score.MATCH, Seq(sortedValue.score) ++ scoredCases.map(_.score): _*)
      )
  }
}
