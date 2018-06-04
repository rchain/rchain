package coop.rchain.models.rholang.sort

import coop.rchain.models.{Match, MatchCase}
import cats.implicits._
import coop.rchain.models.rholang.implicits._

object MatchSortMatcher {
  def sortMatch(m: Match): Either[Throwable, ScoredTerm[Match]] = {
    def sortCase(matchCase: MatchCase): Either[Throwable, ScoredTerm[MatchCase]] =
      for {
        sortedPattern <- ParSortMatcher.sortMatch(matchCase.pattern)
        sortedBody    <- ParSortMatcher.sortMatch(matchCase.source)
      } yield
        ScoredTerm(MatchCase(sortedPattern.term, sortedBody.term, matchCase.freeCount),
                   Node(Seq(sortedPattern.score) ++ Seq(sortedBody.score)))

    for {
      sortedValue <- ParSortMatcher.sortMatch(m.target)
      scoredCases <- m.cases.toList.traverse(sortCase)
    } yield
      ScoredTerm(Match(sortedValue.term, scoredCases.map(_.term), m.locallyFree, m.connectiveUsed),
                 Node(Score.MATCH, Seq(sortedValue.score) ++ scoredCases.map(_.score): _*))
  }
}
