package coop.rchain.models.rholang.sort

import coop.rchain.models.{Match, MatchCase}
import cats.implicits._
import coop.rchain.models.rholang.implicits._

object MatchSortMatcher extends Sortable[Match] {
  def sortMatch(m: Match): ScoredTerm[Match] = {
    def sortCase(matchCase: MatchCase): ScoredTerm[MatchCase] = {
      val sortedPattern = ParSortMatcher.sortMatch(matchCase.pattern)
      val sortedBody    = ParSortMatcher.sortMatch(matchCase.source)
      ScoredTerm(MatchCase(sortedPattern.term, sortedBody.term, matchCase.freeCount),
                 Node(Seq(sortedPattern.score) ++ Seq(sortedBody.score)))
    }

    val sortedValue = ParSortMatcher.sortMatch(m.target)
    val scoredCases = m.cases.toList.map(sortCase)
    ScoredTerm(Match(sortedValue.term, scoredCases.map(_.term), m.locallyFree, m.connectiveUsed),
               Node(Score.MATCH, Seq(sortedValue.score) ++ scoredCases.map(_.score): _*))
  }
}
