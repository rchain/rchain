package coop.rchain.models.rholang.sort

import coop.rchain.models.{Match, MatchCase}

private[sort] object MatchSortMatcher extends Sortable[Match] {
  def sortMatch(m: Match): ScoredTerm[Match] = {
    def sortCase(matchCase: MatchCase): ScoredTerm[MatchCase] = {
      val sortedPattern = Sortable.sortMatch(matchCase.pattern)
      val sortedBody    = Sortable.sortMatch(matchCase.source)
      ScoredTerm(MatchCase(sortedPattern.term, sortedBody.term, matchCase.freeCount),
                 Node(Seq(sortedPattern.score) ++ Seq(sortedBody.score)))
    }

    val sortedValue = Sortable.sortMatch(m.target)
    val scoredCases = m.cases.toList.map(sortCase)
    ScoredTerm(Match(sortedValue.term, scoredCases.map(_.term), m.locallyFree, m.connectiveUsed),
               Node(Score.MATCH, Seq(sortedValue.score) ++ scoredCases.map(_.score): _*))
  }
}
