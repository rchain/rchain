package coop.rchain.models.rholang.sort

import coop.rchain.models.{Receive, ReceiveBind}

object ReceiveSortMatcher extends Sortable[Receive] {

  def sortBind(bind: ReceiveBind): ScoredTerm[ReceiveBind] = {
    val patterns       = bind.patterns
    val source         = bind.source
    val sortedPatterns = patterns.toList.map(channel => Sortable.sortMatch(channel))
    val sortedChannel  = Sortable.sortMatch(source)
    val sortedRemainder = bind.remainder match {
      case Some(bindRemainder) =>
        val scoredVar = Sortable.sortMatch(bindRemainder)
        ScoredTerm(Some(scoredVar.term), scoredVar.score)
      case None => ScoredTerm(None, Leaf(Score.ABSENT))
    }
    ScoredTerm(
      ReceiveBind(sortedPatterns.map(_.term), sortedChannel.term, bind.remainder, bind.freeCount),
      Node(Seq(sortedChannel.score) ++ sortedPatterns.map(_.score) ++ Seq(sortedRemainder.score))
    )
  }

  // The order of the binds must already be presorted by the time this is called.
  // This function will then sort the insides of the preordered binds.
  def sortMatch(r: Receive): ScoredTerm[Receive] = {
    val sortedBinds     = r.binds.toList.map(bind => sortBind(bind))
    val persistentScore = if (r.persistent) 1 else 0
    val sortedBody      = Sortable.sortMatch(r.body)
    ScoredTerm(
      Receive(sortedBinds.map(_.term),
              sortedBody.term,
              r.persistent,
              r.bindCount,
              r.locallyFree,
              r.connectiveUsed),
      Node(Score.RECEIVE,
           Seq(Leaf(persistentScore)) ++ sortedBinds.map(_.score) ++ Seq(sortedBody.score): _*)
    )
  }
}
