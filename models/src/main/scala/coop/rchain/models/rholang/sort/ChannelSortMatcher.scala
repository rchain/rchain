package coop.rchain.models.rholang.sort

import coop.rchain.models.Channel
import coop.rchain.models.Channel.ChannelInstance.{ChanVar, Empty, Quote}
import coop.rchain.models.rholang.implicits._

private[sort] object ChannelSortMatcher extends Sortable[Channel] {
  def sortMatch(channel: Channel): ScoredTerm[Channel] =
    channel.channelInstance match {
      case Quote(par) =>
        val sortedPar = Sortable.sortMatch(par)
        ScoredTerm(Quote(sortedPar.term.get), Node(Score.QUOTE, sortedPar.score))
      case ChanVar(par) =>
        val sortedVar = Sortable.sortMatch(par)
        ScoredTerm(ChanVar(sortedVar.term), Node(Score.CHAN_VAR, sortedVar.score))
      case Empty =>
        ScoredTerm(Empty, Leaf(Score.ABSENT))

    }
}
