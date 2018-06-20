package coop.rchain.models.rholang.sort

import coop.rchain.models.Channel
import coop.rchain.models.Channel.ChannelInstance.{ChanVar, Quote}
import coop.rchain.models.rholang.implicits._

object ChannelSortMatcher {
  def sortMatch(channel: Channel): ScoredTerm[Channel] =
    channel.channelInstance match {
      case Quote(par) =>
        val sortedPar = ParSortMatcher.sortMatch(par)
        ScoredTerm(Quote(sortedPar.term.get), Node(Score.QUOTE, sortedPar.score))
      case ChanVar(par) =>
        val sortedVar = VarSortMatcher.sortMatch(par)
        ScoredTerm(ChanVar(sortedVar.term), Node(Score.CHAN_VAR, sortedVar.score))
    }
}
