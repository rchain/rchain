package coop.rchain.models.rholang.sort

import coop.rchain.models.Channel
import coop.rchain.models.Channel.ChannelInstance.{ChanVar, Quote}
import coop.rchain.models.rholang.implicits._

object ChannelSortMatcher {
  def sortMatch(channelOption: Option[Channel]): Either[Throwable, ScoredTerm[Channel]] =
    channelOption match {
      case Some(c) =>
        c.channelInstance match {
          case Quote(par) =>
            ParSortMatcher
              .sortMatch(par)
              .map(sortedPar =>
                ScoredTerm(Quote(sortedPar.term.get), Node(Score.QUOTE, sortedPar.score)))
          case ChanVar(par) =>
            VarSortMatcher
              .sortMatch(par)
              .map(sortedVar =>
                ScoredTerm(ChanVar(sortedVar.term), Node(Score.CHAN_VAR, sortedVar.score)))
        }
      case None => Left(new IllegalArgumentException("ChannelSortMatcher was passed None"))
    }

}
