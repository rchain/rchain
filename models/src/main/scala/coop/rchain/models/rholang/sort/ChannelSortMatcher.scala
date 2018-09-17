package coop.rchain.models.rholang.sort

import cats.effect.Sync
import cats.implicits._
import coop.rchain.models.{Channel, Expr}
import coop.rchain.models.Channel.ChannelInstance.{ChanVar, Empty, Quote}
import coop.rchain.models.rholang.implicits._

private[sort] object ChannelSortMatcher extends Sortable[Channel] {
  def sortMatch[F[_]: Sync](channel: Channel): F[ScoredTerm[Channel]] =
    channel.channelInstance match {
      case Quote(par) =>
        Sortable.sortMatch(par).map { sortedPar =>
          ScoredTerm(Quote(sortedPar.term.get), Node(Score.QUOTE, sortedPar.score))
        }
      case ChanVar(par) =>
        Sortable.sortMatch(par).map { sortedVar =>
          ScoredTerm(ChanVar(sortedVar.term), Node(Score.CHAN_VAR, sortedVar.score))
        }
      case Empty =>
        Sync[F].pure(ScoredTerm(Empty, Leaf(Score.ABSENT)))
    }
}
