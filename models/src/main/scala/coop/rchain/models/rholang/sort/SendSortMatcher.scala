package coop.rchain.models.rholang.sort

import coop.rchain.models.Send
import cats.implicits._
import coop.rchain.models.rholang.implicits._

object SendSortMatcher extends Sortable[Send] {
  def sortMatch(s: Send): ScoredTerm[Send] = {
    val sortedChan = ChannelSortMatcher.sortMatch(s.chan)
    val sortedData = s.data.toList.map(ParSortMatcher.sortMatch(_))
    val sortedSend = Send(
      chan = sortedChan.term,
      data = sortedData.map(_.term.get),
      persistent = s.persistent,
      locallyFree = s.locallyFree,
      connectiveUsed = s.connectiveUsed
    )
    val persistentScore = if (s.persistent) 1 else 0
    val sendScore = Node(
      Score.SEND,
      Seq(Leaf(persistentScore)) ++ Seq(sortedChan.score) ++ sortedData.map(_.score): _*)
    ScoredTerm(sortedSend, sendScore)
  }
}
