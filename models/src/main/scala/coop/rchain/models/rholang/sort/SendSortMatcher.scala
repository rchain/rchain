package coop.rchain.models.rholang.sort

import coop.rchain.models.Send
import cats.implicits._
import coop.rchain.models.rholang.implicits._

object SendSortMatcher {
  def sortMatch(s: Send): Either[Throwable, ScoredTerm[Send]] =
    for {
      sortedChan <- ChannelSortMatcher.sortMatch(s.chan)
      sortedData <- s.data.toList.traverse(ParSortMatcher.sortMatch(_))
      sortedSend = Send(
        chan = sortedChan.term,
        data = sortedData.map(_.term.get),
        persistent = s.persistent,
        locallyFree = s.locallyFree,
        connectiveUsed = s.connectiveUsed
      )
      persistentScore = if (s.persistent) 1 else 0
      sendScore = Node(
        Score.SEND,
        Seq(Leaf(persistentScore)) ++ Seq(sortedChan.score) ++ sortedData.map(_.score): _*)
    } yield ScoredTerm(sortedSend, sendScore)
}
