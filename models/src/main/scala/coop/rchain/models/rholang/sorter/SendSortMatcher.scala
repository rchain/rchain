package coop.rchain.models.rholang.sorter

import cats.effect.Sync
import coop.rchain.models.{Par, Send}
import coop.rchain.models.rholang.implicits._
import cats.implicits._

private[sorter] object SendSortMatcher extends Sortable[Send] {
  def sortMatch[F[_]: Sync](s: Send): F[ScoredTerm[Send]] =
    for {
      sortedChan <- Sortable.sortMatch(s.chan)
      sortedData <- s.data.toList.traverse(Sortable[Par].sortMatch[F])
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
        Seq(Leaf(persistentScore)) ++ Seq(sortedChan.score) ++ sortedData.map(_.score): _*
      )
    } yield ScoredTerm(sortedSend, sendScore)
}
