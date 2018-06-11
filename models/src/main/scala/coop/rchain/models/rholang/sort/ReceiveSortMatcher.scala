package coop.rchain.models.rholang.sort

import coop.rchain.models.{Receive, ReceiveBind}
import cats.implicits._
import coop.rchain.models.rholang.implicits._

object ReceiveSortMatcher {
  def sortBind(bind: ReceiveBind): Either[Throwable, ScoredTerm[ReceiveBind]] = {
    val patterns = bind.patterns
    val source   = bind.source
    for {
      sortedPatterns <- patterns.toList.traverse(channel => ChannelSortMatcher.sortMatch(channel))
      sortedChannel  <- ChannelSortMatcher.sortMatch(source)
      sortedRemainder <- bind.remainder match {
                          case s @ Some(_) => {
                            VarSortMatcher
                              .sortMatch(s)
                              .map(scoredVar => ScoredTerm(Some(scoredVar.term), scoredVar.score))
                          }
                          case None => ScoredTerm(None, Leaf(Score.ABSENT)).asRight[Throwable]
                        }
    } yield
      ScoredTerm(
        ReceiveBind(sortedPatterns.map(_.term), sortedChannel.term, bind.remainder, bind.freeCount),
        Node(Seq(sortedChannel.score) ++ sortedPatterns.map(_.score) ++ Seq(sortedRemainder.score))
      )
  }

  // The order of the binds must already be presorted by the time this is called.
  // This function will then sort the insides of the preordered binds.
  def sortMatch[M[_]](r: Receive): Either[Throwable, ScoredTerm[Receive]] =
    for {
      sortedBinds     <- r.binds.toList.traverse(bind => sortBind(bind))
      persistentScore = if (r.persistent) 1 else 0
      sortedBody      <- ParSortMatcher.sortMatch(r.body)
    } yield
      ScoredTerm(
        Receive(sortedBinds.map(_.term),
                sortedBody.term,
                r.persistent,
                r.bindCount,
                r.locallyFree,
                r.connectiveUsed),
        Node(Score.RECEIVE,
             Seq(Leaf(persistentScore)) ++
               sortedBinds.map(_.score) ++ Seq(sortedBody.score): _*)
      )
}
