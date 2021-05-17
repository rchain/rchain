package coop.rchain.models.rholang.sorter

import cats.effect.Sync
import coop.rchain.models.{Par, Receive, ReceiveBind, Var}
import cats.implicits._

object ReceiveSortMatcher extends Sortable[Receive] {

  def sortBind[F[_]: Sync](bind: ReceiveBind): F[ScoredTerm[ReceiveBind]] = {
    val patterns = bind.patterns
    val source   = bind.source
    for {
      sortedPatterns <- patterns.traverse(Sortable[Par].sortMatch[F])
      sortedChannel  <- Sortable.sortMatch(source)
      sortedRemainder <- bind.remainder match {
                          case Some(bindRemainder) =>
                            Sortable.sortMatch(bindRemainder).map { scoredVar =>
                              ScoredTerm(Some(scoredVar.term), scoredVar.score)
                            }
                          case None => ScoredTerm(None, Leaf(Score.ABSENT)).pure[F]
                        }
    } yield ScoredTerm(
      ReceiveBind(sortedPatterns.map(_.term), sortedChannel.term, bind.remainder, bind.freeCount),
      Node(Seq(sortedChannel.score) ++ sortedPatterns.map(_.score) ++ Seq(sortedRemainder.score))
    )
  }

  // The order of the binds must already be presorted by the time this is called.
  // This function will then sort the insides of the preordered binds.
  def sortMatch[F[_]: Sync](r: Receive): F[ScoredTerm[Receive]] =
    for {
      sortedBinds         <- r.binds.traverse(sortBind[F])
      persistentScore     = if (r.persistent) 1L else 0L
      peekScore           = if (r.peek) 1L else 0L
      connectiveUsedScore = if (r.connectiveUsed) 1L else 0L
      sortedBody          <- Sortable.sortMatch(r.body)
    } yield ScoredTerm(
      Receive(
        sortedBinds.map(_.term),
        sortedBody.term,
        r.persistent,
        r.peek,
        r.bindCount,
        r.locallyFree,
        r.connectiveUsed
      ),
      Node(
        Score.RECEIVE,
        Seq(Leaf(persistentScore), Leaf(peekScore)) ++ sortedBinds.map(_.score) ++ Seq(
          sortedBody.score
        )
          ++ Seq(Leaf(r.bindCount.toLong)) ++ Seq(Leaf(connectiveUsedScore)): _*
      )
    )
}
