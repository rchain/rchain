package coop.rchain.models.rholang.sorter

import cats.effect.Sync
import coop.rchain.models.{Expr, New}
import cats.implicits._

private[sorter] object NewSortMatcher extends Sortable[New] {
  def sortMatch[F[_]: Sync](n: New): F[ScoredTerm[New]] =
    for {
      sortedPar <- Sortable.sortMatch(n.p)
      sortedDeployId <- n.deployId match {
                         case Some(id) => Leaf(id.sig).pure[F]
                         case None     => Leaf(Score.ABSENT).pure[F]
                       }
      sortedDeployerId <- n.deployerId match {
                           case Some(id) => Leaf(id.publicKey).pure[F]
                           case None     => Leaf(Score.ABSENT).pure[F]
                         }
      uriScore = if (n.uri.isEmpty) List(Leaf(Score.ABSENT)) else n.uri.map(Leaf.apply)
    } yield ScoredTerm(
      New(
        bindCount = n.bindCount,
        p = sortedPar.term,
        uri = n.uri,
        deployId = n.deployId,
        deployerId = n.deployerId,
        locallyFree = n.locallyFree
      ),
      new Node(
        Leaf(Score.NEW) +: (Leaf(n.bindCount.toLong) +: uriScore :+ sortedDeployId :+ sortedDeployerId :+ sortedPar.score)
      )
    )
}
