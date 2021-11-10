package coop.rchain.sdk.dag.syntax

import cats.effect.Sync
import coop.rchain.sdk.dag.data.DagView
import fs2.Stream
import cats.syntax.all._

trait DagViewSyntax {
  implicit def sdkDagSyntaxDagView[F[_], M, S](dagView: DagView[F, M, S]): DagViewOps[F, M, S] =
    new DagViewOps(dagView)
}

final class DagViewOps[F[_], M, S](private val dagView: DagView[F, M, S]) extends AnyVal {

  /** Import of DagData implicit - necessary to enable syntax (extensions) for M and S types */
  import dagView.dd

  /**
    * Senders of the latest messages.
    */
  def activeSenders(implicit s: Sync[F]): F[Set[S]] = dagView.latestMessages.map(_.keySet)

  /**
    * Stream of self justifications.
    */
  def selfJustificationChain(message: M): Stream[F, M] =
    Stream.unfold(message)(_.justifications.find(_.sender == message.sender).map(v => (v, v)))
}
