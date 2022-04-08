package coop.rchain.sdk.dag.syntax

import cats.Applicative
import cats.syntax.all._
import coop.rchain.sdk.dag.data.DagView
import fs2.Stream

/**
  * Basic DagView extensions.
  */
trait DagViewSyntax {
  implicit def sdkDagSyntaxDagView[F[_], M, MId, S, SId](
      dagView: DagView[F, M, MId, S, SId]
  ): DagViewOps[F, M, MId, S, SId] = new DagViewOps(dagView)
}

final class DagViewOps[F[_], M, MId, S, SId](private val dagView: DagView[F, M, MId, S, SId])
    extends AnyVal {

  /** Import of DagData implicit - necessary to enable syntax (extensions) for M and S types */
  import dagView.dd

  /**
    * Stream of self justifications for the sender of the message.
    */
  def selfJustificationChain(message: M)(implicit s: Applicative[F]): Stream[F, M] =
    Stream.unfoldEval(message)(
      _.justifications.toList
        .traverse(dagView.loadMessage)
        .map(_.find(_.sender == message.sender).map(v => (v, v)))
    )
}
