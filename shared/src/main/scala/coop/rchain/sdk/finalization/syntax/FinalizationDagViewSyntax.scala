package coop.rchain.sdk.finalization.syntax

import cats.effect.Sync
import coop.rchain.sdk.dag.data.DagView
import fs2.Stream
import cats.syntax.all._

trait FinalizationDagViewSyntax {
  implicit def sdkFinalizationSyntaxDagView[F[_], M, S](
      dagView: DagView[F, M, S]
  ): DagViewOps[F, M, S] =
    new DagViewOps(dagView)
}

final class DagViewOps[F[_], M, S](private val dagView: DagView[F, M, S]) extends AnyVal {

  /** Import of DagData implicit - necessary to enable syntax (extensions) for M and S types */
  import dagView.dd

}
