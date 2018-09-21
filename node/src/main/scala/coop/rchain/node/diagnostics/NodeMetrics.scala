package coop.rchain.node.diagnostics

import cats.Monad
import cats.data.EitherT

import coop.rchain.catscontrib.MonadTrans
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.node.model.diagnostics.NodeCoreMetrics

trait NodeMetrics[F[_]] {
  def metrics: F[NodeCoreMetrics]
}

object NodeMetrics extends NodeMetricsInstances {
  def apply[F[_]](implicit M: NodeMetrics[F]): NodeMetrics[F] = M

  def forTrans[F[_]: Monad, T[_[_], _]: MonadTrans](
      implicit C: NodeMetrics[F]
  ): NodeMetrics[T[F, ?]] =
    new NodeMetrics[T[F, ?]] {
      def metrics: T[F, NodeCoreMetrics] = C.metrics.liftM[T]
    }
}

sealed abstract class NodeMetricsInstances {
  implicit def eitherTNodeMetrics[E, F[_]: Monad: NodeMetrics[?[_]]]
    : NodeMetrics[EitherT[F, E, ?]] =
    NodeMetrics.forTrans[F, EitherT[?[_], E, ?]]
}
