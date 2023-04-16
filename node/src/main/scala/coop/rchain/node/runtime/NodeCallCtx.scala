package coop.rchain.node.runtime

import cats.data.ReaderT
import cats.effect.kernel.Async
import cats.~>
import coop.rchain.node.diagnostics.Trace
import coop.rchain.node.diagnostics.Trace.TraceId

final case class NodeCallCtx(trace: TraceId) {
  def next: NodeCallCtx = this.copy(trace = Trace.next)
}

object NodeCallCtx {
  def init: NodeCallCtx = NodeCallCtx(Trace.next)

  final case class NodeCallCtxReader[F[_]: Async]() {

    /**
      * Current implementation of Span uses ReaderT layer to hold the local state for tracing.
      *
      * To be able to instantiate NodeRuntime dependencies we need ReaderT implementation for each of them.
      * If it's possible to construct FunctorK implementation like we have for Log then this can be used as a
      * more general implementation.
      */
    type ReaderNodeCallCtx[A] = ReaderT[F, NodeCallCtx, A]

    // Conversions from/to ReaderT and F
    val effToEnv: F ~> ReaderNodeCallCtx = λ[F ~> ReaderNodeCallCtx](ReaderT.liftF(_))

    implicit val localEnvironment = cats.mtl.instances.all.localReader[F, NodeCallCtx]
  }
}
