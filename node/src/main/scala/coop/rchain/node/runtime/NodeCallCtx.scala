package coop.rchain.node.runtime

import cats.data.ReaderT
import cats.effect.kernel.Async
import cats.effect.{CancelToken, ConcurrentEffect, ExitCase, Fiber, IO, SyncIO}
import cats.~>
import coop.rchain.node.diagnostics.Trace
import coop.rchain.node.diagnostics.Trace.TraceId

final case class NodeCallCtx(trace: TraceId) {
  def next: NodeCallCtx = this.copy(trace = Trace.next)
}

object NodeCallCtx {
  def init: NodeCallCtx = NodeCallCtx(Trace.next)

  final case class NodeCallCtxReader[F[_]: AsyncEffect]() {

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
    val envToEff: ReaderNodeCallCtx ~> F = λ[ReaderNodeCallCtx ~> F](x => x.run(NodeCallCtx.init))

    implicit val localEnvironment = cats.mtl.instances.all.localReader[F, NodeCallCtx]

    /**
      * Implementation for ConcurrentEffect for ReaderT cannot be constructed automatically so it's
      * wired up here from existing [[Concurrent]] and [[ConcurrentEffect]] implementations.
      *
      * `runCancelable` and `runAsync` are newly provided.
      */
    implicit val concurrentReaderNodeCallCtx = new ConcurrentEffect[ReaderNodeCallCtx] {
      val c = Async[ReaderNodeCallCtx]
      val t = ConcurrentEffect[F]

      // ConcurrentEffect
      override def runCancelable[A](fa: ReaderNodeCallCtx[A])(
          cb: Either[Throwable, A] => IO[Unit]
      ): SyncIO[CancelToken[ReaderNodeCallCtx]] =
        t.runCancelable(envToEff(fa))(cb).map(effToEnv(_))
      override def runAsync[A](
          fa: ReaderNodeCallCtx[A]
      )(cb: Either[Throwable, A] => IO[Unit]): SyncIO[Unit] =
        t.runAsync(envToEff(fa))(cb)
      // Async
      override def async[A](k: (Either[Throwable, A] => Unit) => Unit): ReaderNodeCallCtx[A] =
        c.async_(k)
      override def asyncF[A](
          k: (Either[Throwable, A] => Unit) => ReaderNodeCallCtx[Unit]
      ): ReaderNodeCallCtx[A] =
        c.asyncF(k)
      // Concurrent
      override def start[A](
          fa: ReaderNodeCallCtx[A]
      ): ReaderNodeCallCtx[Fiber[ReaderNodeCallCtx, A]] = c.start(fa)
      override def racePair[A, B](
          fa: ReaderNodeCallCtx[A],
          fb: ReaderNodeCallCtx[B]
      ): ReaderNodeCallCtx[
        Either[(A, Fiber[ReaderNodeCallCtx, B]), (Fiber[ReaderNodeCallCtx, A], B)]
      ]                                                                             = c.racePair(fa, fb)
      override def suspend[A](thunk: => ReaderNodeCallCtx[A]): ReaderNodeCallCtx[A] = c.defer(thunk)
      override def bracketCase[A, B](acquire: ReaderNodeCallCtx[A])(use: A => ReaderNodeCallCtx[B])(
          release: (A, ExitCase[Throwable]) => ReaderNodeCallCtx[Unit]
      ): ReaderNodeCallCtx[B]                                        = c.bracketCase(acquire)(use)(release)
      override def raiseError[A](e: Throwable): ReaderNodeCallCtx[A] = c.raiseError(e)
      override def handleErrorWith[A](
          fa: ReaderNodeCallCtx[A]
      )(f: Throwable => ReaderNodeCallCtx[A]): ReaderNodeCallCtx[A] =
        c.handleErrorWith(fa)(f)
      override def flatMap[A, B](
          fa: ReaderNodeCallCtx[A]
      )(f: A => ReaderNodeCallCtx[B]): ReaderNodeCallCtx[B] =
        c.flatMap(fa)(f)
      override def tailRecM[A, B](
          a: A
      )(f: A => ReaderNodeCallCtx[Either[A, B]]): ReaderNodeCallCtx[B] =
        c.tailRecM(a)(f)
      override def pure[A](x: A): ReaderNodeCallCtx[A] = c.pure(x)
    }
  }
}
