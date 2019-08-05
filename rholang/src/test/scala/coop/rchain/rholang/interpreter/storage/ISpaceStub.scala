package coop.rchain.rholang.interpreter.storage

import coop.rchain.metrics.Span.TraceId
import coop.rchain.rspace.{
  internal,
  Blake2b256Hash,
  Checkpoint,
  ContResult,
  ISpace,
  Match,
  Result,
  SoftCheckpoint
}

import scala.collection.SortedSet

class ISpaceStub[F[_], C, P, A, K] extends ISpace[F, C, P, A, K] {

  override def consume(
      channels: Seq[C],
      patterns: Seq[P],
      continuation: K,
      persist: Boolean,
      sequenceNumber: Int,
      peeks: SortedSet[Int]
  )(
      implicit m: Match[F, P, A],
      traceId: TraceId
  ): F[Option[(ContResult[C, P, K], Seq[Result[A]])]] = ???

  override def install(channels: Seq[C], patterns: Seq[P], continuation: K)(
      implicit m: Match[F, P, A],
      traceId: TraceId
  ): F[Option[(K, Seq[A])]] = ???

  override def produce(channel: C, data: A, persist: Boolean, sequenceNumber: Int)(
      implicit m: Match[F, P, A],
      traceId: TraceId
  ): F[Option[(ContResult[C, P, K], Seq[Result[A]])]] = ???

  override def createCheckpoint()(implicit traceId: TraceId): F[Checkpoint] = ???

  override def reset(root: Blake2b256Hash)(implicit traceId: TraceId): F[Unit] = ???

  override def getData(channel: C): F[Seq[internal.Datum[A]]] = ???

  override def getWaitingContinuations(
      channels: Seq[C]
  ): F[Seq[internal.WaitingContinuation[P, K]]] = ???

  override def clear()(implicit traceId: TraceId): F[Unit] = ???

  override def close(): F[Unit] = ???

  override def toMap: F[Map[Seq[C], internal.Row[P, A, K]]] = ???

  override def createSoftCheckpoint()(implicit traceId: TraceId): F[SoftCheckpoint[C, P, A, K]] =
    ???

  override def revertToSoftCheckpoint(checkpoint: SoftCheckpoint[C, P, A, K])(
      implicit traceId: TraceId
  ): F[Unit] = ???
}
