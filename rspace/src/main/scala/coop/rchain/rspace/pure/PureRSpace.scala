package coop.rchain.rspace.pure

import cats.effect.Sync
import coop.rchain.metrics.Span.TraceId
import coop.rchain.rspace._

import scala.collection.SortedSet

trait PureRSpace[F[_], C, P, A, K] {
  def consume(
      channels: Seq[C],
      patterns: Seq[P],
      continuation: K,
      persist: Boolean,
      sequenceNumber: Int = 0,
      peek: Boolean = false
  )(implicit traceId: TraceId): F[Option[(ContResult[C, P, K], Seq[Result[A]])]]

  def install(channels: Seq[C], patterns: Seq[P], continuation: K)(
      implicit traceId: TraceId
  ): F[Option[(K, Seq[A])]]

  def produce(
      channel: C,
      data: A,
      persist: Boolean,
      sequenceNumber: Int = 0
  )(implicit traceId: TraceId): F[Option[(ContResult[C, P, K], Seq[Result[A]])]]

  def createCheckpoint()(implicit traceId: TraceId): F[Checkpoint]

  def reset(hash: Blake2b256Hash)(implicit traceId: TraceId): F[Unit]

  def close(): F[Unit]
}

object PureRSpace {
  def apply[F[_]](implicit F: Sync[F]): PureRSpaceApplyBuilders[F] = new PureRSpaceApplyBuilders(F)

  final class PureRSpaceApplyBuilders[F[_]](val F: Sync[F]) extends AnyVal {
    def of[C, P, A, K](
        space: ISpace[F, C, P, A, K]
    )(implicit mat: Match[F, P, A]): PureRSpace[F, C, P, A, K] =
      new PureRSpace[F, C, P, A, K] {
        def consume(
            channels: Seq[C],
            patterns: Seq[P],
            continuation: K,
            persist: Boolean,
            sequenceNumber: Int,
            peek: Boolean = false
        )(implicit traceId: TraceId): F[Option[(ContResult[C, P, K], Seq[Result[A]])]] =
          space.consume(channels, patterns, continuation, persist, sequenceNumber, if (peek) {
            SortedSet((0 to channels.size - 1): _*)
          } else SortedSet.empty)

        def install(channels: Seq[C], patterns: Seq[P], continuation: K)(
            implicit traceId: TraceId
        ): F[Option[(K, Seq[A])]] =
          space.install(channels, patterns, continuation)

        def produce(
            channel: C,
            data: A,
            persist: Boolean,
            sequenceNumber: Int
        )(implicit traceId: TraceId): F[Option[(ContResult[C, P, K], Seq[Result[A]])]] =
          space.produce(channel, data, persist, sequenceNumber)

        def createCheckpoint()(implicit traceId: TraceId): F[Checkpoint] =
          space.createCheckpoint()

        def reset(hash: Blake2b256Hash)(implicit traceId: TraceId): F[Unit] =
          space.reset(hash)

        def close(): F[Unit] = space.close()
      }
  }
}
