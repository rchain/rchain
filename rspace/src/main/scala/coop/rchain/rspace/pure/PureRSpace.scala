package coop.rchain.rspace.pure

import cats.effect.Sync
import coop.rchain.rspace.Match.MatchResult
import coop.rchain.rspace._

import scala.collection.immutable.Seq

class PureRSpace[F[_]: Sync, C, P, E, A, S, R, K](space: FreudianSpace[C, P, E, A, S, R, K]) {

  def consume(channels: Seq[C], patterns: Seq[P], continuation: K, persist: Boolean)(
      implicit m: Match[P, E, A, S, R]): F[MatchResult[(K, Seq[R]), S, E]] =
    Sync[F].delay(space.consume(channels, patterns, continuation, persist))

  def install(channels: Seq[C], patterns: Seq[P], continuation: K)(
      implicit m: Match[P, E, A, S, R]): F[Option[(K, Seq[R])]] =
    Sync[F].delay(space.install(channels, patterns, continuation))

  def produce(channel: C, data: A, persist: Boolean)(
      implicit m: Match[P, E, A, S, R]): F[MatchResult[(K, Seq[R]), S, E]] =
    Sync[F].delay(space.produce(channel, data, persist))

  def createCheckpoint(): F[Checkpoint] = Sync[F].delay(space.createCheckpoint())

  def reset(hash: Blake2b256Hash): F[Unit] = Sync[F].delay(space.reset(hash))

  def close(): F[Unit] = Sync[F].delay(space.close())
}

object PureRSpace {
  def apply[F[_]](implicit F: Sync[F]): PureRSpaceApplyBuilders[F] = new PureRSpaceApplyBuilders(F)

  final class PureRSpaceApplyBuilders[F[_]](val F: Sync[F]) extends AnyVal {
    def of[C, P, E, A, S, R, K](space: FreudianSpace[C, P, E, A, S, R, K]) =
      new PureRSpace[F, C, P, E, A, S, R, K](space)(F)
  }
}
