package coop.rchain.rspace.pure

import cats.effect.Sync
import coop.rchain.rspace._

import scala.collection.immutable.Seq

class PureRSpace[F[_], C, P, A, R, K](space: ISpace[C, P, A, R, K]) {

  def consume(channels: Seq[C], patterns: Seq[P], continuation: K, persist: Boolean)(
      implicit m: Match[P, A, R],
      s: Sync[F]): F[Option[(K, Seq[R])]] =
    s.delay(space.consume(channels, patterns, continuation, persist))

  def install(channels: Seq[C], patterns: Seq[P], continuation: K)(
      implicit m: Match[P, A, R],
      s: Sync[F]): F[Option[(K, Seq[R])]] =
    s.delay(space.install(channels, patterns, continuation))

  def produce(channel: C, data: A, persist: Boolean)(implicit m: Match[P, A, R],
                                                     s: Sync[F]): F[Option[(K, Seq[R])]] =
    s.delay(space.produce(channel, data, persist))

  def createCheckpoint()(s: Sync[F]): F[Checkpoint] = s.delay(space.createCheckpoint())

  def reset(hash: Blake2b256Hash)(s: Sync[F]): F[Unit] = s.delay(space.reset(hash))

  def close()(s: Sync[F]): F[Unit] = s.delay(space.close())
}
