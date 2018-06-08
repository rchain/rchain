package coop.rchain.rspace.pure

import coop.rchain.catscontrib.Capture
import coop.rchain.rspace._

import scala.collection.immutable.Seq

class PureRSpace[F[_], C, P, A, K](space: ISpace[C, P, A, K]) {

  def consume(channels: Seq[C], patterns: Seq[P], continuation: K, persist: Boolean)(
      implicit m: Match[P, A],
      c: Capture[F]): F[Option[(K, Seq[A])]] =
    c.capture(space.consume(channels, patterns, continuation, persist))

  def install(channels: Seq[C], patterns: Seq[P], continuation: K)(
      implicit m: Match[P, A],
      c: Capture[F]): F[Option[(K, Seq[A])]] =
    c.capture(space.install(channels, patterns, continuation))

  def produce(channel: C, data: A, persist: Boolean)(implicit
                                                     m: Match[P, A],
                                                     c: Capture[F]): F[Option[(K, Seq[A])]] =
    c.capture(space.produce(channel, data, persist))

  def getCheckpoint()(c: Capture[F]): F[Blake2b256Hash] = c.capture(space.getCheckpoint())

  def reset(hash: Blake2b256Hash)(c: Capture[F]): F[Unit] = c.capture(space.reset(hash))

  def close()(c: Capture[F]): F[Unit] = c.capture(space.close())
}
