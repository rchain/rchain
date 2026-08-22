package coop.rchain.rspace.merger

import cats.Monoid

/** Change to channel, Vector used here because values will be concatenated mostly */
final case class ChannelChange[A](added: Vector[A], removed: Vector[A])

object ChannelChange {
  def empty[A]: ChannelChange[A] = ChannelChange(Vector.empty, Vector.empty)
  def combine[A](x: ChannelChange[A], y: ChannelChange[A]): ChannelChange[A] =
    ChannelChange(x.added ++ y.added, x.removed ++ y.removed)

  implicit def channelChangeMonoid[A]: Monoid[ChannelChange[A]] =
    new Monoid[ChannelChange[A]] {
      def empty: ChannelChange[A] = ChannelChange.empty
      def combine(x: ChannelChange[A], y: ChannelChange[A]): ChannelChange[A] =
        ChannelChange.combine(x, y)
    }
}
