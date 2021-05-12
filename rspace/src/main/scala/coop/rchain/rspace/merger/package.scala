package coop.rchain.rspace

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.rspace.hashing.Blake2b256Hash

package object merger {
  def computeChannelChange[F[_]: Sync, T](
      hash: Blake2b256Hash,
      getStartValue: Blake2b256Hash => F[Seq[T]],
      getEndValue: Blake2b256Hash => F[Seq[T]]
  ): F[ChannelChange[T]] =
    for {
      dataAtBase     <- getStartValue(hash)
      dataAtMerge    <- getEndValue(hash)
      atMergeAdded   = dataAtMerge diff dataAtBase
      atMergeDeleted = dataAtBase diff dataAtMerge
    } yield ChannelChange(atMergeAdded, atMergeDeleted)
}
