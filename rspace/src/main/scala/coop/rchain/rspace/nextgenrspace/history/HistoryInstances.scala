package coop.rchain.rspace.nextgenrspace.history

import cats.effect.Sync
import cats.implicits._
import cats.{Applicative, FlatMap}
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.nextgenrspace.history.History._

object HistoryInstances {
  def noMerging[F[_]: Sync](
      root: Blake2b256Hash,
      historyStore: HistoryStore[F],
      pointerBlockStore: PointerBlockStore[F]
  ): History[F] =
    new History[F](root, historyStore, pointerBlockStore)
}
