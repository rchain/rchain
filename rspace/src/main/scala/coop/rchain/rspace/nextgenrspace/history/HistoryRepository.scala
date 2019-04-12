package coop.rchain.rspace.nextgenrspace.history

import coop.rchain.rspace.{HistoryReader, HotStoreAction}

trait HistoryRepository[F[_], C, P, A, K] extends HistoryReader[F, C, P, A, K] {
  def process(actions: List[HotStoreAction]): F[HistoryRepository[F, C, P, A, K]]

  def close(): F[Unit]
}
