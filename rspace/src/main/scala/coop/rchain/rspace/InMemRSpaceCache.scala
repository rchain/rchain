package coop.rchain.rspace

import cats.effect.Concurrent
import cats.syntax.all._
import coop.rchain.rspace.history._
import coop.rchain.rspace.internal.{RichDatum, RichJoin, RichKont}
import coop.rchain.store.LazyAdHocKeyValueCache

object InMemRSpaceCache {
  def apply[F[_]: Concurrent, C, P, A, K]: F[HistoryCache[F, C, P, A, K]] =
    for {
      datumsCache <- LazyAdHocKeyValueCache[F, HistoryPointer, Seq[RichDatum[A]]]
      contsCache  <- LazyAdHocKeyValueCache[F, HistoryPointer, Seq[RichKont[P, K]]]
      joinsCache  <- LazyAdHocKeyValueCache[F, HistoryPointer, Seq[RichJoin[C]]]
    } yield HistoryCache(datumsCache, contsCache, joinsCache)
}
