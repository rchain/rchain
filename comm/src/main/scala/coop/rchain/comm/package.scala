package coop.rchain

import cats.mtl.ApplicativeAsk

import coop.rchain.metrics.Metrics

package object comm {
  type PeerNodeAsk[F[_]] = ApplicativeAsk[F, PeerNode]

  object PeerNodeAsk {
    def apply[F[_]](implicit ev: ApplicativeAsk[F, PeerNode]): ApplicativeAsk[F, PeerNode] = ev
  }

  val CommMetricsSource: Metrics.Source = Metrics.Source(Metrics.BaseSource, "comm")
}
