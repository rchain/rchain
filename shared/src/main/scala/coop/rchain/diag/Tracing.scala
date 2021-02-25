package coop.rchain.diag

import coop.rchain.metrics.Metrics

object Tracing {
  val CasperMetricsSource: Metrics.Source = Metrics.Source(Metrics.BaseSource, "casper")
  val RuntimeManagerMetricsSource         = Metrics.Source(CasperMetricsSource, "runtime-manager")
}
