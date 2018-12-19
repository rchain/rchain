package coop.rchain

import coop.rchain.metrics.Metrics

package object casper {
  val CasperMetricsSource: Metrics.Source = Metrics.Source(Metrics.BaseSource, "casper")
}
