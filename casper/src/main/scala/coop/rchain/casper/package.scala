package coop.rchain

import coop.rchain.metrics.Metrics

package object casper {
  val CasperMetricsSource: String = Metrics.source + ".casper"
}
