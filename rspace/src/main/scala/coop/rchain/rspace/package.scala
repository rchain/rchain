package coop.rchain

import coop.rchain.metrics.Metrics

package object rspace {
  val RSpaceMetricsSource: Metrics.Source = Metrics.Source(Metrics.BaseSource, "rspace")
}
