package coop.rchain

import coop.rchain.metrics.Metrics

package object rspace {
  val RSpaceMetricsSource: String = Metrics.source + ".rspace"
}
