package coop.rchain.casper.blocks

import coop.rchain.metrics.Metrics

package object merger {
  val MergingMetricsSource: Metrics.Source =
    Metrics.Source(Metrics.BaseSource, "merging")
}
