package coop.rchain

import coop.rchain.metrics.Metrics
import coop.rchain.models.BlockHash.BlockHash

package object casper {
  type TopoSort = Vector[Vector[BlockHash]]

  val CasperMetricsSource: Metrics.Source = Metrics.Source(Metrics.BaseSource, "casper")
}
