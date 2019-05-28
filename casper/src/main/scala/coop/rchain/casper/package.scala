package coop.rchain

import com.google.protobuf.ByteString
import coop.rchain.metrics.Metrics
import coop.rchain.models.BlockHash.BlockHash

package object casper {
  type DeployId = ByteString
  type TopoSort = Vector[Vector[BlockHash]]

  val CasperMetricsSource: Metrics.Source = Metrics.Source(Metrics.BaseSource, "casper")
}
