package coop.rchain

import coop.rchain.blockstorage.dag.BlockDagRepresentationSyntax
import coop.rchain.metrics.Metrics

package object blockstorage {
  val BlockStorageMetricsSource: Metrics.Source =
    Metrics.Source(Metrics.BaseSource, "block-storage")

  // Block storage syntax
  object syntax extends BlockDagRepresentationSyntax
}
