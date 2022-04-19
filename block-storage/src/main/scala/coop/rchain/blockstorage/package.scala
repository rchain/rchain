package coop.rchain

import coop.rchain.blockstorage.dag.BlockDagRepresentationSyntax
import coop.rchain.blockstorage.{ApprovedStoreSyntax, BlockStoreSyntax, ByteStringKVStoreSyntax}
import coop.rchain.metrics.Metrics

package object blockstorage {
  val BlockStorageMetricsSource: Metrics.Source =
    Metrics.Source(Metrics.BaseSource, "block-storage")

  // Importing syntax object means using all extensions in the project
  object syntax extends AllSyntaxBlockStorage
}

// Block storage syntax
trait AllSyntaxBlockStorage
    extends ApprovedStoreSyntax
    with BlockStoreSyntax
    with BlockDagRepresentationSyntax
    with ByteStringKVStoreSyntax
