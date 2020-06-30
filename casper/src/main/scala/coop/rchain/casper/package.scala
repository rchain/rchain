package coop.rchain

import coop.rchain.casper.util.comm.CommUtilSyntax
import coop.rchain.metrics.Metrics
import coop.rchain.models.BlockHash.BlockHash

package object casper {
  type TopoSort             = Vector[Vector[BlockHash]]
  type BlockProcessing[A]   = Either[BlockError, A]
  type ValidBlockProcessing = BlockProcessing[ValidBlock]

  val CasperMetricsSource: Metrics.Source = Metrics.Source(Metrics.BaseSource, "casper")

  // Importing syntax object means using all extensions in the project
  object syntax extends AllSyntaxCasper with AllSyntaxComm with AllSyntaxBlockStorage
}

// Casper syntax
trait AllSyntaxCasper extends CommUtilSyntax
