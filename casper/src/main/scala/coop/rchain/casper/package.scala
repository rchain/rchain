package coop.rchain

import coop.rchain.casper.blocks.BlockDagStorageSyntax
import coop.rchain.casper.blocks.proposer.ProposerResult
import coop.rchain.casper.protocol.CommUtilSyntax
import coop.rchain.casper.rholang.syntax.{RuntimeManagerSyntax, RuntimeReplaySyntax, RuntimeSyntax}
import coop.rchain.metrics.Metrics
import coop.rchain.models.BlockHash.BlockHash

package object casper {
  type TopoSort             = Vector[Vector[BlockHash]]
  type BlockProcessing[A]   = Either[BlockError, A]
  type ValidBlockProcessing = BlockProcessing[ValidBlock]

  type ProposeFunction[F[_]] = Boolean => F[ProposerResult]

  val CasperMetricsSource: Metrics.Source = Metrics.Source(Metrics.BaseSource, "casper")

  val MergingMetricsSource: Metrics.Source = Metrics.Source(CasperMetricsSource, "merging")

  // Importing syntax object means using all extensions in the project
  object syntax extends AllSyntaxCasper with AllSyntaxComm with AllSyntaxBlockStorage
}

// Casper syntax
trait AllSyntaxCasper
    extends CommUtilSyntax
    with BlockDagStorageSyntax
    with RuntimeManagerSyntax
    with RuntimeSyntax
    with RuntimeReplaySyntax
