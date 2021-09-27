package coop.rchain

import coop.rchain.blockstorage.dag.BlockDagRepresentationSyntax
import coop.rchain.casper.blocks.proposer.ProposerResult
import coop.rchain.casper.rholang.{RuntimeReplaySyntax, RuntimeSyntax}
import coop.rchain.casper.util.comm.CommUtilSyntax
import coop.rchain.casper.util.rholang.RuntimeManagerSyntax
import coop.rchain.metrics.Metrics
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.casper.v2.core.syntax.{CasperSyntax, SafetyOracleSyntax}

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
    with BlockDagRepresentationSyntax
    with RuntimeSyntax
    with RuntimeReplaySyntax
    with RuntimeManagerSyntax
    with SafetyOracleSyntax
    with CasperSyntax
