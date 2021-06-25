package coop.rchain

import com.google.protobuf.ByteString
import coop.rchain.casper.blocks.proposer.ProposerResult
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.util.comm.CommUtilSyntax
import coop.rchain.metrics.Metrics
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.casper.util.rholang.RhoRuntimeSyntax

package object casper {
  type TopoSort             = Vector[Vector[BlockHash]]
  type BlockProcessing[A]   = Either[BlockError, A]
  type ValidBlockProcessing = BlockProcessing[ValidBlock]

  type ProposeFunction[F[_]] = (Casper[F], Boolean) => F[ProposerResult]

  val CasperMetricsSource: Metrics.Source = Metrics.Source(Metrics.BaseSource, "casper")

  val MergingMetricsSource: Metrics.Source = Metrics.Source(CasperMetricsSource, "merging")

  // Importing syntax object means using all extensions in the project
  object syntax
      extends AllSyntaxCasper
      with AllSyntaxComm
      with AllSyntaxBlockStorage
      with RhoRuntimeSyntax

  /** If block message is attestation.
    * Attestation confirms post state hashes of justifications and does not introduce any state change */
  def isAttestationMessage(b: BlockMessage) =
    b.body.state.preStateHash == ByteString.EMPTY &&
      b.body.state.postStateHash == ByteString.EMPTY &&
      b.body.deploys.isEmpty
}

// Casper syntax
trait AllSyntaxCasper extends CommUtilSyntax
