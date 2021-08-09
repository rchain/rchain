package coop.rchain.casper

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper.util.DagOperations
import coop.rchain.dag.DagOps
import coop.rchain.shared.Log

object Attestation {

  /**
    * The node should propose attestation block when there are normal blocks(block with user deploys) between
    * latest blocks and last finalized blocks.
    */
  def shouldProposeAttestation[F[_]: Sync: Log: Casper: BlockStore]: F[Boolean] =
    for {
      snapShot               <- Casper[F].getSnapshot(None)
      latestBlocks           = snapShot.justifications.map(_.latestBlockHash).toList
      lastFinalizedBlockHash = snapShot.lastFinalizedBlock
      lfb                    <- BlockStore[F].getUnsafe(lastFinalizedBlockHash)
      normalBlocksF <- DagOps
                        .bfTraverseF(latestBlocks)(
                          blockHash =>
                            for {
                              b <- snapShot.dag.lookupUnsafe(blockHash)
                              result = if (b.blockNum < lfb.body.state.blockNumber) List.empty
                              else b.parents
                            } yield result
                        )
                        .filterF(b => BlockStore[F].getUnsafe(b).map(!isAttestationBlock(_)))
      normalBlocks <- normalBlocksF.toList
      _ <- Log[F].info(
            s"Blocks ${normalBlocks} with deploys are between latest blocks and last finalized blocks."
          )
    } yield normalBlocks.nonEmpty

}
