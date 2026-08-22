package coop.rchain.casper.blocks

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.rholang.RuntimeManager
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockMetadata
import coop.rchain.models.syntax._

trait BlockDagStorageSyntax {
  implicit final def casperSyntaxBlockDagStorage[F[_]](
      bds: BlockDagStorage[F]
  ): BlockDagStorageOps[F] = new BlockDagStorageOps[F](bds)
}

final class BlockDagStorageOps[F[_]](
    // DagRepresentation extensions / syntax
    private val bds: BlockDagStorage[F]
) extends AnyVal {

  // TODO: legacy function, used only in tests, it should be removed when tests are fixed
  def insertLegacy(block: BlockMessage, invalid: Boolean, approved: Boolean = false)(
      implicit sync: Sync[F]
  ): F[Unit] =
    for {
      fringeWithState <- if (approved) {
                          (Set[BlockHash](), block.postStateHash).pure[F]
                        } else {
                          for {
                            dag      <- bds.getRepresentation
                            dagMsgSt = dag.dagMessageState
                            parents  = block.justifications.map(dagMsgSt.msgMap).toSet
                            fringe   = dagMsgSt.msgMap.latestFringe(parents).map(_.id)
                            fringeState = if (fringe.isEmpty) {
                              RuntimeManager.emptyStateHashFixed.toBlake2b256Hash
                            } else
                              dag.fringeStates(fringe).stateHash
                          } yield (fringe, fringeState.toByteString)
                        }

      (fringe, fringeState) = fringeWithState
      bmd = BlockMetadata
        .fromBlock(block)
        .copy(validationFailed = invalid, fringe = fringe, fringeStateHash = fringeState)

      _ <- bds.insert(bmd, block)
    } yield ()
}
