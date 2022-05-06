package coop.rchain.casper.blocks

import cats.effect.{Concurrent, Timer}
import cats.syntax.all._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.casperbuffer.CasperBufferStorage
import coop.rchain.blockstorage.dag.{BlockDagStorage, DagRepresentation}
import coop.rchain.casper._
import coop.rchain.casper.engine.BlockRetriever
import coop.rchain.casper.protocol.{BlockMessage, CommUtil}
import coop.rchain.casper.rholang.RuntimeManager
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.shared.{Log, Time}

/**
  * Logic for processing incoming blocks
  * Blocks created by node itself are not held here, but in Proposer.
  */
class BlockProcessor[F[_]: Concurrent: BlockDagStorage: BlockStore: CasperBufferStorage: CommUtil: Log](
    storeBlock: BlockMessage => F[Unit],
    requestMissingDependencies: Set[BlockHash] => F[Unit],
    ackProcessed: (BlockMessage) => F[Unit],
    // Casper state to validate block against
    getCasperSnapshot: F[CasperSnapshot],
    validateBlock: (CasperSnapshot, BlockMessage) => F[ValidBlockProcessing],
    effValidBlock: BlockMessage => F[DagRepresentation],
    effInvalidVBlock: (BlockMessage, InvalidBlock, CasperSnapshot) => F[DagRepresentation]
) {

  // validate block and invoke all effects required
  def validateWithEffects(b: BlockMessage): F[ValidBlockProcessing] =
    for {
      cs     <- getCasperSnapshot
      status <- validateBlock(cs, b)
      _ <- status
            .map(s => effValidBlock(b))
            .leftMap {
              // this is to maintain backward compatibility with casper validate method.
              // as it returns not only InvalidBlock or ValidBlock
              case i: InvalidBlock => effInvalidVBlock(b, i, cs)
              case _               => cs.dag.pure[F] // this should never happen
            }
            .merge
      // once block is validated and effects are invoked, it should be removed from buffer
      // _ <- removeFromBuffer(b)
      _ <- ackProcessed(b)
    } yield (status)

  val getDependencyFreeFromBuffer = MultiParentCasper.getDependencyFreeFromBuffer
}

object BlockProcessor {
  // format: off
  def apply[F[_]
  /* Execution */   : Concurrent: Timer: Time: RuntimeManager
  /* Storage */     : BlockStore: BlockDagStorage: CasperBufferStorage
  /* Diagnostics */ : Log: Metrics: Span
  /* Comm */        : CommUtil: BlockRetriever // format: on
  ](casperShardConf: CasperShardConf): BlockProcessor[F] = {

    val storeBlock = (b: BlockMessage) => BlockStore[F].put(b)

    val getCasperStateSnapshot = MultiParentCasper.getSnapshot[F](casperShardConf)

    val requestMissingDependencies = (deps: Set[BlockHash]) => {
      import cats.instances.list._
      deps.toList.traverse_(
        BlockRetriever[F]
          .admitHash(_, admitHashReason = BlockRetriever.MissingDependencyRequested)
      )
    }

    val validateBlock = (s: CasperSnapshot, b: BlockMessage) => MultiParentCasper.validate(b, s)

    def ackProcessed =
      (b: BlockMessage) => BlockRetriever[F].ackInCasper(b.blockHash)

    val effectsForInvalidBlock =
      (b: BlockMessage, r: InvalidBlock, s: CasperSnapshot) =>
        for {
          r <- MultiParentCasper.handleInvalidBlock(b, r, s.dag)
          _ <- CommUtil[F].sendBlockHash(b.blockHash, b.sender)
        } yield r

    val effectsForValidBlock = (b: BlockMessage) =>
      for {
        r <- MultiParentCasper.handleValidBlock(b)
        _ <- CommUtil[F].sendBlockHash(b.blockHash, b.sender)
      } yield r

    new BlockProcessor[F](
      storeBlock,
      requestMissingDependencies,
      ackProcessed,
      getCasperStateSnapshot,
      validateBlock,
      effectsForValidBlock,
      effectsForInvalidBlock
    )
  }
}
