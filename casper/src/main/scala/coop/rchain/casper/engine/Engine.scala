package coop.rchain.casper.engine

import cats.effect.concurrent.Ref
import cats.effect.{Concurrent, Sync, Timer}
import cats.syntax.all._
import cats.{Applicative, Monad}
import coop.rchain.blockstorage.approvedStore.ApprovedStore
import coop.rchain.blockstorage.blockStore.BlockStore
import coop.rchain.blockstorage.casperbuffer.CasperBufferStorage
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.blockstorage.deploy.DeployStorage
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper.LastApprovedBlock.LastApprovedBlock
import coop.rchain.casper._
import coop.rchain.casper.engine.EngineCell._
import coop.rchain.casper.protocol.{CommUtil, _}
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.comm.PeerNode
import coop.rchain.comm.protocol.routing.Packet
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.transport.{Blob, TransportLayer}
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.rspace.state.RSpaceStateManager
import coop.rchain.shared
import coop.rchain.shared._
import coop.rchain.shared.syntax._
import fs2.concurrent.Queue

trait Engine[F[_]] {
  def init: F[Unit]
  def handle(peer: PeerNode, msg: CasperMessage): F[Unit]
}

object Engine {

  def noop[F[_]: Applicative] = new Engine[F] {
    private[this] val noop                                           = Applicative[F].unit
    override def handle(peer: PeerNode, msg: CasperMessage): F[Unit] = noop
    override val init: F[Unit]                                       = noop
  }

  /**
    * Peer says it has no ApprovedBlock
    * @param identifier
    * @tparam F
    * @return
    */
  def logNoApprovedBlockAvailable[F[_]: Log](identifier: String): F[Unit] =
    Log[F].info(
      s"No approved block available on node $identifier. Will request again in 10 seconds."
    )

  /*
   * Note the ordering of the insertions is important.
   * We always want the block dag store to be a subset of the block store.
   */
  def insertIntoBlockAndDagStore[F[_]: Sync: Concurrent: Log: BlockStore: ApprovedStore: BlockDagStorage](
      genesis: BlockMessage,
      approvedBlock: ApprovedBlock
  ): F[Unit] =
    for {
      _ <- BlockStore[F].put(genesis.blockHash, genesis)
      _ <- BlockDagStorage[F].insert(genesis, invalid = false, approved = true)
      _ <- ApprovedStore[F].putApprovedBlock(approvedBlock)
    } yield ()

  private def noApprovedBlockAvailable(peer: PeerNode, identifier: String): Packet =
    ToPacket(NoApprovedBlockAvailable(identifier, peer.toString).toProto)

  def sendNoApprovedBlockAvailable[F[_]: RPConfAsk: TransportLayer: Monad](
      peer: PeerNode,
      identifier: String
  ): F[Unit] =
    for {
      local <- RPConfAsk[F].reader(_.local)
      //TODO remove NoApprovedBlockAvailable.nodeIdentifier, use `sender` provided by TransportLayer
      msg = Blob(local, noApprovedBlockAvailable(local, identifier))
      _   <- TransportLayer[F].stream(peer, msg)
    } yield ()

  // format: off
  def transitionToRunning[F[_]
    /* Execution */   : Concurrent: Time
    /* Transport */   : TransportLayer: CommUtil: BlockRetriever: EventPublisher
    /* State */       : EngineCell: RPConfAsk: ConnectionsCell
    /* Storage */     : BlockStore: BlockDagStorage: CasperBufferStorage: RSpaceStateManager
    /* Diagnostics */ : Log: EventLog: Metrics] // format: on
  (
      blockProcessingQueue: Queue[F, BlockMessage],
      blocksInProcessing: Ref[F, Set[BlockHash]],
      approvedBlock: ApprovedBlock,
      validatorId: Option[ValidatorIdentity],
      init: F[Unit],
      disableStateExporter: Boolean
  ): F[Unit] = {
    val approvedBlockInfo = PrettyPrinter.buildString(approvedBlock.candidate.block, short = true)
    for {
      _ <- Log[F].info(s"Making a transition to Running state. Approved $approvedBlockInfo")
      _ <- EventLog[F].publish(
            shared.Event.EnteredRunningState(
              PrettyPrinter.buildStringNoLimit(approvedBlock.candidate.block.blockHash)
            )
          )
      running = new Running[F](
        blockProcessingQueue,
        blocksInProcessing,
        approvedBlock,
        validatorId,
        init,
        disableStateExporter
      )
      _ <- EngineCell[F].set(running)

    } yield ()
  }

  // format: off
  def transitionToInitializing[F[_]
    /* Execution */   : Concurrent: Time: Timer
    /* Transport */   : TransportLayer: CommUtil: BlockRetriever: EventPublisher
    /* State */       : EngineCell: RPConfAsk: ConnectionsCell: LastApprovedBlock
    /* Rholang */     : RuntimeManager
    /* Casper */      : LastFinalizedHeightConstraintChecker: SynchronyConstraintChecker
    /* Storage */     : BlockStore: ApprovedStore: BlockDagStorage: DeployStorage: CasperBufferStorage: RSpaceStateManager
    /* Diagnostics */ : Log: EventLog: Metrics: Span] // format: on
  (
      blockProcessingQueue: Queue[F, BlockMessage],
      blocksInProcessing: Ref[F, Set[BlockHash]],
      casperShardConf: CasperShardConf,
      validatorId: Option[ValidatorIdentity],
      init: F[Unit],
      trimState: Boolean = true,
      disableStateExporter: Boolean = false
  ): F[Unit] =
    for {
      blockResponseQueue <- Queue.bounded[F, BlockMessage](50)
      stateResponseQueue <- Queue.bounded[F, StoreItemsMessage](50)
      _ <- EngineCell[F].set(
            new Initializing(
              blockProcessingQueue,
              blocksInProcessing,
              casperShardConf,
              validatorId,
              init,
              blockResponseQueue,
              stateResponseQueue,
              trimState,
              disableStateExporter
            )
          )
    } yield ()
}
