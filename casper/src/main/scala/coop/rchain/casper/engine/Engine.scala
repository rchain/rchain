package coop.rchain.casper.engine

import cats.effect.concurrent.Ref
import cats.effect.{Concurrent, Sync}
import cats.implicits._
import cats.{Applicative, Monad}
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.blockstorage.dag.state.BlockDagState
import coop.rchain.blockstorage.deploy.DeployStorage
import coop.rchain.casper.LastApprovedBlock.LastApprovedBlock
import coop.rchain.casper._
import coop.rchain.casper.engine.EngineCell._
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.comm.CommUtil
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
import fs2.concurrent.Queue

trait Engine[F[_]] {
  def init: F[Unit]
  def handle(peer: PeerNode, msg: CasperMessage): F[Unit]
  def withCasper[A](
      f: MultiParentCasper[F] => F[A],
      default: F[A]
  ): F[A] = default
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
  def insertIntoBlockAndDagStore[F[_]: Sync: Concurrent: Log: BlockStore: BlockDagStorage](
      genesis: BlockMessage,
      approvedBlock: ApprovedBlock
  ): F[Unit] =
    for {
      _ <- BlockStore[F].put(genesis.blockHash, genesis)
      _ <- BlockDagStorage[F].insert(
            genesis,
            invalid = false,
            0L
          )
      _ <- BlockStore[F].putApprovedBlock(approvedBlock)
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
    /* Storage */     : BlockStore: BlockDagStorage: RSpaceStateManager
    /* Diagnostics */ : Log: EventLog: Metrics] // format: on
  (
      casper: MultiParentCasper[F],
      blockDagStateRef: Ref[F, BlockDagState],
      approvedBlock: ApprovedBlock,
      validatorId: Option[ValidatorIdentity],
      init: F[Unit],
      disableStateExporter: Boolean,
      processBlockInRunning: BlockMessage => F[Unit]
  ): F[Unit] = {
    val approvedBlockInfo = PrettyPrinter.buildString(approvedBlock.candidate.block, short = true)
    for {
      _ <- Log[F].info(s"Making a transition to Running state. Approved $approvedBlockInfo")
      _ <- EventLog[F].publish(
            shared.Event.EnteredRunningState(
              PrettyPrinter.buildStringNoLimit(approvedBlock.candidate.block.blockHash)
            )
          )
      // TODO these two lines are to make sure finalization is up to date with the DAG state,
      //  in case finalization has been interrupted by shutdown
      s <- casper.getSnapshot()
      _ <- casper.handleValidBlock(approvedBlock.candidate.block, s)
      running = new Running[F](
        casper,
        blockDagStateRef,
        approvedBlock,
        validatorId,
        init,
        disableStateExporter,
        processBlockInRunning: BlockMessage => F[Unit]
      )
      _ <- EngineCell[F].set(running)

    } yield ()
  }

  // format: off
  def transitionToInitializing[F[_]
    /* Execution */   : Concurrent: Time
    /* Transport */   : TransportLayer: CommUtil: BlockRetriever: EventPublisher
    /* State */       : EngineCell: RPConfAsk: ConnectionsCell: LastApprovedBlock
    /* Rholang */     : RuntimeManager
    /* Storage */     : BlockStore: BlockDagStorage: DeployStorage: RSpaceStateManager
    /* Diagnostics */ : Log: EventLog: Metrics: Span] // format: on
  (
      blockDagStateRef: Ref[F, BlockDagState],
      casperShardConf: CasperShardConf,
      validatorId: Option[ValidatorIdentity],
      init: F[Unit],
      trimState: Boolean = true,
      disableStateExporter: Boolean = false,
      processBlockInRunning: BlockMessage => F[Unit]
  ): F[Unit] =
    for {
      blockResponseQueue <- Queue.bounded[F, BlockMessage](50)
      stateResponseQueue <- Queue.bounded[F, StoreItemsMessage](50)
      _ <- EngineCell[F].set(
            new Initializing(
              blockDagStateRef,
              casperShardConf,
              validatorId,
              init,
              blockResponseQueue,
              stateResponseQueue,
              trimState,
              disableStateExporter,
              processBlockInRunning
            )
          )
    } yield ()
}
