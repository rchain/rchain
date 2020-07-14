package coop.rchain.casper.engine

import cats.{Applicative, Monad}
import cats.effect.{Concurrent, Sync}
import cats.implicits._
import EngineCell._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper._
import coop.rchain.casper.LastApprovedBlock.LastApprovedBlock
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.comm.{transport, PeerNode}
import coop.rchain.comm.protocol.routing.Packet
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.transport.{Blob, TransportLayer}
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.shared
import coop.rchain.shared._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.casperbuffer.CasperBufferStorage
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.blockstorage.deploy.DeployStorage
import coop.rchain.blockstorage.finality.LastFinalizedStorage
import coop.rchain.casper.util.comm.CommUtil

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
      _ <- BlockDagStorage[F].insert(genesis, genesis, invalid = false)
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

  def transitionToRunning[F[_]: Sync: EngineCell: Log: EventLog: BlockStore: CommUtil: TransportLayer: ConnectionsCell: RPConfAsk: Time: BlockRetriever: LastFinalizedStorage: Concurrent: CasperBufferStorage: Metrics](
      casper: MultiParentCasper[F],
      approvedBlock: ApprovedBlock,
      validatorId: Option[ValidatorIdentity],
      init: F[Unit]
  ): F[Unit] =
    for {
      _ <- Log[F].info("Making a transition to Running state.")
      _ <- EventLog[F].publish(
            shared.Event.EnteredRunningState(
              PrettyPrinter.buildStringNoLimit(approvedBlock.candidate.block.blockHash)
            )
          )
      running = new Running[F](casper, approvedBlock, validatorId, init)
      _       <- EngineCell[F].set(running)

    } yield ()

  def transitionToInitializing[F[_]: Concurrent: Metrics: Span: Monad: EngineCell: Log: EventLog: BlockStore: CommUtil: TransportLayer: ConnectionsCell: RPConfAsk: Time: SafetyOracle: LastFinalizedBlockCalculator: LastApprovedBlock: BlockDagStorage: LastFinalizedStorage: RuntimeManager: BlockRetriever: EventPublisher: SynchronyConstraintChecker: LastFinalizedHeightConstraintChecker: Estimator: DeployStorage: CasperBufferStorage](
      shardId: String,
      finalizationRate: Int,
      validatorId: Option[ValidatorIdentity],
      init: F[Unit]
  ): F[Unit] = EngineCell[F].set(new Initializing(shardId, finalizationRate, validatorId, init))

}
