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
import coop.rchain.blockstorage.dag.BlockDagStorage

trait Engine[F[_]] {

  def applicative: Applicative[F]
  val noop: F[Unit] = applicative.unit

  def init: F[Unit]                                       = noop
  def handle(peer: PeerNode, msg: CasperMessage): F[Unit] = noop
  def withCasper[A](
      f: MultiParentCasper[F] => F[A],
      default: F[A]
  ): F[A] = default
}

object Engine {

  def noop[F[_]: Applicative] = new Engine[F] {
    override def applicative: Applicative[F] = Applicative[F]
  }

  def logNoApprovedBlockAvailable[F[_]: Log](identifier: String): F[Unit] =
    Log[F].info(s"No approved block available on node $identifier")

  /*
   * Note the ordering of the insertions is important.
   * We always want the block dag store to be a subset of the block store.
   */
  def insertIntoBlockAndDagStore[F[_]: Sync: Concurrent: TransportLayer: ConnectionsCell: Log: BlockStore: BlockDagStorage](
      genesis: BlockMessage,
      approvedBlock: ApprovedBlock
  ): F[Unit] =
    for {
      _ <- BlockStore[F].put(genesis.blockHash, genesis)
      _ <- BlockDagStorage[F].insert(genesis, genesis, invalid = false)
      _ <- BlockStore[F].putApprovedBlock(approvedBlock)
    } yield ()

  private def noApprovedBlockAvailable(peer: PeerNode, identifier: String): Packet = Packet(
    transport.NoApprovedBlockAvailable.id,
    NoApprovedBlockAvailable(identifier, peer.toString).toProto.toByteString
  )

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

  def transitionToRunning[F[_]: Sync: EngineCell: Log: EventLog: RPConfAsk: BlockStore: ConnectionsCell: TransportLayer: Time: Running.RequestedBlocks](
      casper: MultiParentCasper[F],
      approvedBlock: ApprovedBlock,
      init: F[Unit]
  ): F[Unit] =
    for {
      _ <- Log[F].info("Making a transition to Running state.")
      _ <- EventLog[F].publish(
            shared.Event.EnteredRunningState(
              PrettyPrinter.buildStringNoLimit(approvedBlock.candidate.block.blockHash)
            )
          )
      running = new Running[F](casper, approvedBlock, init)
      _       <- EngineCell[F].set(running)

    } yield ()

  def transitionToInitializing[F[_]: Concurrent: Metrics: Span: Monad: EngineCell: Log: EventLog: RPConfAsk: BlockStore: ConnectionsCell: TransportLayer: Time: SafetyOracle: LastFinalizedBlockCalculator: LastApprovedBlock: BlockDagStorage: RuntimeManager: Running.RequestedBlocks: EventPublisher: SynchronyConstraintChecker](
      shardId: String,
      validatorId: Option[ValidatorIdentity],
      init: F[Unit]
  ): F[Unit] = EngineCell[F].set(new Initializing(shardId, validatorId, init))

}
