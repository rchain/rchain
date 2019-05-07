package coop.rchain.casper.engine

import cats.data.EitherT
import cats.effect.concurrent.Ref
import cats.effect.{Concurrent, Sync}
import cats.implicits._
import cats.{Applicative, Monad}
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.{BlockDagStorage, BlockStore}
import coop.rchain.casper.Estimator.Validator
import coop.rchain.casper.LastApprovedBlock.LastApprovedBlock
import coop.rchain.casper.MultiParentCasperRef.MultiParentCasperRef
import coop.rchain.casper._
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib.MonadTrans
import coop.rchain.comm.CommError.ErrorHandler
import coop.rchain.comm.discovery.NodeDiscovery
import coop.rchain.comm.protocol.routing.Packet
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.transport.{Blob, TransportLayer}
import coop.rchain.comm.{transport, PeerNode}
import coop.rchain.metrics.Metrics
import coop.rchain.shared.{Log, LogSource, Time}
import monix.eval.Task
import monix.execution.Scheduler

import scala.concurrent.duration.FiniteDuration
import scala.util.Try

trait CasperEngine[F[_]] {

  def applicative: Applicative[F]

  val noop: F[Unit]                                                                   = applicative.unit
  def init: F[Unit]                                                                   = noop
  def handleBlockMessage(peer: PeerNode, bm: BlockMessage): F[Unit]                   = noop
  def handleBlockRequest(peer: PeerNode, br: BlockRequest): F[Unit]                   = noop
  def handleForkChoiceTipRequest(peer: PeerNode, fctr: ForkChoiceTipRequest): F[Unit] = noop
  def handleApprovedBlock(ab: ApprovedBlock): F[Option[MultiParentCasper[F]]] =
    applicative.pure(none[MultiParentCasper[F]])
  def handleApprovedBlockRequest(peer: PeerNode, br: ApprovedBlockRequest): F[Unit] = noop
  def handleUnapprovedBlock(peer: PeerNode, ub: UnapprovedBlock): F[Unit]           = noop
  def handleBlockApproval(ba: BlockApproval): F[Unit]                               = noop
  def handleNoApprovedBlockAvailable(na: NoApprovedBlockAvailable): F[Unit]         = noop
}

object CasperEngine {

  /*
   * Note the ordering of the insertions is important.
   * We always want the block dag store to be a subset of the block store.
   */
  def insertIntoBlockAndDagStore[F[_]: Sync: Concurrent: ErrorHandler: TransportLayer: ConnectionsCell: Log: BlockStore: BlockDagStorage](
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
    NoApprovedBlockAvailable(identifier, peer.toString).toByteString
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

  def onApprovedBlockTransition[F[_]: Sync: Metrics: Concurrent: Time: ErrorHandler: SafetyOracle: RPConfAsk: TransportLayer: ConnectionsCell: Log: BlockStore: LastApprovedBlock: BlockDagStorage](
      b: ApprovedBlock,
      validators: Set[ByteString],
      runtimeManager: RuntimeManager[F],
      validatorId: Option[ValidatorIdentity],
      shardId: String
  ): F[Option[MultiParentCasper[F]]] =
    for {
      isValid <- Validate.approvedBlock[F](b, validators)
      casper <- if (isValid) {
                 for {
                   _       <- Log[F].info("Valid ApprovedBlock received!")
                   genesis = b.candidate.flatMap(_.block).get
                   _       <- insertIntoBlockAndDagStore[F](genesis, b)
                   _       <- LastApprovedBlock[F].set(b)
                   casper <- MultiParentCasper
                              .hashSetCasper[F](
                                runtimeManager,
                                validatorId,
                                genesis,
                                shardId
                              )
                 } yield Option(casper)
               } else
                 Log[F]
                   .info("Invalid ApprovedBlock received; refusing to add.")
                   .map(_ => none[MultiParentCasper[F]])
    } yield casper
}
