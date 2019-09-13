package coop.rchain.casper.util.comm

import scala.concurrent.duration._
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import com.google.protobuf.ByteString
import cats.Monad
import cats.implicits._
import cats.effect._
import com.google.protobuf.ByteString
import coop.rchain.casper.LastApprovedBlock.LastApprovedBlock
import coop.rchain.casper._
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.casper.engine._, EngineCell._

import coop.rchain.casper.protocol._
import coop.rchain.comm.discovery._
import coop.rchain.comm.protocol.routing.{Packet, Protocol}
import coop.rchain.comm.rp.Connect.RPConfAsk
import coop.rchain.comm.rp._
import coop.rchain.comm.rp.ProtocolHelper.{packet, toPacket}
import coop.rchain.comm.transport.{Blob, PacketType, TransportLayer}
import coop.rchain.comm.{transport, CommError, PeerNode}
import coop.rchain.comm.rp.ProtocolHelper
import coop.rchain.metrics.Metrics
import coop.rchain.p2p.effects._
import coop.rchain.shared._
import coop.rchain.crypto.codec.Base16
import scala.concurrent.duration._

object CommUtil {

  implicit private val logSource: LogSource = LogSource(this.getClass)

  def sendBlock[F[_]: Monad: ConnectionsCell: TransportLayer: Log: Time: RPConfAsk](
      b: BlockMessage
  ): F[Unit] = {
    val serializedBlock = b.toProto.toByteString
    for {
      _ <- streamToPeers[F](transport.BlockMessage, serializedBlock)
      _ <- Log[F].info(s"Sent ${PrettyPrinter.buildString(b)} to peers")
    } yield ()
  }

  def sendBlockRequest[F[_]: Monad: ConnectionsCell: TransportLayer: Log: Time: RPConfAsk: Running.RequestedBlocks](
      hash: BlockHash
  ): F[Unit] =
    (Running.RequestedBlocks[F].read map (_.contains(hash))).ifM(
      ().pure[F],
      for {
        _ <- Running.addNewEntry[F](hash)
        _ <- sendToPeers[F](transport.HasBlockRequest, HasBlockRequestProto(hash).toByteString)
        _ <- Log[F].info(s"Requested missing block ${PrettyPrinter.buildString(hash)} from peers")
      } yield ()
    )

  def sendForkChoiceTipRequest[F[_]: Monad: ConnectionsCell: TransportLayer: Log: Time: RPConfAsk]
      : F[Unit] = {
    val serialized = ForkChoiceTipRequestProto().toByteString
    for {
      _ <- sendToPeers[F](transport.ForkChoiceTipRequest, serialized)
      _ <- Log[F].info(s"Requested fork tip from peers")
    } yield ()
  }

  def sendToPeers[F[_]: Monad: ConnectionsCell: TransportLayer: Log: Time: RPConfAsk](
      pType: PacketType,
      serializedMessage: ByteString
  ): F[Unit] =
    for {
      peers <- ConnectionsCell.random[F]
      conf  <- RPConfAsk[F].ask
      msg   = packet(conf.local, conf.networkId, pType, serializedMessage)
      _     <- TransportLayer[F].broadcast(peers, msg)
    } yield ()

  def streamToPeers[F[_]: Monad: ConnectionsCell: TransportLayer: Log: Time: RPConfAsk](
      pType: PacketType,
      serializedMessage: ByteString
  ): F[Unit] =
    for {
      peers <- ConnectionsCell.random[F]
      local <- RPConfAsk[F].reader(_.local)
      msg   = Blob(local, Packet(pType.id, serializedMessage))
      _     <- TransportLayer[F].stream(peers, msg)
    } yield ()

  def requestApprovedBlock[F[_]: Monad: Concurrent: EngineCell: LastApprovedBlock: Log: Time: Metrics: TransportLayer: ConnectionsCell: RPConfAsk]
      : F[Unit] = {

    def keepOnRequestingTillRunning(bootstrap: PeerNode, msg: Protocol): F[Unit] =
      TransportLayer[F].send(bootstrap, msg) >>= {
        case Right(_) =>
          Log[F].info(s"Successfully sent ApprovedBlockRequest to $bootstrap")
        case Left(error) =>
          Log[F].warn(
            s"Failed to send ApprovedBlockRequest to $bootstrap because of ${CommError.errorMessage(error)}. Retrying in 10 seconds..."
          ) >> Time[F].sleep(10 seconds) >> keepOnRequestingTillRunning(bootstrap, msg)
      }

    val request = ApprovedBlockRequestProto("PleaseSendMeAnApprovedBlock").toByteString
    RPConfAsk[F].ask >>= {
      case conf =>
        conf.bootstrap match {
          case Some(bootstrap) =>
            val msg = packet(conf.local, conf.networkId, transport.ApprovedBlockRequest, request)
            Log[F].info("Starting to request ApprovedBlockRequest") >>
              Concurrent[F].start(keepOnRequestingTillRunning(bootstrap, msg)).as(())
          case None =>
            Log[F].warn("Cannot request for an approved block as standalone") // TODO we should exit here
        }
    }
  }
}
