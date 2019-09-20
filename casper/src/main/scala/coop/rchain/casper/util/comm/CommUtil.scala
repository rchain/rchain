package coop.rchain.casper.util.comm

import cats.Monad
import cats.effect._
import cats.implicits._
import coop.rchain.casper.LastApprovedBlock.LastApprovedBlock
import coop.rchain.casper._
import coop.rchain.casper.engine.EngineCell._
import coop.rchain.casper.engine._
import coop.rchain.casper.protocol._
import coop.rchain.comm.protocol.routing.{Packet, Protocol}
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.rp.ProtocolHelper.packet
import coop.rchain.comm.transport.{Blob, TransportLayer}
import coop.rchain.comm.{CommError, PeerNode}
import coop.rchain.metrics.Metrics
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.shared._

import scala.concurrent.duration._
import scalapb.GeneratedMessage

object CommUtil {

  implicit private val logSource: LogSource = LogSource(this.getClass)

  def sendBlock[F[_]: Monad: ConnectionsCell: TransportLayer: Log: Time: RPConfAsk](
      b: BlockMessage
  ): F[Unit] =
    for {
      _ <- streamToPeers[F](ToPacket(b.toProto))
      _ <- Log[F].info(s"Sent ${PrettyPrinter.buildString(b)} to peers")
    } yield ()

  def sendBlockRequest[F[_]: Monad: ConnectionsCell: TransportLayer: Log: Time: RPConfAsk: Running.RequestedBlocks](
      hash: BlockHash
  ): F[Unit] =
    (Running.RequestedBlocks[F].read map (_.contains(hash))).ifM(
      ().pure[F],
      for {
        _ <- Running.addNewEntry[F](hash)
        _ <- sendToPeers[F](ToPacket(HasBlockRequestProto(hash)))
        _ <- Log[F].info(s"Requested missing block ${PrettyPrinter.buildString(hash)} from peers")
      } yield ()
    )

  def sendForkChoiceTipRequest[F[_]: Monad: ConnectionsCell: TransportLayer: Log: Time: RPConfAsk]
      : F[Unit] =
    for {
      _ <- sendToPeers[F](ToPacket(ForkChoiceTipRequest.toProto))
      _ <- Log[F].info(s"Requested fork tip from peers")
    } yield ()

  def sendToPeers[F[_]: Monad: ConnectionsCell: TransportLayer: Log: Time: RPConfAsk](
      message: Packet
  ): F[Unit] =
    for {
      peers <- ConnectionsCell.random[F]
      conf  <- RPConfAsk[F].ask
      msg   = packet(conf.local, conf.networkId, message)
      _     <- TransportLayer[F].broadcast(peers, msg)
    } yield ()

  def streamToPeers[F[_]: Monad: ConnectionsCell: TransportLayer: Log: Time: RPConfAsk](
      message: Packet
  ): F[Unit] =
    for {
      peers <- ConnectionsCell.random[F]
      local <- RPConfAsk[F].reader(_.local)
      msg   = Blob(local, message)
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

    val request = ApprovedBlockRequestProto("PleaseSendMeAnApprovedBlock")
    RPConfAsk[F].ask >>= {
      case conf =>
        conf.bootstrap match {
          case Some(bootstrap) =>
            val msg = packet(conf.local, conf.networkId, request)
            Log[F].info("Starting to request ApprovedBlockRequest") >>
              Concurrent[F].start(keepOnRequestingTillRunning(bootstrap, msg)).as(())
          case None =>
            Log[F].warn("Cannot request for an approved block as standalone") // TODO we should exit here
        }
    }
  }
}
