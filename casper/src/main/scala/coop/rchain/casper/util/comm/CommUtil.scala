package coop.rchain.casper.util.comm

import cats.effect._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Applicative, Monad}
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

object CommUtil {

  implicit private val logSource: LogSource = LogSource(this.getClass)

  def sendBlockHash[F[_]: Monad: ConnectionsCell: TransportLayer: Log: Time: RPConfAsk](
      hash: BlockHash
  ): F[Unit] =
    sendToPeers(BlockHashMessageProto(hash)) <* Log[F].info(
      s"Sent hash ${PrettyPrinter.buildString(hash)} to peers"
    )

  def sendBlockRequest[F[_]: Monad: ConnectionsCell: TransportLayer: Log: Time: RPConfAsk: Running.RequestedBlocks](
      hash: BlockHash
  ): F[Unit] =
    Running
      .RequestedBlocks[F]
      .read
      .flatMap(
        requested =>
          Applicative[F].unlessA(requested.contains(hash))(
            Running.addNewEntry(hash) >> sendToPeers(HasBlockRequestProto(hash)) <* Log[F]
              .info(s"Requested missing block ${PrettyPrinter.buildString(hash)} from peers")
          )
      )

  def sendForkChoiceTipRequest[F[_]: Monad: ConnectionsCell: TransportLayer: Log: Time: RPConfAsk]
      : F[Unit] =
    sendToPeers(ForkChoiceTipRequest.toProto) <* Log[F].info(
      s"Requested fork tip from peers"
    )

  def sendToPeers[F[_]: Monad: ConnectionsCell: TransportLayer: Log: Time: RPConfAsk, Msg: ToPacket](
      message: Msg
  ): F[Unit] = sendToPeers(ToPacket(message))

  def sendToPeers[F[_]: Monad: ConnectionsCell: TransportLayer: Log: Time: RPConfAsk](
      message: Packet
  ): F[Unit] =
    for {
      peers <- ConnectionsCell.random
      conf  <- RPConfAsk[F].ask
      msg   = packet(conf.local, conf.networkId, message)
      _     <- TransportLayer[F].broadcast(peers, msg)
    } yield ()

  def streamToPeers[F[_]: Monad: ConnectionsCell: TransportLayer: Log: Time: RPConfAsk, Msg: ToPacket](
      message: Msg
  ): F[Unit] = streamToPeers(ToPacket(message))

  def streamToPeers[F[_]: Monad: ConnectionsCell: TransportLayer: Log: Time: RPConfAsk](
      packet: Packet
  ): F[Unit] =
    for {
      peers <- ConnectionsCell.random
      local <- RPConfAsk[F].reader(_.local)
      msg   = Blob(local, packet)
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

    RPConfAsk[F].ask >>= { conf =>
      conf.bootstrap match {
        case Some(bootstrap) =>
          val msg = packet(
            conf.local,
            conf.networkId,
            ApprovedBlockRequest("PleaseSendMeAnApprovedBlock").toProto
          )
          Log[F].info("Starting to request ApprovedBlockRequest") >>
            Concurrent[F].start(keepOnRequestingTillRunning(bootstrap, msg)).void
        case None =>
          Log[F].warn("Cannot request for an approved block as standalone") // TODO we should exit here
      }
    }
  }
}
