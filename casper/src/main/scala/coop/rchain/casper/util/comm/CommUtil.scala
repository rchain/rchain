package coop.rchain.casper.util.comm

import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import com.google.protobuf.ByteString
import cats.Monad
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.LastApprovedBlock.LastApprovedBlock
import coop.rchain.casper._
import coop.rchain.casper.protocol._
import coop.rchain.catscontrib.Capture
import coop.rchain.comm.CommError.ErrorHandler
import coop.rchain.comm.discovery._
import coop.rchain.comm.protocol.routing.Packet
import coop.rchain.comm.rp.Connect.RPConfAsk
import coop.rchain.comm.rp._
import coop.rchain.comm.rp.ProtocolHelper.{packet, toPacket}
import coop.rchain.comm.transport.{Blob, PacketType, TransportLayer}
import coop.rchain.comm.{transport, PeerNode}
import coop.rchain.comm.rp.ProtocolHelper
import coop.rchain.metrics.Metrics
import coop.rchain.p2p.effects._
import coop.rchain.shared._

import scala.concurrent.duration._

object CommUtil {

  private implicit val logSource: LogSource = LogSource(this.getClass)

  def sendBlock[F[_]: Monad: ConnectionsCell: TransportLayer: Log: Time: ErrorHandler: RPConfAsk](
      b: BlockMessage
  ): F[Unit] = {
    val serializedBlock = b.toByteString
    for {
      _ <- streamToPeers[F](transport.BlockMessage, serializedBlock)
      _ <- Log[F].info(s"Sent ${PrettyPrinter.buildString(b)} to peers")
    } yield ()
  }

  def sendBlockRequest[F[_]: Monad: ConnectionsCell: TransportLayer: Log: Time: ErrorHandler: RPConfAsk](
      r: BlockRequest
  ): F[Unit] = {
    val serialized = r.toByteString
    val hashString = PrettyPrinter.buildString(r.hash)
    for {
      _ <- sendToPeers[F](transport.BlockRequest, serialized)
      _ <- Log[F].info(s"Requested missing block $hashString from peers")
    } yield ()
  }

  def sendToPeers[F[_]: Monad: ConnectionsCell: TransportLayer: Log: Time: RPConfAsk](
      pType: PacketType,
      serializedMessage: ByteString
  ): F[Unit] =
    for {
      peers <- ConnectionsCell[F].read
      local <- RPConfAsk[F].reader(_.local)
      msg   = packet(local, pType, serializedMessage)
      _     <- TransportLayer[F].broadcast(peers, msg)
    } yield ()

  def streamToPeers[F[_]: Monad: ConnectionsCell: TransportLayer: Log: Time: RPConfAsk](
      pType: PacketType,
      serializedMessage: ByteString
  ): F[Unit] =
    for {
      peers <- ConnectionsCell[F].read
      local <- RPConfAsk[F].reader(_.local)
      msg   = Blob(local, Packet(pType.id, serializedMessage))
      _     <- TransportLayer[F].stream(peers, msg)
    } yield ()

  def requestApprovedBlock[F[_]: Monad: Capture: LastApprovedBlock: Log: Time: Metrics: TransportLayer: ConnectionsCell: ErrorHandler: PacketHandler: RPConfAsk](
      delay: FiniteDuration
  ): F[Unit] = {
    val request = ApprovedBlockRequest("PleaseSendMeAnApprovedBlock").toByteString
    for {
      maybeBootstrap <- RPConfAsk[F].reader(_.bootstrap)
      local          <- RPConfAsk[F].reader(_.local)
      _ <- maybeBootstrap match {
            case Some(bootstrap) =>
              val msg = packet(local, transport.ApprovedBlockRequest, request)
              TransportLayer[F].send(bootstrap, msg)
            case None => Log[F].warn("Cannot request for an approved block as standalone")
          }
    } yield ()
  }
}
