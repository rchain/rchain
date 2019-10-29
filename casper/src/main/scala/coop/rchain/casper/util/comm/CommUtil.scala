package coop.rchain.casper.util.comm

import scala.concurrent.duration._

import cats.effect._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.tagless.autoFunctorK
import cats.Applicative

import coop.rchain.casper._
import coop.rchain.casper.engine._
import coop.rchain.casper.engine.Running.RequestedBlocks
import coop.rchain.casper.protocol._
import coop.rchain.comm.{CommError, PeerNode}
import coop.rchain.comm.protocol.routing.{Packet, Protocol}
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.rp.ProtocolHelper.packet
import coop.rchain.comm.transport.{Blob, TransportLayer}
import coop.rchain.metrics.Metrics
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.shared._

import com.google.protobuf.ByteString

@autoFunctorK
trait CommUtil[F[_]] {
  def sendBlockHash(hash: BlockHash, blockCreator: ByteString): F[Unit]
  def sendBlockRequest(hash: BlockHash): F[Unit]
  def sendForkChoiceTipRequest: F[Unit]
  def sendToPeers[Msg: ToPacket](message: Msg): F[Unit] = sendToPeers(ToPacket(message))
  def sendToPeers(message: Packet): F[Unit]
  def streamToPeers[Msg: ToPacket](message: Msg): F[Unit] = streamToPeers(ToPacket(message))
  def streamToPeers(packet: Packet): F[Unit]
  def requestApprovedBlock: F[Unit]
}

object CommUtil {

  implicit private val logSource: LogSource = LogSource(this.getClass)

  def apply[F[_]](implicit ev: CommUtil[F]): CommUtil[F] = ev

  def of[F[_]: Concurrent: Log: Time: Metrics: TransportLayer: ConnectionsCell: RPConfAsk: RequestedBlocks]
      : CommUtil[F] =
    new CommUtil[F] {

      def sendBlockHash(hash: BlockHash, blockCreator: ByteString): F[Unit] =
        sendToPeers(BlockHashMessageProto(hash, blockCreator)) <* Log[F].info(
          s"Sent hash ${PrettyPrinter.buildString(hash)} to peers"
        )

      def sendBlockRequest(hash: BlockHash): F[Unit] =
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

      def sendForkChoiceTipRequest: F[Unit] =
        sendToPeers(ForkChoiceTipRequest.toProto) <* Log[F].info(
          s"Requested fork tip from peers"
        )

      def sendToPeers(message: Packet): F[Unit] =
        for {
          peers <- ConnectionsCell.random
          conf  <- RPConfAsk[F].ask
          msg   = packet(conf.local, conf.networkId, message)
          _     <- TransportLayer[F].broadcast(peers, msg)
        } yield ()

      def streamToPeers(packet: Packet): F[Unit] =
        for {
          peers <- ConnectionsCell.random
          local <- RPConfAsk[F].reader(_.local)
          msg   = Blob(local, packet)
          _     <- TransportLayer[F].stream(peers, msg)
        } yield ()

      def requestApprovedBlock: F[Unit] = {

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
              val msg =
                packet(
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

}
