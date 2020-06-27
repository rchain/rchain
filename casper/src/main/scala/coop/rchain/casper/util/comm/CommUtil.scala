package coop.rchain.casper.util.comm

import scala.concurrent.duration._
import cats.effect._
import cats.syntax.all._
import cats.tagless.autoFunctorK
import cats.{Applicative, Monad}
import coop.rchain.casper._
import coop.rchain.casper.engine._
import coop.rchain.casper.protocol._
import coop.rchain.comm.{CommError, PeerNode}
import coop.rchain.comm.protocol.routing.{Packet, Protocol}
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.rp.ProtocolHelper.{packet, protocol}
import coop.rchain.comm.transport.{Blob, TransportLayer}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.shared._
import com.google.protobuf.ByteString
import coop.rchain.casper.util.comm.CommUtil.StandaloneNodeSendToBootstrapError

// TODO: remove CommUtil completely and move to extensions (syntax) on TransportLayer
@autoFunctorK
trait CommUtil[F[_]] {
  // Broadcast packet (in one piece)
  def sendToPeers(message: Packet): F[Unit]

  // Broadcast packet in chunks (stream)
  def streamToPeers(packet: Packet): F[Unit]

  // Send packet with retry
  def sendWithRetry(
      message: Packet,
      peer: PeerNode,
      retryAfter: FiniteDuration,
      messageTypeName: String // Only for log message / should be removed with CommUtil refactor
  ): F[Unit]

  // Send request for the block to all peers
  def sendBlockRequest(hash: BlockHash): F[Unit]
}

object CommUtil {

  implicit private val logSource: LogSource = LogSource(this.getClass)

  // Standalone (bootstrap) node should try send messages to bootstrap node
  final case object StandaloneNodeSendToBootstrapError extends Exception

  def apply[F[_]](implicit ev: CommUtil[F]): CommUtil[F] = ev

  def of[F[_]: Concurrent: TransportLayer: RPConfAsk: ConnectionsCell: Log: Time]
      : CommUtil[F] =
    new CommUtil[F] {

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

      def sendWithRetry(
          message: Packet,
          peer: PeerNode,
          retryAfter: FiniteDuration,
          msgTypeName: String
      ): F[Unit] = {
        def keepOnRequestingTillRunning(peer: PeerNode, msg: Protocol): F[Unit] =
          TransportLayer[F].send(peer, msg) >>= {
            case Right(_) =>
              Log[F].info(s"Successfully sent ${msgTypeName} to $peer")
            case Left(error) =>
              Log[F].warn(
                s"Failed to send ${msgTypeName} to $peer because of ${CommError
                  .errorMessage(error)}. Retrying in $retryAfter..."
              ) >> Time[F].sleep(retryAfter) >> keepOnRequestingTillRunning(peer, msg)
          }

        RPConfAsk[F].ask >>= { conf =>
          val msg = packet(conf.local, conf.networkId, message)
          Log[F].info(s"Starting to request ${msg.getClass.getName}") >>
            Concurrent[F].start(keepOnRequestingTillRunning(peer, msg)).void
        }
      }

      def sendBlockRequest(hash: BlockHash): F[Unit] =
        // TODO: Running is depending on CommUtil, it should't be used here
        Running
          .RequestedBlocks[F]
          .read
          .flatMap(
            requested =>
              Applicative[F].unlessA(requested.contains(hash))(
                Running.addNewEntry(hash) >> sendToPeers(ToPacket(HasBlockRequestProto(hash))) >>
                  Log[F]
                    .info(s"Requested missing block ${PrettyPrinter.buildString(hash)} from peers")
              )
          )

    }
}

trait CommUtilSyntax {
  implicit final def casperSyntaxCommUtil[F[_]](commUtil: CommUtil[F]): CommUtilOps[F] =
    new CommUtilOps[F](commUtil)
}

final class CommUtilOps[F[_]](
    // CommUtil extensions / syntax
    private val commUtil: CommUtil[F]
) {
  def sendToPeers[Msg: ToPacket](message: Msg): F[Unit] =
    commUtil.sendToPeers(ToPacket(message))

  def streamToPeers[Msg: ToPacket](message: Msg): F[Unit] =
    commUtil.streamToPeers(ToPacket(message))

  def sendBlockHash(
      hash: BlockHash,
      blockCreator: ByteString
  )(implicit m: Monad[F], log: Log[F]): F[Unit] =
    sendToPeers(BlockHashMessageProto(hash, blockCreator)) >>
      Log[F].info(s"Sent hash ${PrettyPrinter.buildString(hash)} to peers")

  def sendForkChoiceTipRequest(implicit m: Monad[F], log: Log[F]): F[Unit] =
    sendToPeers(ForkChoiceTipRequest.toProto) >>
      Log[F].info(s"Requested fork tip from peers")

  def requestApprovedBlock(implicit m: Sync[F], r: RPConfAsk[F]): F[Unit] =
    for {
      maybeBootstrap <- RPConfAsk[F].reader(_.bootstrap)
      bootstrap      <- maybeBootstrap.liftTo(StandaloneNodeSendToBootstrapError)
      msg            = ApprovedBlockRequest("PleaseSendMeAnApprovedBlock").toProto
      _              <- commUtil.sendWithRetry(ToPacket(msg), bootstrap, 10.seconds, "ApprovedBlockRequest")
    } yield ()
}
