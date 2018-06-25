package coop.rchain.casper.util.comm

import com.google.protobuf.ByteString

import cats.Monad
import cats.implicits._
import coop.rchain.casper.{MultiParentCasper, PrettyPrinter, Validate}
import coop.rchain.casper.protocol._
import coop.rchain.comm.PeerNode
import coop.rchain.comm.protocol.rchain.Packet
import coop.rchain.crypto.codec.Base16
import coop.rchain.p2p.effects._
import coop.rchain.comm.transport._, CommMessages._
import coop.rchain.comm.discovery._
import coop.rchain.comm.CommError.ErrorHandler
import coop.rchain.shared._

import scala.util.Try

object CommUtil {

  private implicit val logSource: LogSource = LogSource(this.getClass)

  def sendBlock[F[_]: Monad: NodeDiscovery: TransportLayer: Log: Time: ErrorHandler](
      b: BlockMessage): F[Unit] = {
    val serializedBlock = b.toByteString
    val hashString      = PrettyPrinter.buildString(b.blockHash)
    for {
      _               <- Log[F].info(s"CASPER: Beginning send of ${PrettyPrinter.buildString(b)} to peers...")
      successfulPeers <- sendToPeers[F](serializedBlock)
      _               <- successfulPeers.traverse(peer => Log[F].info(s"CASPER: Sent $hashString to $peer"))
    } yield ()
  }

  def sendBlockRequest[F[_]: Monad: NodeDiscovery: TransportLayer: Log: Time: ErrorHandler](
      r: BlockRequest): F[Unit] = {
    val serialized = r.toByteString
    val hashString = PrettyPrinter.buildString(r.hash)
    for {
      _               <- Log[F].info(s"CASPER: Beginning request of missing block $hashString from peers...")
      successfulPeers <- sendToPeers[F](serialized)
      _ <- successfulPeers.traverse(peer =>
            Log[F].info(s"CASPER: Requested $hashString from $peer"))
    } yield ()
  }

  def sendToPeers[F[_]: Monad: NodeDiscovery: TransportLayer: Log: Time: ErrorHandler](
      serializedMessage: ByteString): F[List[PeerNode]] =
    for {
      peers <- NodeDiscovery[F].peers
      local <- TransportLayer[F].local
      sends <- peers.toList.traverse { peer =>
                val msg = PacketMessage(packet(local, serializedMessage))
                TransportLayer[F].send(msg, peer).map(res => (res, peer))
              }
      successes <- sends.traverse {
                    case (Left(err), _) =>
                      Log[F].error(s"$err") *> List.empty[PeerNode].pure[F]
                    case (Right(_), peer) =>
                      List(peer).pure[F]
                  }
    } yield successes.flatten

  def casperPacketHandler[
      F[_]: Monad: MultiParentCasper: NodeDiscovery: TransportLayer: Log: Time: ErrorHandler](
      peer: PeerNode): PartialFunction[Packet, F[Option[Packet]]] =
    Function
      .unlift(
        (p: Packet) => { packetToBlockRequest(p) orElse packetToBlockMessage(p) }
      )
      .andThen {
        case b: BlockMessage =>
          for {
            isOldBlock <- MultiParentCasper[F].contains(b)
            _ <- if (isOldBlock) {
                  Log[F].info(
                    s"CASPER: Received block ${PrettyPrinter.buildString(b.blockHash)} again.")
                } else {
                  handleNewBlock[F](b)
                }
          } yield none[Packet]

        case r: BlockRequest =>
          for {
            dag   <- MultiParentCasper[F].blockDag
            local <- TransportLayer[F].local
            block = dag.blockLookup.get(r.hash).map(_.toByteString)
            maybeMsg = block.map(serializedMessage =>
              PacketMessage(packet(local, serializedMessage)))
            send     <- maybeMsg.traverse(msg => TransportLayer[F].send(msg, peer))
            hash     = PrettyPrinter.buildString(r.hash)
            logIntro = s"Received request for block $hash from $peer. "
            _ <- send match {
                  case None => Log[F].info(logIntro + "No response given since block not found.")
                  case Some(Left(err)) =>
                    Log[F].info(logIntro) *> Log[F].error(
                      s"Error sending block $hash to $peer: $err")
                  case Some(Right(_)) => Log[F].info(logIntro + "Response sent.")
                }
          } yield none[Packet]
      }

  private def handleNewBlock[
      F[_]: Monad: MultiParentCasper: NodeDiscovery: TransportLayer: Log: Time: ErrorHandler](
      b: BlockMessage): F[Unit] =
    for {
      _ <- Log[F].info(s"CASPER: Received ${PrettyPrinter.buildString(b)}.")
      _ <- MultiParentCasper[F].addBlock(b)
    } yield ()

  private def packetToBlockMessage(msg: Packet): Option[BlockMessage] =
    Try(BlockMessage.parseFrom(msg.content.toByteArray)).toOption

  private def packetToBlockRequest(msg: Packet): Option[BlockRequest] =
    Try(BlockRequest.parseFrom(msg.content.toByteArray)).toOption
      .filter(r => r.base16Hash == Base16.encode(r.hash.toByteArray))
}
