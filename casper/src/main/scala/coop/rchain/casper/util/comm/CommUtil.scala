package coop.rchain.casper.util.comm

import com.google.protobuf.ByteString

import cats.Monad
import cats.implicits._
import coop.rchain.catscontrib.Capture
import coop.rchain.casper.{MultiParentCasper, MultiParentCasperConstructor, PrettyPrinter, Validate}
import coop.rchain.casper.protocol._
import coop.rchain.comm.{PeerNode, ProtocolHelper}
import coop.rchain.comm.protocol.rchain.Packet
import coop.rchain.crypto.codec.Base16
import coop.rchain.p2p.effects._
import coop.rchain.comm.connect.Connect
import coop.rchain.comm.transport._, CommMessages._
import coop.rchain.comm.discovery._
import coop.rchain.comm.CommError.ErrorHandler
import coop.rchain.metrics.Metrics
import coop.rchain.shared._

import scala.concurrent.duration._
import scala.util.Try

object CommUtil {

  private implicit val logSource: LogSource = LogSource(this.getClass)

  def sendBlock[F[_]: Monad: NodeDiscovery: TransportLayer: Log: Time: ErrorHandler](
      b: BlockMessage): F[Unit] = {
    val serializedBlock = b.toByteString
    val hashString      = PrettyPrinter.buildString(b.blockHash)
    for {
      _ <- Log[F].info(s"CASPER: Beginning send of ${PrettyPrinter.buildString(b)} to peers...")
      _ <- sendToPeers[F](serializedBlock)
      _ <- Log[F].info(s"CASPER: Sent $hashString to peers")
    } yield ()
  }

  def sendBlockRequest[F[_]: Monad: NodeDiscovery: TransportLayer: Log: Time: ErrorHandler](
      r: BlockRequest): F[Unit] = {
    val serialized = r.toByteString
    val hashString = PrettyPrinter.buildString(r.hash)
    for {
      _ <- Log[F].info(s"CASPER: Beginning request of missing block $hashString from peers...")
      _ <- sendToPeers[F](serialized)
      _ <- Log[F].info(s"CASPER: Requested $hashString from peers")
    } yield ()
  }

  def sendToPeers[F[_]: Monad: NodeDiscovery: TransportLayer: Log: Time: ErrorHandler](
      serializedMessage: ByteString): F[Unit] =
    for {
      peers <- NodeDiscovery[F].peers
      local <- TransportLayer[F].local
      msg   = packet(local, serializedMessage)
      _     <- TransportLayer[F].broadcast(peers, msg)
    } yield ()

  def requestApprovedBlock[
      F[_]: Monad: Capture: MultiParentCasperConstructor: Log: Time: Metrics: TransportLayer: NodeDiscovery: ErrorHandler: PacketHandler]
    : F[Unit] = {
    val request = ApprovedBlockRequest("PleaseSendMeAnApprovedBlock").toByteString

    def askPeers(peers: List[PeerNode], local: PeerNode): F[Unit] = peers match {
      case peer :: rest =>
        for {
          _    <- Log[F].info(s"CASPER: Sending request for ApprovedBlock to $peer")
          send <- TransportLayer[F].roundTrip(peer, packet(local, request), 5.seconds)
          _ <- send match {
                case Left(err) =>
                  Log[F].info(s"CASPER: Failed to get response from $peer because: $err") *>
                    askPeers(rest, local)

                case Right(response) =>
                  Log[F]
                    .info(s"CASPER: Received response from $peer! Processing...")
                    .flatMap(_ => {
                      val maybeSender = ProtocolHelper.sender(response)
                      val maybePacket = toPacket(response).toOption

                      (maybeSender, maybePacket) match {
                        case (Some(sender), Some(_)) =>
                          Connect
                            .handlePacket[F](sender, maybePacket)
                            .flatMap(_ => {
                              MultiParentCasperConstructor[F].lastApprovedBlock.flatMap {
                                case Some(_) => ().pure[F] //valid ApprovedBlock received
                                case None    => askPeers(rest, local)
                              }
                            })
                        case (None, _) =>
                          Log[F].error(
                            s"CASPER: Response from $peer invalid. The sender of the message could not be determined.") *> askPeers(
                            rest,
                            local)
                        case (Some(_), None) =>
                          Log[F].error(
                            s"CASPER: Response from $peer invalid. A packet was expected, but received ${response.message}.") *> askPeers(
                            rest,
                            local)
                      }
                    })

              }
        } yield ()

      case Nil => ().pure[F]
    }

    for {
      a     <- MultiParentCasperConstructor[F].lastApprovedBlock
      peers <- NodeDiscovery[F].peers
      local <- TransportLayer[F].local
      _     <- a.fold(askPeers(peers.toList, local))(_ => ().pure[F])
    } yield ()
  }

  def casperPacketHandler[
      F[_]: Monad: MultiParentCasperConstructor: NodeDiscovery: TransportLayer: Log: Time: ErrorHandler](
      peer: PeerNode): PartialFunction[Packet, F[Option[Packet]]] =
    Function
      .unlift(
        (p: Packet) => {
          packetToBlockRequest(p) orElse packetToApprovedBlock(p) orElse packetToApprovedBlockRequest(
            p) orElse packetToBlockMessage(p)
        }
      )
      .andThen {
        case b @ (_: BlockMessage | _: BlockRequest) =>
          MultiParentCasperConstructor[F].casperInstance match {
            case Left(ex) =>
              Log[F]
                .warn(
                  "CASPER: a Casper message was received, " +
                    "however could not be processed due to the following error. " +
                    ex.getMessage
                )
                .map(_ => none[Packet])

            case Right(casper) =>
              implicit val casperEvidence = casper
              blockPacketHandler[F](peer, b)
          }

        case a: ApprovedBlock =>
          Log[F].info("CASPER: Received ApprovedBlock. Processing...") *>
            MultiParentCasperConstructor[F].receive(a).map(_ => none[Packet])

        case _: ApprovedBlockRequest =>
          for {
            _ <- Log[F].info(s"CASPER: Received ApprovedBlockRequest from $peer")
            a <- MultiParentCasperConstructor[F].lastApprovedBlock
          } yield a.map(b => Packet(b.toByteString))
      }

  def blockPacketHandler[
      F[_]: Monad: MultiParentCasper: NodeDiscovery: TransportLayer: Log: Time: ErrorHandler](
      peer: PeerNode,
      msg: scalapb.GeneratedMessage): F[Option[Packet]] =
    msg match {
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
          dag      <- MultiParentCasper[F].blockDag
          local    <- TransportLayer[F].local
          block    = dag.blockLookup.get(r.hash).map(_.toByteString)
          maybeMsg = block.map(serializedMessage => packet(local, serializedMessage))
          send     <- maybeMsg.traverse(msg => TransportLayer[F].send(peer, msg))
          hash     = PrettyPrinter.buildString(r.hash)
          logIntro = s"CASPER: Received request for block $hash from $peer. "
          _ <- send match {
                case None    => Log[F].info(logIntro + "No response given since block not found.")
                case Some(_) => Log[F].info(logIntro + "Response sent.")
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

  private def packetToApprovedBlock(msg: Packet): Option[ApprovedBlock] =
    Try(ApprovedBlock.parseFrom(msg.content.toByteArray)).toOption
      .filter(_.block.nonEmpty)

  private def packetToApprovedBlockRequest(msg: Packet): Option[ApprovedBlockRequest] =
    Try(ApprovedBlockRequest.parseFrom(msg.content.toByteArray)).toOption
      .filter(r => r.identifier.nonEmpty && r.identifier.forall(_.isLetterOrDigit))

  private def packetToBlockRequest(msg: Packet): Option[BlockRequest] =
    Try(BlockRequest.parseFrom(msg.content.toByteArray)).toOption
      .filter(r => r.base16Hash == Base16.encode(r.hash.toByteArray))
}
