package coop.rchain.comm

import java.nio.file._

import cats.data._

import coop.rchain.catscontrib._
import coop.rchain.comm.protocol.routing._

// TODO we need lower level errors and general error, for now all in one place
// TODO cleanup unused errors (UDP trash)
sealed trait CommError
final case class UnknownCommError(msg: String)                      extends CommError
final case class DatagramSizeError(size: Int)                       extends CommError
final case class DatagramFramingError(ex: Exception)                extends CommError
final case class DatagramException(ex: Exception)                   extends CommError
final case object HeaderNotAvailable                                extends CommError
final case class ProtocolException(th: Throwable)                   extends CommError
final case class UnknownProtocolError(msg: String)                  extends CommError
final case class PublicKeyNotAvailable(node: PeerNode)              extends CommError
final case class ParseError(msg: String)                            extends CommError
final case object EncryptionHandshakeIncorrectlySigned              extends CommError
final case object BootstrapNotProvided                              extends CommError
final case class PeerNodeNotFound(peer: PeerNode)                   extends CommError
final case class PeerUnavailable(peer: PeerNode)                    extends CommError
final case class WrongNetwork(peer: PeerNode, msg: String)          extends CommError
final case class MessageToLarge(peer: PeerNode)                     extends CommError
final case class MalformedMessage(pm: Protocol)                     extends CommError
final case object CouldNotConnectToBootstrap                        extends CommError
final case class InternalCommunicationError(msg: String)            extends CommError
final case object TimeOut                                           extends CommError
final case object UpstreamNotAvailable                              extends CommError
final case class UnexpectedMessage(msgStr: String)                  extends CommError
final case object SenderNotAvailable                                extends CommError
final case class PongNotReceivedForPing(peer: PeerNode)             extends CommError
final case class UnableToStorePacket(packet: Packet, th: Throwable) extends CommError
final case class UnableToRestorePacket(key: String, th: Throwable)  extends CommError
// TODO add Show instance

object CommError {

  type CommErrT[F[_], A] = EitherT[F, CommError, A]
  type CommErr[A]        = Either[CommError, A]

  def unknownCommError(msg: String): CommError             = UnknownCommError(msg)
  def unknownProtocol(msg: String): CommError              = UnknownProtocolError(msg)
  def parseError(msg: String): CommError                   = ParseError(msg)
  def protocolException(th: Throwable): CommError          = ProtocolException(th)
  def headerNotAvailable: CommError                        = HeaderNotAvailable
  def peerNodeNotFound(peer: PeerNode): CommError          = PeerNodeNotFound(peer)
  def peerUnavailable(peer: PeerNode): CommError           = PeerUnavailable(peer)
  def wrongNetwork(peer: PeerNode, msg: String): CommError = WrongNetwork(peer, msg)
  def messageToLarge(peer: PeerNode): CommError            = MessageToLarge(peer)
  def publicKeyNotAvailable(peer: PeerNode): CommError     = PublicKeyNotAvailable(peer)
  def couldNotConnectToBootstrap: CommError                = CouldNotConnectToBootstrap
  def internalCommunicationError(msg: String): CommError   = InternalCommunicationError(msg)
  def malformedMessage(pm: Protocol): CommError            = MalformedMessage(pm)
  def upstreamNotAvailable: CommError                      = UpstreamNotAvailable
  def unexpectedMessage(msgStr: String): CommError         = UnexpectedMessage(msgStr)
  def senderNotAvailable: CommError                        = SenderNotAvailable
  def pongNotReceivedForPing(peer: PeerNode): CommError    = PongNotReceivedForPing(peer)
  def timeout: CommError                                   = TimeOut
  def unableToStorePacket(packet: Packet, th: Throwable): CommError =
    UnableToStorePacket(packet, th)
  def unableToRestorePacket(key: String, th: Throwable) = UnableToRestorePacket(key, th)

  def errorMessage(ce: CommError): String =
    ce match {
      case PeerUnavailable(_) => "Peer is currently unavailable"
      case MessageToLarge(p)  => s"Message rejected by peer $p because it was too large"
      case PongNotReceivedForPing(_) =>
        "Peer is behind a firewall and can't be accessed from outside"
      case CouldNotConnectToBootstrap      => "Node could not connect to bootstrap node"
      case TimeOut                         => "Timeout"
      case InternalCommunicationError(msg) => s"Internal communication error. $msg"
      case UnknownProtocolError(msg)       => s"Unknown protocol error. $msg"
      case UnableToStorePacket(p, er) =>
        s"Could not serialize packet $p. Error message: ${er.getMessage}"
      case UnableToRestorePacket(p, er) =>
        s"Could not deserialize packet $p. Error message: ${er.getMessage}"
      case ProtocolException(t) =>
        val msg = Option(t.getMessage).getOrElse("")
        s"Protocol error. $msg"
      case _ => ce.toString
    }

  implicit class CommErrorToMessage(commError: CommError) {
    val message: String = CommError.errorMessage(commError)
  }
}
