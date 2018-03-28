package coop.rchain.comm

import cats._, cats.data._, cats.implicits._

// TODO we need lower level errors and general error, for now all in one place
sealed trait CommError
final case class UnknownCommError(msg: String)         extends CommError
final case class DatagramSizeError(size: Int)          extends CommError
final case class DatagramFramingError(ex: Exception)   extends CommError
final case class DatagramException(ex: Exception)      extends CommError
final case object HeaderNotAvailable                   extends CommError
final case class ProtocolException(th: Throwable)      extends CommError
final case class UnknownProtocolError(msg: String)     extends CommError
final case class PublicKeyNotAvailable(node: PeerNode) extends CommError
final case class ParseError(msg: String)               extends CommError
final case object EncryptionHandshakeIncorrectlySigned extends CommError
final case object BootstrapNotProvided                 extends CommError
final case class PeerNodeNotFound(peer: PeerNode)      extends CommError
// TODO add Show instance

object CommError {

  type CommErrT[F[_], A] = EitherT[F, CommError, A]
  type CommErr[A]        = Either[CommError, A]

  def unknownProtocol(msg: String): CommError          = UnknownProtocolError(msg)
  def parseError(msg: String): CommError               = ParseError(msg)
  def protocolException(th: Throwable): CommError      = ProtocolException(th)
  def headerNotAvailable: CommError                    = HeaderNotAvailable
  def peerNodeNotFound(peer: PeerNode): CommError      = PeerNodeNotFound(peer)
  def publicKeyNotAvailable(peer: PeerNode): CommError = PublicKeyNotAvailable(peer)
}
