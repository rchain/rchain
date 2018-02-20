package coop.rchain.comm

// TODO we need lower level errors and general error, for now all in one place
sealed trait CommError
final case class UnknownCommError(msg: String)         extends CommError
final case class DatagramSizeError(size: Int)          extends CommError
final case class DatagramFramingError(ex: Exception)   extends CommError
final case class DatagramException(ex: Exception)      extends CommError
final case object HeaderNotAvailable                   extends CommError
final case class ProtocolException(ex: Exception)      extends CommError
final case class UnknownProtocolError(msg: String)     extends CommError
final case object KeysNotAvailable                     extends CommError
final case class ParseError(msg: String)               extends CommError
final case object EncryptionHandshakeIncorrectlySigned extends CommError
// TODO add Show instance
