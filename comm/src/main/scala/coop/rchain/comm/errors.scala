package coop.rchain.comm

sealed trait CommError
final case class UnknownCommError(msg: String) extends CommError

final case class DatagramSizeError(size: Int)        extends CommError
final case class DatagramFramingError(ex: Exception) extends CommError
final case class DatagramException(ex: Exception)    extends CommError

final case class ProtocolException(ex: Exception)  extends CommError
final case class UnknownProtocolError(msg: String) extends CommError
