package coop.rchain.comm

sealed trait CommError
case class UnknownCommError(msg: String) extends CommError

case class DatagramSizeError(size: Int) extends CommError
case class DatagramFramingError(ex: Exception) extends CommError
case class DatagramException(ex: Exception) extends CommError

case class ProtocolException(ex: Exception) extends CommError
case class UnknownProtocolError(msg: String) extends CommError
