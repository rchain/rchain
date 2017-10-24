package coop.rchain.comm

import java.net.{DatagramPacket, DatagramSocket}
// import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

sealed trait DatagramError extends CommError
case class DatagramOverflow(size: Int) extends DatagramError
case class DatagramUnderflow(size: Int) extends DatagramError
case class DatagramFramingError(ex: Throwable) extends DatagramError
case class DatagramException(ex: Throwable) extends DatagramError

sealed trait DatagramError extends Throwable
case class DatagramOverflow(size: Int) extends DatagramError
case class DatagramUnderflow(size: Int) extends DatagramError
case class DatagramFramingError(size: Int) extends DatagramError

/**
  * Implement the Comm protocol for unicast (point-to-point) datagram
  * (UDP) sockets.
  *
  * Unconnected datagram sockets are simple to send and receive
  * through, though they do not guarantee delivery (even by detecting
  * delivery failure). They are also more difficult to combine use in
  * request-response protocols. Datagrams are limited in length and
  * require a framing protocol at the application (this) level to
  * recover correctly sized data.
  *
  * An instance of UnicastComm requires that its own location
  * (`local`) be given; this supplies the source data for datagrams it
  * sends.
  */
case class UnicastComm(local: PeerNode) extends Comm {
  val receiver = new DatagramSocket(local.endpoint.udpPort)
  val sender = new DatagramSocket()

  /*
   * Timeout for recv() calls; might need to be adjusted lower. This
   * is unrelated to timeout configurable for round-trip messages in a
   * ProtocolHandler.
   */
  receiver.setSoTimeout(500) // 500 ms

  val recv_buffer = new Array[Byte](65508)
  val recv_dgram = new DatagramPacket(recv_buffer, recv_buffer.size)

  /*
   * encode() and decode() make a naive framing protocol for dgrams,
   * since neither dgrams nor protocol buffers comes with its
   * own. Here, we simply encode the length <= 65506 in the first two
   * bytes of the dgram payload.
   */

  private def encode(data: Seq[Byte]): Either[DatagramError, Array[Byte]] = {
    val sz = data.size
    if (sz > 65506) {
      Left(DatagramOverflow(sz))
    } else {
      val payload = new Array[Byte](2 + sz)
      payload(0) = (sz >> 8).toByte
      payload(1) = sz.toByte
      Array.copy(data.toArray, 0, payload.toArray, 2, sz)
      Right(payload)
    }
  }

  private def decode(data: Array[Byte]): Either[DatagramError, Seq[Byte]] =
    if (data.size < 2) {
      Left(DatagramUnderflow(data.size))
    } else {
      (((data(0) << 8) & 0xff00) | (data(1) & 0xff)) match {
        case sz if sz < 0 => Left(DatagramUnderflow(sz))
        case sz if sz > 65506 => Left(DatagramOverflow(sz))
        case sz => Right(data.slice(2, sz + 2))
      }
    }

  /**
    * Receive a datagram.
    *
    * This method returns a @scala.util.Try, which may contain an
    * exception if the underlying socket receive
    * throws. See @java.net.DatagramSocket.receive.
    */
  override def recv: Either[CommError, Seq[Byte]] =
    try {
      receiver.receive(recv_dgram)
      decode(recv_dgram.getData) match {
        case Right(bytes) => Right(bytes)
        case Left(err)    => Left(err)
      }
    } catch {
      case NonFatal(ex) => Left(DatagramException(ex))
    }

  /**
    * Send data to a peer.
    *
    * Return Success(true) if no exception occurred, otherwise return
    * the exception in a @scala.util.Try. See
    * @java.net.DatagramSocket.send for a list of possible exceptions.
    */
  override def send(data: Seq[Byte], peer: PeerNode): Either[CommError, Unit] =
    encode(data) match {
      case Right(payload) => {
        println(s"COMM Sending to ${peer.endpoint.udpSocketAddress}")
        val dgram = new DatagramPacket(payload, 0, payload.size, peer.endpoint.udpSocketAddress)
        try {
          sender.send(dgram)
          Right(())
        } catch {
          case NonFatal(ex) => Left(DatagramException(ex))
        }
      }
      case Left(err) => Left(err)
    }
}
