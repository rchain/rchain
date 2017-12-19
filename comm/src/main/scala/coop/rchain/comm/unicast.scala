package coop.rchain.comm

import java.net.{DatagramPacket, DatagramSocket, SocketAddress}
import scala.util.control.NonFatal

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
case class UnicastComm(local: PeerNode) extends Comm[SocketAddress] {
  println(s"LOCAL: ${local.endpoint.udpSocketAddress}")

  // val receiver = new DatagramSocket(local.endpoint.udpPort)
  // val sender = new DatagramSocket(local.endpoint.udpSocketAddress)

  // val receiver = new DatagramSocket(local.endpoint.udpPort)
  val sender = new DatagramSocket(local.endpoint.udpPort)

  /*
   * Timeout for recv() calls; might need to be adjusted lower. This
   * is unrelated to timeout configurable for round-trip messages in a
   * ProtocolHandler.
   */
  // receiver.setSoTimeout(500) // 500 ms
  sender.setSoTimeout(500) // 500 ms

  val recv_buffer = new Array[Byte](65508)
  val recv_dgram = new DatagramPacket(recv_buffer, recv_buffer.size)

  /*
   * encode() and decode() make a naive framing protocol for dgrams,
   * since neither dgrams nor protocol buffers comes with its
   * own. Here, we simply encode the length <= 65506 in the first two
   * bytes of the dgram payload.
   */

  private def encode(data: Seq[Byte]): Either[CommError, Array[Byte]] = {
    val sz = data.size
    if (sz > 65506) {
      Left(DatagramSizeError(sz))
    } else {
      val payload = new Array[Byte](2 + sz)
      payload(0) = (sz >> 8).toByte
      payload(1) = sz.toByte
      Array.copy(data.toArray, 0, payload.toArray, 2, sz)
      Right(payload)
    }
  }

  private def decode(data: Array[Byte]): Either[CommError, Seq[Byte]] =
    if (data.size < 2) {
      Left(DatagramSizeError(data.size))
    } else {
      (((data(0) << 8) & 0xff00) | (data(1) & 0xff)) match {
        case sz if (sz < 0 || 65506 < sz) => Left(DatagramSizeError(sz))
        case sz                           => Right(data.slice(2, sz + 2))
      }
    }

  /**
    * Receive a datagram.
    *
    * Returns `Right` with the bytes read from the socket or Left with
    * an error, if something went wrong.
    */
  override def recv: Either[CommError, (SocketAddress, Seq[Byte])] =
    try {
      // receiver.receive(recv_dgram)
      sender.receive(recv_dgram)
      // println(s"RECV from ${recv_dgram.getSocketAddress}")
      // sender.send(new DatagramPacket("FOO".getBytes, 0, 3, recv_dgram.getSocketAddress))
      decode(recv_dgram.getData) match {
        case Right(data) => Right((recv_dgram.getSocketAddress, data))
        case Left(err) => Left(err)
      }
    } catch {
      case NonFatal(ex: Exception) => Left(DatagramException(ex))
    }

  /**
    * Send data to a peer.
    *
    * Returns `Right` with `true`, if no error was detected.
    * Otherwise, returns `Left` with the error.
    */
  override def send(data: Seq[Byte], peer: PeerNode): Either[CommError, Unit] =
    encode(data).flatMap { payload =>
      val dgram = new DatagramPacket(payload, 0, payload.size, peer.endpoint.udpSocketAddress)
      try {
        // println(s"SEND from ${sender.getLocalSocketAddress} -> ${dgram.getSocketAddress}")
        sender.send(dgram)
        Right(())
      } catch {
        case NonFatal(ex: Exception) => Left(DatagramException(ex))
      }
    }
}
