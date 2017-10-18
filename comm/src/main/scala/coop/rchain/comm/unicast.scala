package coop.rchain.comm

import java.net.{DatagramPacket, DatagramSocket}
import scala.util.{Failure, Success, Try}

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
case class UnicastComm(val local: PeerNode) extends Comm {
  lazy val receiver = new DatagramSocket(local.endpoint.udpPort)
  lazy val sender = new DatagramSocket()

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

  private def encode(data: Seq[Byte]): Try[Array[Byte]] = {
    val sz = data.size
    if (sz > 65506) {
      Failure(new Exception(s"Datagram too large to encode (got $sz)."))
    } else {
      val payload = new Array[Byte](2 + sz)
      payload(0) = (sz >> 8).toByte
      payload(1) = sz.toByte
      Array.copy(data.toArray, 0, payload.toArray, 2, sz)
      Success(payload)
    }
  }

  private def decode(data: Array[Byte]): Try[Seq[Byte]] =
    if (data.size < 2) {
      Failure(new Exception(s"Datagram too small (got ${data.size})."))
    } else {
      val sz = ((data(0) << 8) & 0xff00) | (data(1) & 0xff)
      if (0 < sz && sz <= 65506) {
        Success(data.slice(2, sz + 2))
      } else {
        Failure(new Exception(s"Datagram size protocol error (got $sz)."))
      }
    }

  /**
    * Receive a datagram.
    *
    * This method returns a @scala.util.Try, which may contain an
    * exception if the underlying socket receive
    * throws. See @java.net.DatagramSocket.receive.
    */
  override def recv: Try[Seq[Byte]] =
    try {
      receiver.receive(recv_dgram)
      decode(recv_dgram.getData)
    } catch {
      case ex: Throwable => Failure(ex)
    }

  /**
    * Send data to a peer.
    *
    * Return Success(true) if no exception occurred, otherwise return
    * the exception in a @scala.util.Try. See
    * @java.net.DatagramSocket.send for a list of possible exceptions.
    */
  override def send(data: Seq[Byte], peer: PeerNode): Try[Boolean] =
    encode(data) match {
      case Success(payload) => {
        println(s"COMM Sending to ${peer.endpoint.udpSocketAddress}")
        val dgram = new DatagramPacket(payload, 0, payload.size, peer.endpoint.udpSocketAddress)
        sender.send(dgram)
        return Success(true)
      }
      case Failure(ex) => Failure(ex)
    }
}
