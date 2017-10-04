package coop.rchain.comm

import java.net.{DatagramPacket, DatagramSocket}
import java.util.UUID

object UnicastComm {}

class UnicastComm(p: Peer) extends Comm {
  val peers = new PeerMap[Any]
  val myself = new Peer(p.id, new Endpoint("localhost", p.endpoint.port))
  lazy val receiver = new DatagramSocket(p.endpoint.port)
  lazy val sender = new DatagramSocket()

  val recv_buffer = new Array[Byte](65508)
  val recv_dgram = new DatagramPacket(recv_buffer, recv_buffer.size)

  /*
   *  encode() and decode() make a naive framing protocol for dgrams,
   *  since neither dgrams nor protocol buffers comes with its
   *  own. Here, we simply encode the length <= 65506 in the first two
   *  bytes of the dgram payload.
   */

  def encode(data: Array[Byte]): Array[Byte] = {
    // TODO: Bounds check
    val sz = data.size
    println(f"encode() length ${sz} (0x${sz}%x)")
    val payload = new Array[Byte](2 + sz)
    payload(0) = ((sz >>> 8) & 0xff).asInstanceOf[Byte]
    payload(1) = (sz & 0xff).asInstanceOf[Byte]
    Array.copy(data, 0, payload, 2, sz)
    payload
  }

  def decode(data: Array[Byte]): Array[Byte] = {
    // TODO: Bounds check
    val sz = (data(0) << 8) + (data(1) & 0xff)
    println(f"decode() length ${sz} (0x${sz}%x)")
    data.slice(2, sz + 2)
  }

  override def recv(): Result = {
    receiver.receive(recv_dgram)
    Response(decode(recv_dgram.getData))
  }

  override def send(data: Array[Byte]) = {
    val sz = data.size
    // TODO: ensure sz <= 65506
    val payload = encode(data)
    val dgram = new DatagramPacket(payload, payload.size)
    peers foreach { (p, _) =>
      {
        dgram.setSocketAddress(p.endpoint inetSocketAddress)
        sender.send(dgram)
      }
    }
  }

  override def sendTo(data: Array[Byte], id: UUID) =
    peers.get(id) match {
      case Some((p, _)) => {
        val payload = encode(data)
        val dgram = new DatagramPacket(payload,
                                       0,
                                       payload.size,
                                       p.endpoint inetSocketAddress)
        sender.send(dgram)
      }
      case _ => ()
    }

  override def addPeer(p: Peer) = peers.add(p, null)
  override def getPeers = peers.peers
  override def removePeer(pid: UUID) = peers.remove(pid)
  override def removePeer(p: Peer) = peers.remove(p)
  override def peer = myself
}
