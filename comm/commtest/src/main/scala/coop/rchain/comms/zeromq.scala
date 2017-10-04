package coop.rchain.comm

import org.zeromq.ZMQ

import scala.collection.concurrent.TrieMap
import java.util.UUID

object ZeromqComm {
  lazy val context = {
    ZMQ.context(1)
  }
}

class ZeromqComm(p: Peer) extends Comm {

  /*
   *  myself represents the connection point for others; it's the
   *  host:port that others use to connect to this node and will
   *  include data like the public IP address or domain name, etc.
   */
  val myself = new Peer(p.id, new Endpoint("localhost", p.endpoint.port))

  val peers = new TrieMap[UUID, (Peer, ZMQ.Socket)]

  /*
   * Receiving stuff
   */

  lazy val receiver = {
    val uri = s"tcp://${p.endpoint format}"
    val s = ZeromqComm.context.socket(ZMQ.PULL)
    s.bind(uri)
    println(s"Bound sock $uri.")
    s
  }

  override def recv(): Result = Response(receiver.recv(0))

  /*
   * Sending stuff
   */

  override def send(data: Array[Byte]) =
    peers foreach {
      case (id, (p, sock)) =>
        sock.send(data, ZMQ.DONTWAIT) match {
          case false => Error("Couldn't send to ${id toString}")
          case _     => Response((s"Sent $data: " getBytes) ++ data)
        }
    }

  override def sendTo(data: Array[Byte], id: UUID) =
    peers.get(id) match {
      case Some((p, sock)) => {
        sock.send(data, ZMQ.DONTWAIT)
      }
      case None => ()
    }

  /*
   *  Maintaining the peer map.
   */

  override def addPeer(p: Peer) = {
    val s = ZeromqComm.context.socket(ZMQ.PUSH)
    val uri = s"tcp://${p.endpoint format}"
    s.connect(uri)
    peers.put(p.id, (p, s))
  }

  override def removePeer(p: Peer) = removePeer(p.id)

  override def removePeer(id: UUID) =
    peers.remove(id)

  override def getPeers: Array[Peer] =
    peers map { case (id, (p, sock)) => p } toArray

  override def peer = myself
}
