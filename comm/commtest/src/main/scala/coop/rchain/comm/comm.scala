package coop.rchain.comm

import scala.collection.concurrent.TrieMap
import java.util.UUID

class Endpoint(val host: String, val port: Int) {
  override def toString = s"#{Endpoint $host:$port}"
  def format = s"$host:$port"

  lazy val inetSocketAddress = new java.net.InetSocketAddress(host, port)
  lazy val inetAddress = java.net.InetAddress.getByName(host)
}

object Endpoint {
  def fromString(hostport: String,
                 defaultHost: String = "*",
                 defaultPort: Int = 44444): Endpoint = {
    val stuff = hostport split ":"
    stuff.size match {
      case 1 =>
        new Endpoint(stuff(0), defaultPort)
      case 2 =>
        if (stuff(0) == "") new Endpoint("localhost", stuff(1) toInt)
        else new Endpoint(stuff(0), stuff(1) toInt)
    }
  }
}

case class Peer(id: UUID, endpoint: Endpoint)

trait Result

case class Response(data: Array[Byte]) extends Result
case class Error(message: String) extends Result

trait Comm {
  def send(data: Array[Byte])
  def sendTo(data: Array[Byte], id: UUID)
  def recv(): Result
  def addPeer(p: Peer): Unit
  def removePeer(p: Peer): Unit
  def removePeer(pid: UUID): Unit
  def getPeers(): Array[Peer]
  def peer(): Peer // Myself
}

class PeerMap[A] {
  val peerMap = new TrieMap[UUID, (Peer, A)]

  def add(p: Peer, a: A): Unit =
    peerMap.put(p.id, (p, a))

  def get(id: UUID): Option[(Peer, A)] = peerMap.get(id)
  def get(p: Peer): Option[(Peer, A)] = get(p.id)

  def remove(id: UUID): Option[(Peer, A)] = peerMap.remove(id)
  def remove(p: Peer): Option[(Peer, A)] = remove(p.id)

  def peers: Array[Peer] = peerMap map { case (_, (p, _)) => p } toArray

  def foreach(f: (Peer, A) => Unit) = peerMap foreach {
    case (_, (p, a)) =>
      f(p, a)
  }
}
