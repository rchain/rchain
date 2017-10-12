package coop.rchain.comm

class Endpoint(val host: String, val port: Int) {
  override def toString = s"#{Endpoint $host:$port}"
  def format = s"$host:$port"

  def toInetSocketAddress: java.net.InetSocketAddress =
    new java.net.InetSocketAddress(host, port)
}

object EndpointFactory {
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

case class Peer(id: java.util.UUID, endpoint: Endpoint)

trait Result

case class Response(data: Array[Byte]) extends Result
case class Error(message: String) extends Result

trait Comm {
  def send(data: Array[Byte])
  def sendTo(data: Array[Byte], id: java.util.UUID)
  def recv(): Result
  def addPeer(p: Peer): Unit
  def removePeer(p: Peer): Unit
  def removePeer(pid: java.util.UUID): Unit
  def getPeers(): Array[Peer]
  def peer(): Peer // Myself
}
