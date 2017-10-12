package coop.rchain.comm

import org.rogach.scallop._
import coop.rchain.kv._

import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue}
import java.util.UUID

object Defaults {
  val listenPort = 44444
  val transport = Some("zeromq")
  val listen = Some(s"*:$listenPort")
  val httpPort = Some(8878)
}

class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
  version("0.0.1 rchain blah blah")

  // These keywords denote implemented transports
  val validTransports = Set("zeromq", "netty")

  val transport = opt[String](
    default = Defaults.transport,
    short = 't',
    descr = "Transport mechanism to use; one of: " + (validTransports mkString ", "))

  validate(transport) { t =>
    if (validTransports contains t) Right(Unit)
    else
      Left(
        s"Bad transport: $t (must be one of: " + (validTransports mkString ", ") + ")")
  }

  val listen = opt[String](
    default = Defaults.listen,
    short = 'p',
    descr = "Address (host:port) on which transport should listen.")

  // Defer analysis of these host:port values until later; it's hard
  // to verify statically
  val peers = opt[String](
    default = None,
    descr = "Comma-separated list of peer nodes in host:port format.")

  val home =
    opt[String](default = None, descr = "Home address for initial seed.")

  val httpPort = opt[Int](default = Defaults.httpPort,
                          validate = (0 <),
                          short = 'H',
                          descr = "Port on which HTTP server should listen.")

  verify
}

class Receiver(comm: Comm, commands: BlockingQueue[Protocol]) extends Thread {
  override def run(): Unit =
    while (true) {
      val stuff = comm.recv()
      stuff match {
        case Response(d) => {
          println(s"Received: " + new String(d))
          try {
            commands add (Protocol parseFrom d)
          } catch {
            case e: Exception => e.printStackTrace
          }
        }
        case Error(e) => println(s"Error: $e")
      }
    }
}

object CommTest {
  def makeEndpoint(spec: String) =
    EndpointFactory.fromString(spec, defaultPort = Defaults.listenPort)

  def bootstrap(me: UUID, comm: Comm, listen: Endpoint, home: Endpoint) = {
    val homeId = UUID.randomUUID
    val peer = new Peer(homeId, home)
    comm.addPeer(peer)
    val factory = new MessageFactory(me)
    val buf = new java.io.ByteArrayOutputStream

    factory.protocol.withHello(
      factory.hello.withNode(factory.node(new Peer(me, listen)))) writeTo buf
    comm.sendTo(buf.toByteArray, homeId)

    buf.reset
    factory.protocol.withGetPeers(factory.getPeers) writeTo buf
    comm.sendTo(buf.toByteArray, homeId)

    buf.reset
    factory.protocol.withGetBlocks(factory.getBlocks) writeTo buf
    comm.sendTo(buf.toByteArray, homeId)

    buf.reset
    factory.protocol.withDisconnect(factory.disconnect) writeTo buf
    comm.sendTo(buf.toByteArray, homeId)

    comm.removePeer(peer)
  }

  def main(args: Array[String]) {
    val conf = new Conf(args)

    val listen = makeEndpoint(conf.listen())

    val peers =
      if (conf.peers.isSupplied) {
        (conf.peers() split ",") filter { _ != "" } map makeEndpoint
      } else {
        new Array[Endpoint](0)
      }

    val store = new KeyValueStore

    println(conf.summary)

    val me = UUID.randomUUID
    println(s"I am $me")

    val cmdQueue = new java.util.concurrent.LinkedBlockingQueue[Protocol]

    val comm =
      conf.transport() match {
        case "zeromq" =>
          new ZeromqComm(new Peer(me, listen))
        case "netty" =>
          new NettyComm(new Peer(me, listen))
      }

    peers foreach { p =>
      comm.addPeer(new Peer(UUID.randomUUID, p))
    }

    val messageHandler = new MessageHandler(me, comm, store, cmdQueue)
    messageHandler start

    val receiver = new Receiver(comm, cmdQueue)
    receiver start

    if (conf.home.isSupplied) {
      val addy = makeEndpoint(s"localhost:${listen port}") // Replace with public IP
      println(s"Homing from $addy.")
      bootstrap(me, comm, addy, makeEndpoint(conf.home()))
    }

    val http = new HttpServer(conf.httpPort(), messageHandler)
    http start

    sys.addShutdownHook({
      val factory = new MessageFactory(me)
      val buf = new java.io.ByteArrayOutputStream
      factory.protocol.withDisconnect(factory.disconnect) writeTo buf
      comm.send(buf.toByteArray)
      println("disconnected")
    })
  }
}
