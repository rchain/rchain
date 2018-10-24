package coop.rchain.comm.discovery

import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.catscontrib.ski._
import coop.rchain.comm.CachedConnections.ConnectionsCache
import coop.rchain.comm._
import coop.rchain.metrics.Metrics
import coop.rchain.shared.{Log, LogSource}
import io.grpc._
import io.grpc.netty._
import monix.eval._
import monix.execution._

import scala.concurrent.duration._

class GrpcKademliaRPC(port: Int, timeout: FiniteDuration)(
    implicit
    scheduler: Scheduler,
    peerNodeAsk: PeerNodeAsk[Task],
    metrics: Metrics[Task],
    log: Log[Task],
    connectionsCache: ConnectionsCache[Task, KademliaConnTag]
) extends KademliaRPC[Task] {

  private implicit val logSource: LogSource = LogSource(this.getClass)

  private val connections = connectionsCache(clientChannel)

  import connections.connection

  def ping(peer: PeerNode): Task[Boolean] =
    for {
      _       <- Metrics[Task].incrementCounter("protocol-ping-sends")
      channel <- connection(peer, enforce = false)
      local   <- peerNodeAsk.ask
      pongErr <- KademliaGrpcMonix
                  .stub(channel)
                  .sendPing(Ping().withSender(node(local)))
                  .nonCancelingTimeout(timeout)
                  .attempt
      _ <- Task.delay(channel.shutdown())
      _ <- Task.unit.asyncBoundary // return control to caller thread
    } yield pongErr.fold(kp(false), kp(true))

  def lookup(key: Seq[Byte], peer: PeerNode): Task[Seq[PeerNode]] =
    for {
      _     <- Metrics[Task].incrementCounter("protocol-lookup-send")
      local <- peerNodeAsk.ask
      lookup = Lookup()
        .withId(ByteString.copyFrom(key.toArray))
        .withSender(node(local))
      channel <- connection(peer, enforce = false)
      responseErr <- KademliaGrpcMonix
                      .stub(channel)
                      .sendLookup(lookup)
                      .nonCancelingTimeout(timeout)
                      .attempt
      _ <- Task.delay(channel.shutdown())
      _ <- Task.unit.asyncBoundary // return control to caller thread
    } yield
      responseErr.fold(
        kp(Seq.empty[PeerNode]),
        lr => lr.nodes.map(toPeerNode)
      )

  def receive(
      pingHandler: PeerNode => Task[Unit],
      lookupHandler: (PeerNode, Array[Byte]) => Task[Seq[PeerNode]]
  ): Task[Unit] =
    Task.delay {
      NettyServerBuilder
        .forPort(port)
        .executor(scheduler)
        .addService(
          KademliaGrpcMonix
            .bindService(new SimpleKademliaRPCService(pingHandler, lookupHandler), scheduler)
        )
        .build
        .start
    }

  private def clientChannel(peer: PeerNode): Task[ManagedChannel] =
    for {
      _ <- log.debug(s"Creating new channel to peer ${peer.toAddress}")
      c <- Task.delay {
            NettyChannelBuilder
              .forAddress(peer.endpoint.host, peer.endpoint.udpPort)
              .executor(scheduler)
              .usePlaintext()
              .build()
          }
    } yield c

  private def node(n: PeerNode): Node =
    Node()
      .withId(ByteString.copyFrom(n.key.toArray))
      .withHost(ByteString.copyFromUtf8(n.endpoint.host))
      .withUdpPort(n.endpoint.udpPort)
      .withTcpPort(n.endpoint.tcpPort)

  private def toPeerNode(n: Node): PeerNode =
    PeerNode(NodeIdentifier(n.id.toByteArray), Endpoint(n.host.toStringUtf8, n.tcpPort, n.udpPort))

  class SimpleKademliaRPCService(
      pingHandler: PeerNode => Task[Unit],
      lookupHandler: (PeerNode, Array[Byte]) => Task[Seq[PeerNode]]
  ) extends KademliaGrpcMonix.KademliaRPCService {
    def sendLookup(lookup: Lookup): Task[LookupResponse] = {
      val id               = lookup.id.toByteArray
      val sender: PeerNode = toPeerNode(lookup.sender.get)
      lookupHandler(sender, id)
        .map(peers => LookupResponse().withNodes(peers.map(node)))
    }
    def sendPing(ping: Ping): Task[Pong] = {
      val sender: PeerNode = toPeerNode(ping.sender.get)
      pingHandler(sender).as(Pong())
    }
  }

}
