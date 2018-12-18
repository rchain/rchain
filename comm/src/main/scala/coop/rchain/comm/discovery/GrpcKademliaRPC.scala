package coop.rchain.comm.discovery

import scala.concurrent.duration._
import scala.util.Try

import cats.implicits._

import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.catscontrib.ski._
import coop.rchain.comm._
import coop.rchain.comm.CachedConnections.ConnectionsCache
import coop.rchain.comm.discovery.KademliaGrpcMonix.KademliaRPCServiceStub
import coop.rchain.metrics.Metrics
import coop.rchain.shared.{Log, LogSource}

import com.google.protobuf.ByteString
import io.grpc._
import io.grpc.netty._
import monix.eval._
import monix.execution._

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

  import connections.cell

  def ping(peer: PeerNode): Task[Boolean] =
    for {
      _       <- Metrics[Task].incrementCounter("protocol-ping-sends")
      local   <- peerNodeAsk.ask
      ping    = Ping().withSender(node(local))
      pongErr <- withClient(peer)(_.sendPing(ping).nonCancelingTimeout(timeout)).attempt
    } yield pongErr.fold(kp(false), kp(true))

  def lookup(key: Seq[Byte], peer: PeerNode): Task[Seq[PeerNode]] =
    for {
      _           <- Metrics[Task].incrementCounter("protocol-lookup-send")
      local       <- peerNodeAsk.ask
      lookup      = Lookup().withId(ByteString.copyFrom(key.toArray)).withSender(node(local))
      responseErr <- withClient(peer)(_.sendLookup(lookup).nonCancelingTimeout(timeout)).attempt
    } yield responseErr.fold(kp(Seq.empty[PeerNode]), _.nodes.map(toPeerNode))

  def disconnect(peer: PeerNode): Task[Unit] =
    cell.modify { s =>
      for {
        _ <- s.connections.get(peer) match {
              case Some(c) =>
                log
                  .debug(s"Disconnecting from peer ${peer.toAddress}")
                  .map(kp(Try(c.shutdown())))
                  .void
              case _ => Task.unit // ignore if connection does not exists already
            }
      } yield s.copy(connections = s.connections - peer)
    }

  private def withClient[A](peer: PeerNode, enforce: Boolean = false)(
      f: KademliaRPCServiceStub => Task[A]
  ): Task[A] =
    for {
      channel <- connections.connection(peer, enforce)
      stub    <- Task.delay(KademliaGrpcMonix.stub(channel))
      result <- f(stub).doOnFinish {
                 case Some(_) => disconnect(peer)
                 case _       => Task.unit
               }
      _ <- Task.unit.asyncBoundary // return control to caller thread
    } yield result

  def receive(
      pingHandler: PeerNode => Task[Unit],
      lookupHandler: (PeerNode, Array[Byte]) => Task[Seq[PeerNode]]
  ): Task[Unit] =
    cell.modify { s =>
      Task.delay {
        val server = NettyServerBuilder
          .forPort(port)
          .executor(scheduler)
          .addService(
            KademliaGrpcMonix
              .bindService(new SimpleKademliaRPCService(pingHandler, lookupHandler), scheduler)
          )
          .build
          .start

        val c: Cancelable = () => server.shutdown().awaitTermination()
        s.copy(server = Some(c))
      }
    }

  def shutdown(): Task[Unit] = {
    def shutdownServer: Task[Unit] = cell.modify { s =>
      for {
        _ <- log.info("Shutting down Kademlia RPC server")
        _ <- s.server.fold(Task.unit)(server => Task.delay(server.cancel()))
      } yield s.copy(server = None, shutdown = true)
    }

    def disconnectFromPeers: Task[Unit] =
      for {
        peers <- cell.read.map(_.connections.keys.toSeq)
        _     <- log.info("Disconnecting from all peers")
        _     <- Task.gatherUnordered(peers.map(disconnect))
      } yield ()

    cell.read.flatMap { s =>
      if (s.shutdown) Task.unit
      else shutdownServer *> disconnectFromPeers
    }
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
