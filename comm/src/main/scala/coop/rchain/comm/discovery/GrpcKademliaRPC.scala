package coop.rchain.comm.discovery

import cats._, cats.data._, cats.implicits._, cats.mtl._
import coop.rchain.catscontrib._, Catscontrib._, ski._, TaskContrib._
import coop.rchain.comm._
import monix.eval._
import monix.execution.atomic._
import monix.execution._
import coop.rchain.metrics.Metrics
import coop.rchain.shared.{Cell, Log, LogSource}
import scala.concurrent.Future
import io.grpc._, io.grpc.netty._
import com.google.protobuf.ByteString
import scala.concurrent.duration._
import com.google.protobuf.ByteString

class GrpcKademliaRPC(localPeerNode: LocalPeerNode, port: Int, timeout: FiniteDuration)(
    implicit
    scheduler: Scheduler,
    metrics: Metrics[Task],
    log: Log[Task]
) extends KademliaRPC[Task] {

  private implicit val logSource: LogSource = LogSource(this.getClass)

  def ping(peer: PeerNode): Task[Boolean] =
    for {
      _       <- Metrics[Task].incrementCounter("protocol-ping-sends")
      channel <- clientChannel(peer)
      pongErr <- KademliaGrpcMonix
                  .stub(channel)
                  .sendPing(Ping().withSender(node(localPeerNode())))
                  .nonCancelingTimeout(timeout)
                  .attempt
      _ <- Task.delay(channel.shutdown())
      _ <- Task.unit.asyncBoundary // return control to caller thread
    } yield pongErr.fold(kp(false), kp(true))

  def lookup(key: Seq[Byte], peer: PeerNode): Task[Seq[PeerNode]] =
    for {
      _ <- Metrics[Task].incrementCounter("protocol-lookup-send")
      lookup = Lookup()
        .withId(ByteString.copyFrom(key.toArray))
        .withSender(node(localPeerNode()))
      channel <- clientChannel(peer)
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
