package coop.rchain.comm.discovery

import cats._
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.catscontrib._
import coop.rchain.catscontrib.ski._
import coop.rchain.comm.CachedConnections.ConnectionsCache
import coop.rchain.comm._
import coop.rchain.metrics.Metrics
import coop.rchain.shared.{Log, LogSource}
import io.grpc._
import io.grpc.netty._
import monix.eval._
import monix.execution._

import scala.concurrent.Future
import scala.concurrent.duration._

class GrpcKademliaRPC(
    src: PeerNode,
    port: Int,
    timeout: FiniteDuration,
    connectionsCache: ConnectionsCache[Task]
)(
    implicit
    scheduler: Scheduler,
    metrics: Metrics[Task],
    log: Log[Task]
) extends KademliaRPC[Task] {

  private implicit val logSource: LogSource = LogSource(this.getClass)

  private val connections = connectionsCache(clientChannel)

  import connections.connection

  def ping(peer: PeerNode): Task[Boolean] =
    for {
      _       <- Metrics[Task].incrementCounter("protocol-ping-sends")
      channel <- connection(peer, enforce = false)
      pongErr <- Task
                  .fromFuture {
                    KademliaRPCServiceGrpc
                      .stub(channel)
                      .sendPing(Ping().withSender(node(src)))
                  }
                  .nonCancelingTimeout(timeout)
                  .attempt
      _ <- Task.delay(channel.shutdown())
    } yield pongErr.fold(kp(false), kp(true))

  def lookup(key: Seq[Byte], peer: PeerNode): Task[Seq[PeerNode]] =
    for {
      _ <- Metrics[Task].incrementCounter("protocol-lookup-send")
      lookup = Lookup()
        .withId(ByteString.copyFrom(key.toArray))
        .withSender(node(src))
      channel <- connection(peer, enforce = false)
      responseErr <- Task
                      .fromFuture {
                        KademliaRPCServiceGrpc.stub(channel).sendLookup(lookup)
                      }
                      .nonCancelingTimeout(timeout)
                      .attempt
      _ <- Task.delay(channel.shutdown())
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
        .addService(
          KademliaRPCServiceGrpc
            .bindService(new SimpleKademliaRPCService[Task](pingHandler, lookupHandler), scheduler)
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

  class SimpleKademliaRPCService[F[_]: Futurable: Functor](
      pingHandler: PeerNode => F[Unit],
      lookupHandler: (PeerNode, Array[Byte]) => F[Seq[PeerNode]]
  ) extends KademliaRPCServiceGrpc.KademliaRPCService {
    def sendLookup(lookup: Lookup): Future[LookupResponse] = {
      val id               = lookup.id.toByteArray
      val sender: PeerNode = toPeerNode(lookup.sender.get)
      lookupHandler(sender, id)
        .map(peers => LookupResponse().withNodes(peers.map(node)))
        .toFuture
    }
    def sendPing(ping: Ping): Future[Pong] = {
      val sender: PeerNode = toPeerNode(ping.sender.get)
      pingHandler(sender).as(Pong()).toFuture
    }
  }

}
