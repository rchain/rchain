package coop.rchain.comm.discovery

import scala.concurrent.duration._
import scala.util.Try

import cats.implicits._

import coop.rchain.catscontrib.ski._
import coop.rchain.comm._
import coop.rchain.comm.CachedConnections.ConnectionsCache
import coop.rchain.comm.discovery.KademliaGrpcMonix.KademliaRPCServiceStub
import coop.rchain.grpc.implicits._
import coop.rchain.metrics.Metrics
import coop.rchain.metrics.implicits._
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
  private implicit val metricsSource: Metrics.Source =
    Metrics.Source(CommMetricsSource, "discovery.kademlia.grpc")

  private val cell = connectionsCache(clientChannel)

  def ping(peer: PeerNode): Task[Boolean] =
    for {
      _     <- Metrics[Task].incrementCounter("ping")
      local <- peerNodeAsk.ask
      ping  = Ping().withSender(toNode(local))
      pongErr <- withClient(peer, timeout)(
                  _.sendPing(ping)
                    .timer("ping-time")
                ).attempt
    } yield pongErr.fold(kp(false), kp(true))

  def lookup(key: Seq[Byte], peer: PeerNode): Task[Seq[PeerNode]] =
    for {
      _      <- Metrics[Task].incrementCounter("protocol-lookup-send")
      local  <- peerNodeAsk.ask
      lookup = Lookup().withId(ByteString.copyFrom(key.toArray)).withSender(toNode(local))
      responseErr <- withClient(peer, timeout)(
                      _.sendLookup(lookup)
                        .timer("lookup-time")
                    ).attempt
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

  private def withClient[A](peer: PeerNode, timeout: FiniteDuration, enforce: Boolean = false)(
      f: KademliaRPCServiceStub => Task[A]
  ): Task[A] =
    for {
      channel <- cell.connection(peer, enforce)
      stub    <- Task.delay(KademliaGrpcMonix.stub(channel).withDeadlineAfter(timeout))
      result <- f(stub).doOnFinish {
                 case Some(_) => disconnect(peer)
                 case _       => Task.unit
               }
      _ <- Task.unit.asyncBoundary // return control to caller thread
    } yield result

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
      else shutdownServer >> disconnectFromPeers
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
}
