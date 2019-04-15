package coop.rchain.comm.discovery

import scala.concurrent.duration._

import cats.implicits._

import coop.rchain.catscontrib.ski._
import coop.rchain.comm._
import coop.rchain.comm.discovery.KademliaGrpcMonix.KademliaRPCServiceStub
import coop.rchain.grpc.implicits._
import coop.rchain.metrics.Metrics
import coop.rchain.metrics.implicits._

import com.google.protobuf.ByteString
import io.grpc._
import io.grpc.netty._
import monix.eval._
import monix.execution._

class GrpcKademliaRPC(networkId: String, timeout: FiniteDuration)(
    implicit
    scheduler: Scheduler,
    peerNodeAsk: PeerNodeAsk[Task],
    metrics: Metrics[Task]
) extends KademliaRPC[Task] {

  implicit private val metricsSource: Metrics.Source =
    Metrics.Source(CommMetricsSource, "discovery.kademlia.grpc")

  def ping(peer: PeerNode): Task[Boolean] =
    for {
      _     <- Metrics[Task].incrementCounter("ping")
      local <- peerNodeAsk.ask
      ping  = Ping().withSender(toNode(local)).withNetworkId(networkId)
      pongErr <- withClient(peer, timeout)(
                  _.sendPing(ping)
                    .timer("ping-time")
                ).attempt
    } yield pongErr.fold(kp(false), _.networkId == networkId)

  def lookup(key: Seq[Byte], peer: PeerNode): Task[Seq[PeerNode]] =
    for {
      _     <- Metrics[Task].incrementCounter("protocol-lookup-send")
      local <- peerNodeAsk.ask
      lookup = Lookup()
        .withId(ByteString.copyFrom(key.toArray))
        .withSender(toNode(local))
        .withNetworkId(networkId)
      responseErr <- withClient(peer, timeout)(
                      _.sendLookup(lookup)
                        .timer("lookup-time")
                    ).attempt
    } yield
      responseErr.fold(
        kp(Seq.empty[PeerNode]),
        r => if (r.networkId == networkId) r.nodes.map(toPeerNode) else Seq.empty[PeerNode]
      )

  private def withClient[A](peer: PeerNode, timeout: FiniteDuration, enforce: Boolean = false)(
      f: KademliaRPCServiceStub => Task[A]
  ): Task[A] =
    for {
      channel <- clientChannel(peer)
      stub    <- Task.delay(KademliaGrpcMonix.stub(channel).withDeadlineAfter(timeout))
      result  <- f(stub).doOnFinish(kp(Task.delay(channel.shutdown()).attempt.void))
      _       <- Task.unit.asyncBoundary // return control to caller thread
    } yield result

  private def clientChannel(peer: PeerNode): Task[ManagedChannel] =
    for {
      c <- Task.delay {
            NettyChannelBuilder
              .forAddress(peer.endpoint.host, peer.endpoint.udpPort)
              .executor(scheduler)
              .usePlaintext()
              .build()
          }
    } yield c
}
