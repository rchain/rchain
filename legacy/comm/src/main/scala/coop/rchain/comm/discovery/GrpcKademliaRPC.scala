package coop.rchain.comm.discovery

import cats.effect.Sync
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.catscontrib.ski._
import coop.rchain.comm._
import coop.rchain.comm.discovery.KademliaGrpcMonix.KademliaRPCServiceStub
import coop.rchain.comm.rp.Connect.RPConfAsk
import coop.rchain.grpc.implicits._
import coop.rchain.metrics.Metrics
import coop.rchain.metrics.implicits._
import coop.rchain.monix.Monixable
import coop.rchain.shared.syntax._
import io.grpc._
import io.grpc.netty._
import monix.eval.Task
import monix.execution.Scheduler

import scala.concurrent.duration._

class GrpcKademliaRPC[F[_]: Monixable: Sync: RPConfAsk: Metrics](
    networkId: String,
    timeout: FiniteDuration
)(implicit scheduler: Scheduler)
    extends KademliaRPC[F] {

  implicit private val metricsSource: Metrics.Source =
    Metrics.Source(CommMetricsSource, "discovery.kademlia.grpc")

  def ping(peer: PeerNode): F[Boolean] =
    for {
      _       <- Metrics[F].incrementCounter("ping")
      local   <- RPConfAsk[F].ask.map(_.local)
      ping    = Ping().withSender(toNode(local)).withNetworkId(networkId)
      pongErr <- withClient(peer, timeout)(_.sendPing(ping).fromTask.timer("ping-time")).attempt
    } yield pongErr.fold(kp(false), _.networkId == networkId)

  def lookup(key: Seq[Byte], peer: PeerNode): F[Seq[PeerNode]] =
    for {
      _     <- Metrics[F].incrementCounter("protocol-lookup-send")
      local <- RPConfAsk[F].ask.map(_.local)
      lookup = Lookup()
        .withId(ByteString.copyFrom(key.toArray))
        .withSender(toNode(local))
        .withNetworkId(networkId)
      responseErr <- withClient(peer, timeout)(_.sendLookup(lookup).fromTask.timer("lookup-time")).attempt
      peers = responseErr match {
        case Right(r) if r.networkId == networkId =>
          r.nodes.map(toPeerNode)
        case _ => Seq.empty[PeerNode]
      }
    } yield peers

  private def withClient[A](peer: PeerNode, timeout: FiniteDuration, enforce: Boolean = false)(
      f: KademliaRPCServiceStub => F[A]
  ): F[A] =
    for {
      channel <- clientChannel(peer)
      stub    <- Sync[F].delay(KademliaGrpcMonix.stub(channel).withDeadlineAfter(timeout))
      result <- f(stub).toTask
                 .doOnFinish(kp(Task.delay(channel.shutdown()).attempt.void))
                 .fromTask
      _ <- Task.unit.asyncBoundary.fromTask // return control to caller thread
    } yield result

  private def clientChannel(peer: PeerNode): F[ManagedChannel] =
    for {
      c <- Sync[F].delay {
            NettyChannelBuilder
              .forAddress(peer.endpoint.host, peer.endpoint.udpPort)
              .executor(scheduler)
              .usePlaintext()
              .build()
          }
    } yield c
}
