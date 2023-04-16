package coop.rchain.comm.discovery

import cats.effect.kernel.Resource
import cats.effect.std.Dispatcher
import cats.effect.{Async, Sync}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.catscontrib.ski._
import coop.rchain.comm._
import coop.rchain.comm.rp.Connect.RPConfAsk
import coop.rchain.metrics.Metrics
import coop.rchain.metrics.implicits.MetricsSyntaxConversion
import coop.rchain.shared.syntax._
import fs2.grpc.client.ClientOptions
import fs2.grpc.syntax.all._
import io.grpc._
import io.grpc.netty._

import scala.concurrent.duration._

class GrpcKademliaRPC[F[_]: Async: RPConfAsk: Metrics](
    networkId: String,
    timeout: FiniteDuration
) extends KademliaRPC[F] {

  implicit private val metricsSource: Metrics.Source =
    Metrics.Source(CommMetricsSource, "discovery.kademlia.grpc")

  def ping(peer: PeerNode): F[Boolean] =
    for {
      _     <- Metrics[F].incrementCounter("ping")
      local <- RPConfAsk[F].ask.map(_.local)
      ping  = Ping().withSender(toNode(local)).withNetworkId(networkId)
      pongErr <- withClient(peer, timeout)(
                  _.sendPing(ping, new Metadata).timer("ping-time")
                ).attempt
    } yield pongErr.fold(kp(false), _.networkId == networkId)

  def lookup(key: Seq[Byte], peer: PeerNode): F[Seq[PeerNode]] =
    for {
      _     <- Metrics[F].incrementCounter("protocol-lookup-send")
      local <- RPConfAsk[F].ask.map(_.local)
      lookup = Lookup()
        .withId(ByteString.copyFrom(key.toArray))
        .withSender(toNode(local))
        .withNetworkId(networkId)
      responseErr <- withClient(peer, timeout)(
                      _.sendLookup(lookup, new Metadata).timer("lookup-time")
                    ).attempt
      peers = responseErr match {
        case Right(r) if r.networkId == networkId =>
          r.nodes.map(toPeerNode)
        case _ => Seq.empty[PeerNode]
      }
    } yield peers

  private def withClient[A](peer: PeerNode, timeout: FiniteDuration)(
      f: KademliaRPCServiceFs2Grpc[F, Metadata] => F[A]
  ): F[A] = {
    val channelResource = NettyChannelBuilder
      .forAddress(peer.endpoint.host, peer.endpoint.udpPort)
      .usePlaintext()
      .resource[F]

    val co = ClientOptions.default.configureCallOptions(
      _.withDeadlineAfter(timeout.toMillis, MILLISECONDS)
    )

    val clientResource = channelResource.flatMap(x => KademliaRPCServiceFs2Grpc.stubResource(x, co))

    clientResource.use(f)
  }
}
