package coop.rchain.comm

import cats.effect.{Resource, Sync}
import com.google.protobuf.ByteString
import coop.rchain.metrics.Metrics
import coop.rchain.monix.Monixable
import coop.rchain.sdk.syntax.all._
import io.grpc
import io.grpc.netty.NettyServerBuilder
import monix.execution.Scheduler

package object discovery {
  val DiscoveryMetricsSource: Metrics.Source =
    Metrics.Source(CommMetricsSource, "discovery.kademlia")

  def acquireKademliaRPCServer[F[_]: Monixable: Sync](
      networkId: String,
      port: Int,
      pingHandler: PeerNode => F[Unit],
      lookupHandler: (PeerNode, Array[Byte]) => F[Seq[PeerNode]],
      grpcScheduler: Scheduler
  ): Resource[F, grpc.Server] = {
    val server = NettyServerBuilder
      .forPort(port)
      .executor(grpcScheduler)
      .addService(
        KademliaGrpcMonix
          .bindService(
            new GrpcKademliaRPCServer(networkId, pingHandler, lookupHandler),
            grpcScheduler
          )
      )
      .build

    Resource.make(Sync[F].delay(server.start))(s => Sync[F].delay(s.shutdown.void()))
  }

  def toPeerNode(n: Node): PeerNode =
    PeerNode(NodeIdentifier(n.id.toByteArray), Endpoint(n.host.toStringUtf8, n.tcpPort, n.udpPort))

  def toNode(n: PeerNode): Node =
    Node()
      .withId(ByteString.copyFrom(n.key.toArray))
      .withHost(ByteString.copyFromUtf8(n.endpoint.host))
      .withUdpPort(n.endpoint.udpPort)
      .withTcpPort(n.endpoint.tcpPort)
}
